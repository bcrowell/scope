#!/usr/bin/perl


# This program includes code by zentara, http://www.perlmonks.org/?node_id=583578

use warnings;
use strict;
use Glib qw/TRUE FALSE/;
use Gtk2 -init;
use IO::File;
use Time::HiRes;
use Math::FFT;

# Notes on moving average:
#   Looking at fftexplorer's source code, it looks like it simply averages the complex amplitudes of n frames, and that's what I've done here.
#   But actually that doesn't make much sense. For any given frequency f, there is a known phase relationship between one frame and the next.
#   This should be taken into account, and in fact by the time you've done that, you've really just made the fft frame bigger. So why not just do that?
#   Removed moving average code for this reason.

# The following are from Audio::OSS's Constants.pm, which is constructed by its Makefile.PL by compiling a C program that #includes soundfile.h.
# This works on intel (little-endian). I think these might have to be swapped to work on ARM (which can be either little- or big-endian).
my %dsp_constant = (
    SNDCTL_DSP_RESET => 0x00005000 ,
    SNDCTL_DSP_SYNC => 0x00005001 ,
    SNDCTL_DSP_CHANNELS => 0xc0045006 ,
    SNDCTL_DSP_GETFMTS => 0x8004500b ,
    SNDCTL_DSP_SETFMT => 0xc0045005,
    SNDCTL_DSP_SPEED => 0xc0045002,
    AFMT_S16_BE => 0x00000020 ,
);

# In both time and frequency mode, we read data in chunks of $small_buff_size samples.
# These raw samples are stuck into a circular buffer of $big_buff_size samples.
# In frequency mode:
#   $big_buff_size is also the size of the fft frame.
#   The defaults is $big_buff_size=8192=2^13, which was the default in fftexplorer and seems to work well.

my $small_buff_size_log2 = 9;       # normally 9; if too small, may get bogged down by gtk events
                                    # In my current implementation of time mode, this also affects how long a trace can be.
my $n_small_buffs_log2 = 4;         # normally 4; making this bigger makes fft (logarithmically) slower, but increases frequency resolution

my $small_buff_size = (1<<$small_buff_size_log2);
my $n_small_buffs = (1<<$n_small_buffs_log2);
my $big_buff_size = (1<<($n_small_buffs_log2+$small_buff_size_log2));

my $desired_sampling_rate = 48000; # request most common native rate; this may get changed based on what the sound card actually is willing to set itself to
my $sample_size_bytes = 2; # only 2 works
my $mode = 'f'; # can be 'f' (frequency) or 't' (time)

my $go = 1; # boolean, should we be collecting and displaying data, or not?

my $pixels_per_channel_log2 = -1;
my $f_lo = 0;
my $closeup = 0;
my $save_pixels_per_channel_log2; # for use when exiting closeup
my $save_f_lo; # for use when exiting closeup
my $log_power_scale = 0; # boolean
# avoid sudden changes in y scale in frequency mode:
my $old_y_scale;
my $old_noise;
# -----------------------------------------------------------------------------------------------------------------
#          global data related to GUI
# -----------------------------------------------------------------------------------------------------------------
my @sound_data;
my $spectrum;
my ($window,$area,$pixmap,%allocated_colors,$gc,$colormap);
my @big_buff = (0) x $big_buff_size; # preallocate it for efficiency
my $first_valid_small = 0;
my $n_valid_smalls = 0;

# -----------------------------------------------------------------------------------------------------------------
#          open sound input
# -----------------------------------------------------------------------------------------------------------------
my ($dsp,$error,$sampling_rate);
sub start_sound_input {
  ($dsp,$error,$sampling_rate) = open_sound_input($desired_sampling_rate);
  if ($error) {die "error opening sound input, $error"}
}
start_sound_input();

print "actual sampling rate=$sampling_rate\n";
print "small_buff_size=$small_buff_size   n_small_buffs=$n_small_buffs    big_buff_size=$big_buff_size \n";
print "reads will happen every ",((1./$sampling_rate)*$small_buff_size)," s,",
      " fft frame filled once every ",((1./$sampling_rate)*$small_buff_size)*$n_small_buffs," s",
      "\n";
my $nyquist = $sampling_rate/2;

# -----------------------------------------------------------------------------------------------------------------
#          create, run, and destroy GUI
# -----------------------------------------------------------------------------------------------------------------
gui_setup();
my $event_source_tag;
my $count = 0;
STDOUT->autoflush(1);
my $busy_drawing = 0;
$event_source_tag = Glib::IO->add_watch(
  fileno($dsp),
  'in',
  sub {
    if ($go) {
      my $err = collect_sound();
      if ($err!=0) {die $err}
      if ($mode eq 'f') {
        my $fft = Math::FFT->new(\@big_buff);
        $spectrum = $fft->spctrm(); # has $big_buff_size/2+1 elements
      }
      if (!$busy_drawing) {$busy_drawing=1; draw(); $busy_drawing=0}
    }
    return 1;
  }
);
Gtk2->main;
turn_off_sound_input();
exit(0);
# -----------------------------------------------------------------------------------------------------------------
#           sound-collecting code
# -----------------------------------------------------------------------------------------------------------------
sub turn_off_sound_input {
  Glib::Source->remove($event_source_tag);
  close_sound_input($dsp);
}

sub collect_sound {
  my @data;
  my $buff;
  my $template;
  if ($sample_size_bytes==2) {$template = "n$small_buff_size"}
  if ($sample_size_bytes==4) {$template = "N$small_buff_size"}
  if (!defined $template) {return "illegal sample size, $sample_size_bytes bytes"}
  my $nbytes = $small_buff_size*$sample_size_bytes;
  my $nread = read($dsp,$buff,$nbytes);
  #print "nbytes=$nbytes, nread=$nread\n";
  @sound_data = unpack($template,$buff);
  put_new_data_in_circular_buffer();
  return 0;
}

# -----------------------------------------------------------------------------------------------------------------
#          GUI
# -----------------------------------------------------------------------------------------------------------------
sub draw {
  my $force = 0;
  if (@_) {$force = shift}
  my $t1 = [Time::HiRes::gettimeofday];

  return if $mode eq 'f' && $n_valid_smalls<$n_small_buffs && !$force;

  # get current window size and freeze it, so x y scaling is constant in the pixmap
  my (undef, undef, $width0, $height0, undef) = $window->window->get_geometry;
  $window->set_size_request($width0,$height0);
  $window->set_resizable(0);

  my $colormap = $pixmap->get_colormap;
  my $gc = $pixmap->{gc} || new Gtk2::Gdk::GC $pixmap;
  my ($w,$h);
  (undef, undef, $w, $h, undef) = $area->window->get_geometry;

  # clear the window
  $gc->set_foreground(get_color($colormap, 'white'));
  $pixmap->draw_rectangle($gc,1,0,0,$w,$h); # x,y,w,h

  if ($mode eq 't') {draw_time_mode($pixmap,$colormap,$gc,$w,$h)}
  if ($mode eq 'f') {
    draw_freq_mode($spectrum,$pixmap,$colormap,$gc,$w,$h,$f_lo,$pixels_per_channel_log2);
  }

  #without this line the screen won't be updated until a screen action
  $area->queue_draw; # "Once the main loop becomes idle (after the current batch of events has been processed, roughly), the window will receive expose events for the union of all regions that have been invalidated."

  my $t2 = [Time::HiRes::gettimeofday];
  #print "draw() took ",Time::HiRes::tv_interval($t1,$t2)," s\n";

  # Without the following, we would only redraw when we got an expose event, which could be only when the event queue emptied out.
  $area->window->draw_drawable(
      $area->style->fg_gc( $area->state ), $pixmap,
      0,0,0,0,-1,-1 # x,y (src), x,y (dest), w,h; -1 for w and h means copy whole thing
  );

}

sub draw_freq_mode {
  my ($spectrum,$pixmap,$colormap,$gc,$w,$h,$f_lo,$pixels_per_channel_log2) = @_; #w=width, h=height, in pixels
 
  my $t1 = [Time::HiRes::gettimeofday];

  my $n_spectrum = $big_buff_size/2;

  my $pixels_per_channel = 1;
  if ($pixels_per_channel_log2>0) {$pixels_per_channel = 1<<$pixels_per_channel_log2}
  if ($pixels_per_channel_log2<0) {$pixels_per_channel = 1./(1<<(-$pixels_per_channel_log2))}
  my $f_hi = $f_lo + $nyquist*$w/$big_buff_size/$pixels_per_channel; # one pixel per channel
  my $chan_lo = int(($f_lo/$nyquist)*$big_buff_size);

  my $scale_x = sub {
    my $chan = shift;
    return bit_shift_left(int($chan),$pixels_per_channel_log2);
  };
  my $chan_hi = max_chan_visible($w,$pixels_per_channel_log2,$scale_x,$chan_lo);
  my ($max,$noise,$y_scale) = autoscale_power_spectrum($spectrum,$chan_lo,$f_hi,$big_buff_size,$scale_x,$w,$h,$chan_hi); # returns undef if spectrum is undef

  my $bottom_of_log_scale = sqrt($noise/10.)*$y_scale;
  my $log_bottom_of_log_scale = log($bottom_of_log_scale); # guaranteed not to be <= 0

  my $scale = sub {
    my ($x,$y) = @_;
    my $u = sqrt($y)*$y_scale; # if u=0 to 1, it's in range
    if ($log_power_scale) { # optional logarithmic rescaling
      if ($u>$bottom_of_log_scale) {
        $u = (log($u)-$log_bottom_of_log_scale)/(-$log_bottom_of_log_scale);
      }
      else {
        $u=0;
      }
    }
    return (&$scale_x($x),$h-int($h*$u));
  };

  draw_frequency_scale_and_grid($gc,$pixmap,$f_lo,$f_hi,$scale_x,$w,$h,$big_buff_size,$chan_lo,$chan_hi);
  draw_spectrum($spectrum,$gc,$pixmap,$f_lo,$f_hi,$scale_x,$scale,$w,$h,$big_buff_size,$chan_lo,$chan_hi);

  my $t2 = [Time::HiRes::gettimeofday];
  #print "fft and drawing took ",Time::HiRes::tv_interval($t1,$t2)," s\n";


  $first_valid_small = 0;
  $n_valid_smalls = 0;
}

sub max_chan_visible {
  my ($w,$pixels_per_channel_log2,$scale_x,$chan_lo) = @_;
  my $chan = bit_shift_left(int($w),-$pixels_per_channel_log2)+$chan_lo;
  while (&$scale_x($chan-$chan_lo)<$w) {++$chan}
  while (&$scale_x($chan-$chan_lo)>$w && $chan>$chan_lo) {--$chan}
  return $chan;
}

sub autoscale_power_spectrum {
  my ($spectrum,$chan_lo,$f_hi,$big_buff_size,$scale_x,$w,$h,$chan_hi)  = @_;
  return if !defined $spectrum;
  # find maximum:
  my $max = 0;
  my $max_k;
  my $lo_for_max = $chan_lo;
  # Look for the maximum value, with a high-pass filter because we tend to get a big noise peak at DC.
  for (my $k=$lo_for_max; $k<$chan_hi; $k++) {
    if ($spectrum->[$k]*$k>$max) {$max=$spectrum->[$k]*$k; $max_k=$k}
  }
  $max = $max/$max_k;
  # Find approximate median (only using a random sample of channels, since the sort is a slow n log n operation).
  # It's referred to as median in code and comments, but actually I use the 25th percentile.
  # The median is typically somewhere in the middle of the noise.
  my @random_sample = ();
  my $step = 7; # pick an odd number to avoid possible special behavior of powers of 2
  while (($chan_hi-$chan_lo)/$step<100 && $step>1) { # but choose a smaller step if it would result in a sample size of less than 100
    $step = ($step+1)/2-1;
  }
  for (my $k=$chan_lo; $k<$chan_hi; $k+=7) {
    push @random_sample,[$k,$spectrum->[$k]];
  }
  @random_sample = sort {$a->[1]*$a->[0] <=> $b->[1]*$b->[0]} @random_sample; # multiplication is high-pass filter
  my $median = $random_sample[int(@random_sample/4)]->[1]; # labeled as the median, but is really the 25th percentile
  # Never make the max go off the screen. But when the input is quiet, we don't want to scale it up so much that the noise looks gigantic. If the input
  # consists of sound interspersed with silence, we don't want crazy, abrupt changes in scale. The median, which is a measure of background noise,
  # should stay fairly constant. The median can also be useful in setting the bottom of a log scale. We're doing our computations in this subroutine using
  # the power spectrum, but the scale factor will be applied to the square root of the power spectrum, which is what's actually displayed.
  # The output $y_scale is supposed to be such that when it's multiplied by sqrt(power), the result is between 0 and 1 for the visible portion of the graph.
  my $y_scale = 1./(100.*sqrt($median+1.)); # make the noise about 1/400 of the full scale; for typical s/n ratios, this makes noise visible across the board
  my $overload = sqrt($max)*$y_scale; # If this is >1, then the biggest peak is too high
  if ($overload>1.) {$y_scale /= $overload}
  if ($median<=1.) {$median=1.} # we take log of it, so don't let it be zero

  # change scale smoothly, not abruptly:
  if (defined $old_y_scale) { 
    my $decay=.25;
    $y_scale = (1.-$decay)*$old_y_scale+$decay*$y_scale;
    $median  = (1.-$decay)*$old_noise  +$decay*$median;
  } 
  $old_y_scale = $y_scale;
  $old_noise   = $median;

  return ($max,$median,$y_scale);
}

sub draw_spectrum {
  my ($spectrum,$gc,$pixmap,$f_lo,$f_hi,$scale_x,$scale,$w,$h,$big_buff_size,$chan_lo,$chan_hi) = @_;
  return if !defined $spectrum;
  $gc->set_foreground(get_color($colormap, 'black'));
  my ($prev_x,$prev_y) = &$scale(0,0);
  for (my $k=$chan_lo; $k<$chan_hi; $k++) {
    my $power = $spectrum->[$k];
    my ($x,$y) = &$scale($k-$chan_lo,$power);
    if ($k>$chan_lo) {
      $pixmap->draw_rectangle($gc,1,$prev_x,$prev_y,($x-$prev_x),($h-$prev_y));
    }
    ($prev_x,$prev_y) =($x,$y);
    #++$k; # fixme !?!?!?!!!!!!!!!!!!!!!!!!!!!!!!!!!!! incs twice
  }
}

sub draw_frequency_scale_and_grid {
  my ($gc,$pixmap,$f_lo,$f_hi,$scale_x,$w,$h,$big_buff_size,$chan_lo,$chan_hi) = @_;
  $gc->set_foreground(get_color($colormap, 'gray'));
  my $scale_factor = &$scale_x(1000)/1000.;
  my $freq_range = $f_hi-$f_lo;
  my $grid_interval = optimal_grid_spacing($freq_range);
  my $layout = make_text_layout();
  my $f1 = (int($f_lo/$grid_interval)+1)*$grid_interval;
  #print "f_lo=$f_lo f_hi=$f_hi f1=$f1 grid_interval=$grid_interval nyquist=$nyquist\n";
  for (my $f=$f1; $f<$f_hi; $f+=$grid_interval) {
    my $k = ($f/$nyquist)*$big_buff_size;
    if ($k>$chan_lo) {
      my $x = &$scale_x($k-$chan_lo);
      last if $x>$w;
      $pixmap->draw_line($gc,$x,0,$x,$h);
      $layout->set_text ($f);
      $pixmap->draw_layout($gc,$x-15,30,$layout);
      $layout->set_text ('Hz');
      $pixmap->draw_layout($gc,$x-15,45,$layout);
    }
  }
}

sub make_text_layout {
  my $surface = Cairo::ImageSurface->create ('argb32', 300, 200); # http://cairographics.org/documentation/cairomm/reference/namespaceCairo.html#d3f86970e1bd354b263303c9b8759166
  my $cr = Cairo::Context->create ($surface);
  my $layout = Gtk2::Pango::Cairo::create_layout ($cr);
  return $layout;
}

sub optimal_grid_spacing {
  my $max = shift;
  my $z = log($max)/log(10.);
  my $zf = $z-int($z);
  my $q = exp($zf*log(10.)); # e.g., if top freq displayed is 7000 Hz, q=7
  my $best = 999.;
  my $best_r = 1;
  my $optimal_n_lines = 30;
  my $root_10 = sqrt(10.);
  foreach my $r(1,2,5) { # r=spacing of grid
    my $n = $q/$r; # how many divisions would we have with this spacing?
    while ($n<$optimal_n_lines/$root_10) {$n*=10}
    while ($n>$optimal_n_lines*$root_10) {$n/=10}
    my $badness = abs(log($n/10.));
    if ($badness<$best) {$best_r=$r; $best=$badness}
  }
  my $grid_interval = $best_r;
  while ($max/$grid_interval < $optimal_n_lines/$root_10) {$grid_interval /= 10.}
  while ($max/$grid_interval > $optimal_n_lines*$root_10) {$grid_interval *= 10.}
  return $grid_interval;
}

sub draw_time_mode {
  my ($pixmap,$colormap,$gc,$w,$h) = @_; #w=width, h=height

  my $i1 = $first_valid_small*$small_buff_size;
  my $n_samples = $n_valid_smalls*$small_buff_size;
  my $i2 = ($i1 + $n_samples-1)%$big_buff_size;

  my $half_height = int($h/2);

  $gc->set_foreground(get_color($colormap, 'black'));
  $pixmap->draw_line($gc,0,$half_height,$w,$half_height);

  my $scale = sub {
    use integer;
    my ($x,$y) = @_;
    return ($x<<1,$half_height-($y>>3));
  };

  my $dc_filter_window_log2 = 4;
  my $dc_filter_window = 1<<$dc_filter_window_log2;
  my $dc_level = 0;
  for (my $i=$i1; $i<$i1+$dc_filter_window; $i++) {
    $dc_level += $big_buff[$i];
  }

  my $i = $i1;
  my $j = ($i1-$dc_filter_window)%$big_buff_size;
  my ($prev_x,$prev_y) = &$scale(0,$big_buff[$i1]-$dc_level);
  for (my $k=0 ; $k<$n_samples; $k++) {
    my $sample = $big_buff[$i];
    if ($k>= $dc_filter_window) {
      $dc_level += ($sample-$big_buff[$j]);
    }
    my ($x,$y) = &$scale($k,$sample-($dc_level>>$dc_filter_window_log2));
    # draw as a staircase (presumably vertical and horizontal lines are implemented more efficiently)
    if ($k>0) {
      $pixmap->draw_line($gc, $prev_x,$prev_y, $x,$prev_y);
      $pixmap->draw_line($gc, $x,$prev_y, $x,$y);
      last if $x>$w;
    }
    ($prev_x,$prev_y) = ($x,$y);
    $i++;
    $j++;
    if ($i>=$big_buff_size) {$i -= $big_buff_size}
    if ($j>=$big_buff_size) {$j -= $big_buff_size}
  }
  $first_valid_small = ($first_valid_small+$n_valid_smalls)%$n_small_buffs;
  $n_valid_smalls = 0;

}

sub put_new_data_in_circular_buffer {
  # Splice the newly collected small buffer into the large circular buffer.
  my $k=($first_valid_small+$n_valid_smalls)%$n_small_buffs;
  splice @big_buff,$k*$small_buff_size,$small_buff_size,@sound_data;
  ++$n_valid_smalls;
  if ($n_valid_smalls>$n_small_buffs) { # circular buffer has overflowed
    $n_valid_smalls = $n_small_buffs;
    $first_valid_small = ($first_valid_small+1)%$n_small_buffs
  }
}

sub get_color {
    my ($colormap, $name) = @_;
    my $ret;

    if ($ret = $allocated_colors{$name}) {
        return $ret;
    }

    my $color = Gtk2::Gdk::Color->parse($name);
    $colormap->alloc_color($color,TRUE,TRUE);

    $allocated_colors{$name} = $color;

    return $color;
}

# Create a new backing pixmap of the appropriate size
sub configure_event {
   my $widget = shift;    # GtkWidget         *widget
   my $event  = shift;    # GdkEventConfigure *event

   $pixmap = Gtk2::Gdk::Pixmap->new(
      $widget->window,
      $widget->allocation->width,
      $widget->allocation->height, -1
   );

   $pixmap->draw_rectangle(
      $widget->style->white_gc,    # or black_gc
      TRUE,
      0, 0,
      $widget->allocation->width,
      $widget->allocation->height
   );

   $gc       = Gtk2::Gdk::GC->new( $pixmap );
   $colormap = $pixmap->get_colormap;

   return TRUE;
}

# Redraw the screen from the backing pixmap
sub expose_event {
   my $widget = shift;    # GtkWidget      *widget
   my $event  = shift;    # GdkEventExpose *event

   $widget->window->draw_drawable(
      $widget->style->fg_gc( $widget->state ), $pixmap,
      $event->area->x,                         $event->area->y,
      $event->area->x,                         $event->area->y,
      $event->area->width,                     $event->area->height
   );

   return FALSE;
}

# -----------------------------------------------------------------------------------------------------------------
#           low-level sound stuff
# -----------------------------------------------------------------------------------------------------------------
sub close_sound_input {
  my $dsp = shift;
  $dsp->close;
}

sub open_sound_input {
  no strict 'subs';

  my $sampling_rate_desired = 48000; # default
  if (@_) {$sampling_rate_desired = shift}

  my $dsp = new IO::File("</dev/dsp") or return(undef,"open failed: $!");

  dsp_reset($dsp) or return(undef,"reset failed: $!");

  #--- format

  my $mask = get_supported_fmts($dsp);
  my $format_desired;
  $format_desired = $dsp_constant{AFMT_S16_BE}; # 16 bits, NE=native, LE=little-endian, BE=big-endian
  if ($mask & $format_desired) {
    set_fmt($dsp, $format_desired) or return(undef,"set format failed: $!");
  }
  else {
    return(undef,"desired format not available in set_fmt");
  }

  #--- sampling rate

  my $sampling_rate = set_sps($dsp, $sampling_rate_desired); # May change the global variable by, e.g., 10 %. This is normal http://manuals.opensound.com/developer/SNDCTL_DSP_SPEED.html

  #--- mono
     
  my $channels = pack "L",1;
  my $was = $channels;
  ioctl $dsp, $dsp_constant{SNDCTL_DSP_CHANNELS}, $channels or return(undef,"error setting mono, $!");

  return ($dsp,0,$sampling_rate);

}

sub dsp_reset {
    my $dsp = shift;
    ioctl $dsp, $dsp_constant{SNDCTL_DSP_SYNC}, 0 or return undef;
    ioctl $dsp, $dsp_constant{SNDCTL_DSP_RESET}, 0;
}

sub get_supported_fmts {
  my ($fh,$in) = @_;
  do_ioctl($fh,$in,$dsp_constant{SNDCTL_DSP_GETFMTS});
}

sub set_fmt {
  my ($fh,$in) = @_;
  do_ioctl($fh,$in,$dsp_constant{SNDCTL_DSP_SETFMT});
}

sub set_sps {
  my ($fh,$in) = @_;
  do_ioctl($fh,$in,$dsp_constant{SNDCTL_DSP_SPEED});
}

sub do_ioctl {
  my $fh = shift;
  my $in = shift || 0;
  my $ioctl = shift;
  my $out = pack "L", $in;
  ioctl($fh, $ioctl, $out) or return undef;
  return unpack "L", $out;
}
# -----------------------------------------------------------------------------------------------------------------
#          GUI setup
# -----------------------------------------------------------------------------------------------------------------
sub gui_setup {

$pixmap     = undef;
$gc         = undef;
$colormap   = undef;
 
my %allocated_colors;
my ($x0,$y0,$x1,$y1,$width,) = (0,0,0,0);

# Create the window
$window = new Gtk2::Window ( "toplevel" );
$window->signal_connect ("delete_event", sub { turn_off_sound_input(); Gtk2->main_quit; });
$window->set_border_width (10);
$window->set_size_request(1024+28,480); # oscilloscope area comes out 1024 pixels wide on my system
$window->set_position('center');

my $vbox = Gtk2::VBox->new( 0, 0 );
$window->add($vbox);
$vbox->set_border_width(2);

my $hbox = Gtk2::HBox->new( 0, 0 );
$vbox->pack_start($hbox,1,1,0);
$hbox->set_border_width(2);

my $hbox1 = Gtk2::HBox->new( 0, 0 );
$vbox->pack_start($hbox1,0,0,0);
$hbox1->set_border_width(2);

my @buttons = (
  ['Freeze/Go', sub {
    $go = !$go;
    if (!$go) {
      close_sound_input($dsp);
    }
    else {
      start_sound_input();
    }
  }],
  ['Frequency/Time' , sub {
    if ($mode eq 'f') {$mode = 't'; $old_y_scale = undef; $old_noise=undef} else {$mode = 'f'}
  }],
  ['Zoom In' , sub {
    if ($pixels_per_channel_log2<6) {++$pixels_per_channel_log2; draw(1)}
  }],
  ['Zoom Out' , sub {
    if ($pixels_per_channel_log2>-3) {--$pixels_per_channel_log2; draw(1)}
  }],
  ['Log/Linear Y' , sub {
    if ($mode eq 'f') {$log_power_scale=!$log_power_scale; draw(1)}
  }],
);

foreach my $b(@buttons) {
  my ($label,$sub) = @$b;
  my $button = Gtk2::Button->new($label);
  $hbox1->pack_start($button, FALSE, FALSE, 2);
  $button->signal_connect(clicked => $sub);
}

# Create the drawing area.
$area = new Gtk2::DrawingArea; #don't confuse with Gtk2::Drawable
$hbox->pack_start($area,1,1,0);
$area->set_events ([qw/exposure-mask
         	       leave-notify-mask
		       button-press-mask
		       pointer-motion-mask
		       pointer-motion-hint-mask/]);

$area->signal_connect (button_press_event => sub {
  my $drawing_area = shift;
  my $event = shift; # Gtk2::Gdk::Event::Button
  my $x = $event->x;
  $closeup = !$closeup;
  if ($closeup) {
    my (undef, undef, $w, undef, undef) = $drawing_area->window->get_geometry;
    my $chan = bit_shift_left(int($x),-$pixels_per_channel_log2); # how many channels above f_lo did they click?
    $save_f_lo = $f_lo;
    my $f_center = $f_lo + $chan*$nyquist/$big_buff_size;
    $save_pixels_per_channel_log2 = $pixels_per_channel_log2;
    $pixels_per_channel_log2 = 4;
    $f_lo = $f_center - .5*$nyquist*($w/(1<<$pixels_per_channel_log2))/$big_buff_size;    
  }
  else {
    $pixels_per_channel_log2 = $save_pixels_per_channel_log2;
    $f_lo = $save_f_lo;
  }
  draw(1);
});
# Signals used to handle backing pixmap
$area->signal_connect( expose_event    => \&expose_event );
$area->signal_connect( configure_event => \&configure_event );


$window->show_all;
}

# -----------------------------------------------------------------------------------------------------------------
#          misc
# -----------------------------------------------------------------------------------------------------------------
sub bit_shift_left {
  my $x = shift;
  my $n = shift;
  if ($n>0) {return $x<<$n} else {return $x>>(-$n)}
}
