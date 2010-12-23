#!/usr/bin/perl


# This program includes code by zentara, http://www.perlmonks.org/?node_id=583578

use warnings;
use strict;
use Glib qw/TRUE FALSE/;
use Gtk2 -init;
use IO::File;
use Time::HiRes;
use Math::FFT;

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
#   We can average the complex amplitudes over $moving_average_length frames.
#   The defaults are $big_buff_size=8192, $moving_average_length=64. 
#   In fftexplorer, these are the defaults (if averaging is turned on).

my $small_buff_size_log2 = 9;       # normally 9; if too small, may get bogged down by gtk events
                                    # In my current implementation of time mode, this also affects how long a trace can be.
my $n_small_buffs_log2 = 4;         # normally 4; making this bigger makes fft (logarithmically) slower, but increases frequency resolution
                                    # making this >=4 causes long delays between callbacks to draw()??
my $moving_average_length_log2 = 0; # normally 6

my $small_buff_size = (1<<$small_buff_size_log2);
my $n_small_buffs = (1<<$n_small_buffs_log2);
my $big_buff_size = (1<<($n_small_buffs_log2+$small_buff_size_log2));
my $moving_average_length = (1<<$moving_average_length_log2);

my $desired_sampling_rate = 48000; # request most common native rate; this may get changed based on what the sound card actually is willing to set itself to
my $sample_size_bytes = 2; # only 2 works
my $mode = 'f'; # can be 'f' (frequency) or 't' (time)


# -----------------------------------------------------------------------------------------------------------------
#          data shared between threads
# -----------------------------------------------------------------------------------------------------------------
my @sound_data;

# -----------------------------------------------------------------------------------------------------------------
#          global data related to GUI
# -----------------------------------------------------------------------------------------------------------------
my ($window,$area,$pixmap,%allocated_colors,$gc,$colormap);
my @big_buff = (0) x $big_buff_size; # preallocate it for efficiency
my $first_valid_small = 0;
my $n_valid_smalls = 0;
my @fft_frames = (); # array of refs to fft's (complex amplitudes); a FIFO buffer of length $moving_average_length
my @fft_running_sum = (0) x ($big_buff_size); # the current sum of the spectra in fft_frames

# -----------------------------------------------------------------------------------------------------------------
#          open sound input
# -----------------------------------------------------------------------------------------------------------------
my ($dsp,$error,$sampling_rate) = open_sound_input($desired_sampling_rate);
print "actual sampling rate=$sampling_rate, dsp fileno=",fileno($dsp),"\n";
if ($error) {die "error opening sound input, $error"}

print "small_buff_size=$small_buff_size   n_small_buffs=$n_small_buffs    big_buff_size=$big_buff_size    moving_average_length=$moving_average_length\n";
print "reads will happen every ",((1./$sampling_rate)*$small_buff_size)," s,",
      " fft frame filled once every ",((1./$sampling_rate)*$small_buff_size)*$n_small_buffs," s",
      " time to refresh moving average is ",((1./$sampling_rate)*$small_buff_size)*$n_small_buffs*$moving_average_length," s",
      "\n";

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
    my $err = collect_sound();
    if ($err!=0) {die $err}
    if (!$busy_drawing) {$busy_drawing=1; draw(); $busy_drawing=0}
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
  my $t1 = [Time::HiRes::gettimeofday];

  return if $mode eq 'f' && $n_valid_smalls<$n_small_buffs;

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

  if ($mode eq 't') {draw_time_mode($colormap,$gc,$w,$h)}
  if ($mode eq 'f') {draw_freq_mode($colormap,$gc,$w,$h)}

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
  my ($colormap,$gc,$w,$h) = @_; #w=width, h=height
 
  my $t1 = [Time::HiRes::gettimeofday];

  my $n_spectrum = $big_buff_size/2;

  my $spectrum;
  my $fft = Math::FFT->new(\@big_buff);
  if ($moving_average_length>1) {
    my $ampl = $fft->rdft(); # complex amplitudes
    # FIXME: For efficiency, the following should only be applied to data in the range of frequencies that are actually being displayed.
    # FIXME: Since this is all floating point, rounding errors will accumulate.
    print "averaging ",(0+@fft_frames)," frames\n";
    if (@fft_frames>=$moving_average_length) {
      my $old = $fft_frames[0];
      for (my $i=0; $i<$big_buff_size; $i++) {
        $fft_running_sum[$i] += ($ampl->[$i]-$old->[$i]);
      }
      shift @fft_frames; # I think by reference counting this should free the memory.
    }
    else {
      for (my $i=0; $i<$big_buff_size; $i++) {
        $fft_running_sum[$i] += $ampl->[$i];
      }
    }
    push @fft_frames,$ampl;
    $spectrum = [];
    my $j = 0;
    for (my $i=0; $i<$n_spectrum; $i++,$j+=2) {
      my ($real,$imag) = ($fft_running_sum[$j],$fft_running_sum[$j+1]);
      $spectrum->[$i] = $real*$real+$imag*$imag;
    }    
  }
  else {
    $spectrum = $fft->spctrm(); # has $big_buff_size/2+1 elements, but don't count on the final one to exist, because it won't if we're doing averaging
  }

  my $max_power = 0;
  my $max_is_at = 0;
  for (my $i=$n_spectrum/8; $i<$n_spectrum*3/4; $i++) {
    my $power = $spectrum->[$i];
    if ($power > $max_power) {$max_power = $power; $max_is_at = $i}
  }
  if ($max_power==0) {$max_power=1.} # prevent div by zero later
  print "max is at $max_is_at\n";

  my $x_shift = 1;
  while (($big_buff_size<<$x_shift)<$w/2) {++$x_shift}

  my $log_scale = sub {
    my ($x,$y) = @_;
    my $max_e_folds = 16;
    my $u = -$max_e_folds;
    if ($y/$max_power>0) {$u = log($y/$max_power)}
    if ($u<-8) {$u= -$max_e_folds}
    return (int($x)<<$x_shift,$h-int($h*$u/$max_e_folds));
  };
  my $scale = sub {
    my ($x,$y) = @_;
    return (int($x)<<$x_shift,$h-int($h*$y/$max_power*.5));
  };

  $gc->set_foreground(get_color($colormap, 'black'));

  my ($prev_x,$prev_y) = &$scale(0,0);
  my $k=0;
  foreach my $power(@$spectrum) {
    my ($x,$y) = &$scale($k,$power);
    if ($k>2) {
      $pixmap->draw_rectangle($gc,1,$prev_x,$prev_y,($x-$prev_x),($h-$prev_y));
    }
    last if $x>$w;
    ($prev_x,$prev_y) =($x,$y);
    ++$k;
  }

  my $t2 = [Time::HiRes::gettimeofday];
  #print "fft and drawing took ",Time::HiRes::tv_interval($t1,$t2)," s\n";


  $first_valid_small = 0;
  $n_valid_smalls = 0;
}

sub draw_time_mode {
  my ($colormap,$gc,$w,$h) = @_; #w=width, h=height

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

my $button1 = Gtk2::Button->new('Draw');
$hbox1->pack_start( $button1, FALSE, FALSE, 2);
$button1->signal_connect( clicked => \&draw);

# Create the drawing area.
$area = new Gtk2::DrawingArea; #don't confuse with Gtk2::Drawable
$hbox->pack_start($area,1,1,0);

$area->set_events ([qw/exposure-mask
         	       leave-notify-mask
		       button-press-mask
		       pointer-motion-mask
		       pointer-motion-hint-mask/]);

$area->signal_connect (button_press_event => \&button_press_event);
# Signals used to handle backing pixmap
$area->signal_connect( expose_event    => \&expose_event );
$area->signal_connect( configure_event => \&configure_event );


$window->show_all;
}
# -----------------------------------------------------------------------------------------------------------------
#          misc
# -----------------------------------------------------------------------------------------------------------------
sub warning {
  my $message = shift;
  print STDERR $message,"\n";
}
