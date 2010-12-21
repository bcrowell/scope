#!/usr/bin/perl


# This program includes code by zentara, http://www.perlmonks.org/?node_id=583578

use warnings;
use strict;
use threads;
use threads::shared;
use Glib qw/TRUE FALSE/;
use Gtk2 -init;
use IO::File;
use Audio::OSS qw(:formats);
use Time::HiRes;

# -----------------------------------------------------------------------------------------------------------------
#          data shared between threads
# -----------------------------------------------------------------------------------------------------------------
my ($time_to_die,$collect_sound_error,$fresh_data,@sound_data) :shared;
$time_to_die = 0;
$fresh_data = 0;

# -----------------------------------------------------------------------------------------------------------------
#          create sound-reading thread
# -----------------------------------------------------------------------------------------------------------------
my $collect_sound_thread;
$collect_sound_thread = threads->create(\&collect_sound) or die "error creating thread, $!";

# -----------------------------------------------------------------------------------------------------------------
#          global data related to GUI
# -----------------------------------------------------------------------------------------------------------------
my ($window,$area,$pixmap,%allocated_colors,$gc,$colormap);
# -----------------------------------------------------------------------------------------------------------------
#          create, run, and destroy GUI
# -----------------------------------------------------------------------------------------------------------------
my @copy_of_sound_data;
gui_setup();
# The following could possibly be done more efficiently by using g_io_add_watch_full().
my $event_source_tag = Glib::Idle->add(
  sub {
    if ($fresh_data) {
      $fresh_data = 0;
      draw();
    }
    return 1; # don't automatically remove this callback
  }
); 
Gtk2->main;
Glib::Source->remove($event_source_tag);
$collect_sound_thread->join();
if ($collect_sound_error) {die $collect_sound_error}
exit(0);
# -----------------------------------------------------------------------------------------------------------------
#           sound-collecting code
# -----------------------------------------------------------------------------------------------------------------
sub collect_sound {
  my @data;
  my $buff;
  my ($dsp,$error) = open_sound_input();
  if ($error) {$collect_sound_error = $error; return undef}
  while(read($dsp,$buff,1024) && !$time_to_die) {
    {
      lock(@sound_data);
      @sound_data = unpack("n1024",$buff);
      $fresh_data = 1;
    }
  }
  close_sound_input($dsp);
  return 1;
}

# -----------------------------------------------------------------------------------------------------------------
#          GUI
# -----------------------------------------------------------------------------------------------------------------
sub draw {
  {
    lock(@sound_data);
    @copy_of_sound_data = @sound_data;
  }
 
  # get current window size and freeze it, so x y scaling is constant in the pixmap
  my (undef, undef, $width0, $height0, undef) = $window->window->get_geometry;
  $window->set_size_request($width0,$height0);
  $window->set_resizable(0);

  my ($w,$h); # width and height
  (undef, undef, $w, $h, undef) = $area->window->get_geometry;

  my $colormap = $pixmap->get_colormap;
  my $gc = $pixmap->{gc} || new Gtk2::Gdk::GC $pixmap;

  $gc->set_foreground(get_color($colormap, 'white'));
  $pixmap->draw_rectangle($gc,1,0,0,$w,$h); # x,y,w,h
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
  for (my $i=0; $i<$dc_filter_window; $i++) {
    $dc_level += $copy_of_sound_data[0];
  }

  my $i = 0;
  my ($prev_x,$prev_y) = &$scale(0,$copy_of_sound_data[0]-$dc_level);
  foreach my $sample(@copy_of_sound_data) {
    my $j = $i-1-$dc_filter_window;
    if ($j>=0) {
      $dc_level += ($copy_of_sound_data[$j+$dc_filter_window]-$copy_of_sound_data[$j]);
    }
    my ($x,$y) = &$scale($i,$sample-($dc_level>>$dc_filter_window_log2));
    # draw as a staircase (presumably vertical and horizontal lines are implemented more efficiently)
    if ($i>0) {
      $pixmap->draw_line($gc, $prev_x,$prev_y, $x,$prev_y);
      $pixmap->draw_line($gc, $x,$prev_y, $x,$y);
      #$pixmap->draw_line($gc, $prev_x,$half_height, $x,$half_height);
    }
    ($prev_x,$prev_y) = ($x,$y);
    $i++;
  }

  #without this line the screen won't be updated until a screen action
  $area->queue_draw;                                                      
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

# set a default foreground
   $gc->set_foreground( get_color( $colormap, 'red' ) );

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

  my $sampling_rate_desired = 48000;
  if (@_) {$sampling_rate_desired = shift}

  my $dsp = new IO::File("</dev/dsp") or return(undef,"open failed: $!");

  Audio::OSS::dsp_reset($dsp) or return(undef,"reset failed: $!");

  #--- format

  my $mask = Audio::OSS::get_supported_fmts($dsp);
  my $format_desired = AFMT_S16_BE; # 16 bits, NE=native, LE=little-endian, BE=big-endian
  if ($mask & $format_desired) {
    Audio::OSS::set_fmt($dsp, $format_desired) or return(undef,"set format failed: $!");
  }
  else {
    return(undef,"desired format not available in set_fmt");
  }

  #--- sampling rate

  my $sampling_rate_actual = Audio::OSS::set_sps($dsp, $sampling_rate_desired);
  $sampling_rate_actual eq $sampling_rate_desired or return(undef,"sampling rate set to $sampling_rate_actual rather than $sampling_rate_desired");

 #--- mono
     
  my $channels = pack "L",1;
  my $was = $channels;
  # The hex below is SNDCTL_DSP_CHANNELS.
  ioctl $dsp, 0xc0045006, $channels or return(undef,"error setting mono, $!");
  #$was eq $channels or return(undef,"error setting mono, result not as requested");

  return ($dsp,0);

}
# -----------------------------------------------------------------------------------------------------------------
#          GUI setup
# -----------------------------------------------------------------------------------------------------------------
# gtk2 pixmaps (on linux ?) have a current limit 
# of short unsigned INT , highest pixels is 
# 32767 is (8bit int max) -1     
sub gui_setup {
my $xsize = 2400; # maxsize = 32767
my $ysize = 100;

$pixmap     = undef;
$gc         = undef;
$colormap   = undef;
 
my %allocated_colors;
my ($x0,$y0,$x1,$y1,$width,) = (0,0,0,0);

# Create the window
$window = new Gtk2::Window ( "toplevel" );
$window->signal_connect ("delete_event", sub { $time_to_die = 1; Gtk2->main_quit; });
$window->set_border_width (10);
$window->set_size_request(640,480);
$window->set_position('center');

my $vbox = Gtk2::VBox->new( 0, 0 );
$window->add($vbox);
$vbox->set_border_width(2);

my $hbox = Gtk2::HBox->new( 0, 0 );
$vbox->pack_start($hbox,1,1,0);
$hbox->set_size_request(320,240);
$hbox->set_border_width(2);

my $hbox1 = Gtk2::HBox->new( 0, 0 );
$vbox->pack_start($hbox1,0,0,0);
$hbox1->set_border_width(2);

my $button1 = Gtk2::Button->new('Draw');
$hbox1->pack_start( $button1, FALSE, FALSE, 2);
$button1->signal_connect( clicked => \&draw);

# Create the drawing area.
$area = new Gtk2::DrawingArea; #don't confuse with Gtk2::Drawable
$area->size ($xsize, $ysize);
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

sub wait_for_flag {
  my $who = shift;
  my $name_of_flag = shift;
  my $flag_ref = shift;
  my $desired = shift; # 0 or 1
  my $max_sleep = shift; # milliseconds

  my $ms_slept = 0;
  my $this_sleep = 1; # milliseconds
  while ($$flag_ref != $desired) {
    Time::HiRes::usleep(1000*$this_sleep); # microseconds=1000*milliseconds
    $ms_slept += $this_sleep;
    if ($this_sleep<100) {$this_sleep = $this_sleep * 2}
    if ($ms_slept>$max_sleep) {
      warning("in $who, $name_of_flag was set for $ms_slept milliseconds, which is greater than $max_sleep");
      return 0;
    }
  }
  return 1;
}
