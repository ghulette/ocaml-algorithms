let img_filename = "camel500.jpg"

let run () =
  let screen = Sdlvideo.set_video_mode 400 400 [`DOUBLEBUF] in
  let image = Sdlloader.load_image img_filename in
  let img_pos = Sdlvideo.rect 0 0 300 300 in
  Sdlvideo.blit_surface ~dst_rect:img_pos ~src:image ~dst:screen ();
  Sdlvideo.flip screen;
  Sdltimer.delay 2000

let main () =
  Sdl.init [`VIDEO];
  at_exit Sdl.quit;
  run ()

let _ = main ()
