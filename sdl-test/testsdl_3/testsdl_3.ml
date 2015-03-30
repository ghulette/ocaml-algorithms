open Sdlevent
open Sdlkey

let img_filename = "camel500.jpg"

let rec wait_for_esc () =
  match wait_event () with
  | KEYDOWN {keysym = KEY_ESCAPE} -> 
     print_endline "Escaped!"
  | event -> 
     print_endline (string_of_event event);
     wait_for_esc ()

let run () =
  let screen = Sdlvideo.set_video_mode 500 300 [`DOUBLEBUF] in
  let image = Sdlloader.load_image img_filename in
  let img_pos = Sdlvideo.rect 0 0 500 300 in
  Sdlvideo.blit_surface ~dst_rect:img_pos ~src:image ~dst:screen ();
  Sdlvideo.flip screen;
  wait_for_esc ()

let main () =
  Sdl.init [`VIDEO];
  at_exit Sdl.quit;
  run ()

let _ = main ()
