let main () =
  Sdl.init [`VIDEO];
  Sdlvideo.set_video_mode 320 200 [] |> ignore;
  Sdltimer.delay 2000;
  Sdl.quit ()

let _ = main ()
