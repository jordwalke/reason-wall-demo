open Tsdl;

open Wall;

module Text = Wall_text;

let w = 1000;

let h = 600;

let f =
  try (float_of_string(Sys.argv[1])) {
  | _ => 1.0
  };

let fw = int_of_float(f *. float(w));

let fh = int_of_float(f *. float(h));

let lw = float(w);

let lh = float(h);

let pw = lw *. f;

let ph = lh *. f;

let render = (vg, sw, sh, _t) => {
  let width = lw *. f *. sw;
  let height = lh *. f *. sh;
  Renderer.render(
    vg,
    ~width,
    ~height,
    Image.transform(Transform.scale(sw *. f, sh *. f), Blender.draw),
  );
};

open Tgles2;

let main = () => {
  Printexc.record_backtrace(true);
  switch (Sdl.init(Sdl.Init.video)) {
  | Error(`Msg(e)) =>
    Sdl.log("Init error: %s", e);
    exit(1);
  | Ok () =>
    switch (
      Sdl.create_window(
        ~w=fw,
        ~h=fh,
        "SDL OpenGL",
        Sdl.Window.(opengl + allow_highdpi),
      )
    ) {
    | Error(`Msg(e)) =>
      Sdl.log("Create window error: %s", e);
      exit(1);
    | Ok(w) =>
      /*Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;*/
      /*Sdl.gl_set_attribute Sdl.Gl.context_major_version 2;*/
      /*Sdl.gl_set_attribute Sdl.Gl.context_minor_version 1;*/
      ignore(Sdl.gl_set_swap_interval(-1));
      let (ow, oh) = Sdl.gl_get_drawable_size(w);
      Sdl.log(
        "window size: %d,%d\topengl drawable size: %d,%d",
        fw,
        fh,
        ow,
        oh,
      );
      let sw = float(ow) /. float(fw)
      and sh = float(oh) /. float(fh);
      ignore(Sdl.gl_set_attribute(Sdl.Gl.stencil_size, 1));
      switch (Sdl.gl_create_context(w)) {
      | Error(`Msg(e)) =>
        Sdl.log("Create context error: %s", e);
        exit(1);
      | Ok(ctx) =>
        let vg = Renderer.create(~antialias=true, ());
        let t = ref(0.0);
        let quit = ref(false);
        let event = Sdl.Event.create();
        while (! quit^) {
          while (Sdl.poll_event(Some(event))) {
            switch (Sdl.Event.enum(Sdl.Event.get(event, Sdl.Event.typ))) {
            | `Quit => quit := true
            | _ => ()
            };
          };
          Unix.sleepf(0.020);
          t := t^ +. 0.050;
          Gl.viewport(0, 0, ow, oh);
          let c = Blender.Theme.background;
          Gg.Color.(Gl.clear_color(r(c), g(c), b(c), a(c)));
          /*Gl.clear_color  0.3 0.3 0.32 1.0;*/
          Gl.(
            clear(
              color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit,
            )
          );
          Gl.enable(Gl.blend);
          Gl.blend_func_separate(
            Gl.one,
            Gl.src_alpha,
            Gl.one,
            Gl.one_minus_src_alpha,
          );
          Gl.enable(Gl.cull_face_enum);
          Gl.disable(Gl.depth_test);
          render(vg, sw, sh, t^);
          Sdl.gl_swap_window(w);
        };
        Sdl.gl_delete_context(ctx);
        Sdl.destroy_window(w);
        Sdl.quit();
        exit(0);
      };
    }
  };
};

let () = main();
/*draw_bevel
  draw_colored_node_wire
  draw_node_wire*/
