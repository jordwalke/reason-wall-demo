open Tsdl;

open Wall;

module C = Wall_canvas;

module P = C.Path;

let normalize = ((dx, dy)) => {
  let d = sqrt(dx *. dx +. dy *. dy);
  if (d > 1.0) {
    (dx /. d, dy /. d);
  } else {
    (dx, dy);
  };
};

let load_font = (name) => {
  let ic = open_in_bin(name);
  let dim = in_channel_length(ic);
  let fd = Unix.descr_of_in_channel(ic);
  let buffer = Bigarray.Array1.map_file(fd, Bigarray.int8_unsigned, Bigarray.c_layout, false, dim);
  let offset = List.hd(Stb_truetype.enum(buffer));
  switch (Stb_truetype.init(buffer, offset)) {
  | None => assert false
  | Some(font) => font
  };
};

let font_icons = lazy (load_font("entypo.ttf"));

let font_sans = lazy (load_font("Roboto-Regular.ttf"));

let font_sans_bold = lazy (load_font("Roboto-Bold.ttf"));

let font_emoji = lazy (load_font("NotoEmoji-Regular.ttf"));

let draw_eyes = (vg, xf, x, y, w, h, mx, my, t) => {
  let ex = w *. 0.23;
  let ey = h *. 0.5;
  let lx = x +. ex;
  let ly = y +. ey;
  let rx = x +. w -. ex;
  let ry = y +. ey;
  let br = min(ex, ey) *. 0.5;
  let blink = 1.0 -. sin(t *. 0.5) ** 200.0 *. 0.8;
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(
      ~sx=x,
      ~sy=y +. h *. 0.5,
      ~ex=x +. w *. 0.1,
      ~ey=y +. h,
      ~inner=Color.v(0.0, 0.0, 0.0, 0.125),
      ~outer=Color.v(0.0, 0.0, 0.0, 0.0625)
    ),
    C.fill_path(
      (t) => {
        P.ellipse(t, ~cx=lx +. 3.0, ~cy=ly +. 16.0, ~rx=ex, ~ry=ey);
        P.ellipse(t, ~cx=rx +. 3.0, ~cy=ry +. 16.0, ~rx=ex, ~ry=ey);
      }
    )
  );
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(
      ~sx=x,
      ~sy=y +. h *. 0.25,
      ~ex=x +. w *. 0.1,
      ~ey=y +. h,
      ~inner=Color.v(0.86, 0.86, 0.86, 1.0),
      ~outer=Color.v(0.5, 0.5, 0.5, 1.0)
    ),
    C.fill_path(
      (t) => {
        P.ellipse(t, ~cx=lx, ~cy=ly, ~rx=ex, ~ry=ey);
        P.ellipse(t, ~cx=rx, ~cy=ry, ~rx=ex, ~ry=ey);
      }
    )
  );
  let (dx, dy) = normalize(((mx -. rx) /. (ex *. 10.0), (my -. ry) /. (ey *. 10.0)));
  let dx = dx *. ex *. 0.4;
  let dy = dy *. ey *. 0.5;
  C.draw'(
    vg,
    xf,
    Paint.color(Color.v(0.125, 0.125, 0.125, 1.0)),
    C.fill_path(
      (t) =>
        P.ellipse(
          t,
          ~cx=lx +. dx,
          ~cy=ly +. dy +. ey *. 0.25 *. (1.0 -. blink),
          ~rx=br,
          ~ry=br *. blink
        )
    )
  );
  C.draw'(
    vg,
    xf,
    Paint.color(Color.v(0.125, 0.125, 0.125, 1.0)),
    C.fill_path(
      (t) =>
        P.ellipse(
          t,
          ~cx=rx +. dx,
          ~cy=ry +. dy +. ey *. 0.25 *. (1.0 -. blink),
          ~rx=br,
          ~ry=br *. blink
        )
    )
  );
  let gloss =
    Paint.radial_gradient(
      ~cx=lx -. ex *. 0.25,
      ~cy=ry -. ey *. 0.5,
      ~inr=ex *. 0.1,
      ~outr=ex *. 0.75,
      ~inner=Color.v(1.0, 1.0, 1.0, 0.5),
      ~outer=Color.v(1.0, 1.0, 1.0, 0.0)
    );
  C.draw'(vg, xf, gloss, C.fill_path((t) => P.ellipse(t, ~cx=lx, ~cy=ly, ~rx=ex, ~ry=ey)));
  let gloss =
    Paint.radial_gradient(
      ~cx=rx -. ex *. 0.25,
      ~cy=ry -. ey *. 0.5,
      ~inr=ex *. 0.1,
      ~outr=ex *. 0.75,
      ~inner=Color.v(1.0, 1.0, 1.0, 0.5),
      ~outer=Color.v(1.0, 1.0, 1.0, 0.0)
    );
  C.draw'(vg, xf, gloss, C.fill_path((t) => P.ellipse(t, ~cx=rx, ~cy=ry, ~rx=ex, ~ry=ey)));
};

let draw_graph = (vg, xf, x, y, w, h, t) => {
  let samples = [|
    (1.0 +. sin(t *. 1.2345 +. cos(t *. 0.33457) *. 0.44)) *. 0.5,
    (1.0 +. sin(t *. 0.68363 +. cos(t *. 1.3) *. 1.55)) *. 0.5,
    (1.0 +. sin(t *. 1.1642 +. cos(t *. 0.33457) *. 1.24)) *. 0.5,
    (1.0 +. sin(t *. 0.56345 +. cos(t *. 1.63) *. 0.14)) *. 0.5,
    (1.0 +. sin(t *. 1.6245 +. cos(t *. 0.254) *. 0.3)) *. 0.5,
    (1.0 +. sin(t *. 0.345 +. cos(t *. 0.03) *. 0.6)) *. 0.5
  |];
  let dx = w /. 5.0;
  let sx = (i) => x +. float(i) *. dx;
  let sy = (i) => y +. h *. samples[i] *. 0.8;
  /* Graph background */
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(
      ~sx=x,
      ~sy=y,
      ~ex=x,
      ~ey=y +. h,
      ~inner=Color.v(0.00, 0.60, 0.75, 0.00),
      ~outer=Color.v(0.00, 0.60, 0.75, 0.25)
    ),
    C.fill_path(
      (t) => {
        P.move_to(t, ~x=sx(0), ~y=sy(0));
        for (i in 1 to 5) {
          P.bezier_to(
            t,
            ~c1x=sx(i - 1) +. dx *. 0.5,
            ~c1y=sy(i - 1),
            ~c2x=sx(i) -. dx *. 0.5,
            ~c2y=sy(i),
            ~x=sx(i),
            ~y=sy(i)
          );
        };
        P.line_to(t, ~x=x +. w, ~y=y +. h);
        P.line_to(t, ~x, ~y=y +. h);
      }
    )
  );
  /* Graph line */
  C.draw'(
    vg,
    xf,
    Paint.color(Color.v(0.0, 0.0, 0.0, 0.125)),
    C.stroke_path(
      Outline.{...default, stroke_width: 3.0},
      (t) => {
        P.move_to(t, sx(0), sy(0) +. 2.0);
        for (i in 1 to 5) {
          P.bezier_to(
            t,
            ~c1x=sx(i - 1) +. dx *. 0.5,
            ~c1y=sy(i - 1) +. 2.0,
            ~c2x=sx(i) -. dx *. 0.5,
            ~c2y=sy(i) +. 2.0,
            ~x=sx(i),
            ~y=sy(i) +. 2.0
          );
        };
      }
    )
  );
  C.draw'(
    vg,
    xf,
    Paint.color(Color.v(0.0, 0.60, 0.75, 1.0)),
    C.stroke_path(
      Outline.{...default, stroke_width: 3.0},
      (t) => {
        P.move_to(t, sx(0), sy(0));
        for (i in 1 to 5) {
          P.bezier_to(
            t,
            ~c1x=sx(i - 1) +. dx *. 0.5,
            ~c1y=sy(i - 1),
            ~c2x=sx(i) -. dx *. 0.5,
            ~c2y=sy(i),
            ~x=sx(i),
            ~y=sy(i)
          );
        };
      }
    )
  );
  /* Graph sample pos */
  for (i in 0 to 5) {
    C.draw'(
      vg,
      xf,
      Paint.radial_gradient(
        ~cx=sx(i),
        ~cy=sy(i) +. 2.0,
        ~inr=3.0,
        ~outr=8.0,
        ~inner=Color.v(0.0, 0.0, 0.0, 0.125),
        ~outer=Color.v(0.0, 0.0, 0.0, 0.0)
      ),
      C.fill_path((t) => P.rect(t, ~x=sx(i) -. 10.0, ~y=sy(i) -. 10.0 +. 2.0, ~w=20.0, ~h=20.0))
    );
  };
  C.draw'(
    vg,
    xf,
    Paint.color(Color.v(0.0, 0.6, 0.75, 1.0)),
    C.fill_path(
      (t) =>
        for (i in 0 to 5) {
          P.circle(t, ~cx=sx(i), ~cy=sy(i), ~r=4.0);
        }
    )
  );
  C.draw'(
    vg,
    xf,
    Paint.color(Color.v(0.8, 0.8, 0.8, 1.0)),
    C.fill_path(
      (t) =>
        for (i in 0 to 5) {
          P.circle(t, ~cx=sx(i), ~cy=sy(i), ~r=2.0);
        }
    )
  );
};

let draw_spinner = (~frame=?, vg, xf, cx, cy, r, t) => {
  let a0 = 0.0 +. t *. 6.0;
  let a1 = C.pi +. t *. 6.0;
  let r0 = r;
  let r1 = r *. 0.75;
  let sx = cx +. cos(a0) *. (r0 +. r1) *. 0.5;
  let sy = cy +. sin(a0) *. (r0 +. r1) *. 0.5;
  let ex = cx +. cos(a1) *. (r0 +. r1) *. 0.5;
  let ey = cy +. sin(a1) *. (r0 +. r1) *. 0.5;
  C.draw'(
    ~frame?,
    vg,
    xf,
    Paint.linear_gradient(
      ~sx,
      ~sy,
      ~ex,
      ~ey,
      ~inner=Color.v(0.0, 0.0, 0.0, 0.0),
      ~outer=Color.v(0.0, 0.0, 0.0, 0.5)
    ),
    C.fill_path(
      (t) => {
        P.arc(t, ~cx, ~cy, ~r=r0, ~a0, ~a1, `CW);
        P.arc(t, ~cx, ~cy, ~r=r1, ~a0=a1, ~a1=a0, `CCW);
        P.close(t);
      }
    )
  );
};

let draw_colorwheel = (vg, xf, x, y, w, h, t) => {
  let cx = x +. w *. 0.5;
  let cy = y +. h *. 0.5;
  let hue = sin(t *. 0.12);
  let r1 = min(w, h) *. 0.5 -. 5.0;
  let r0 = r1 -. 20.0;
  let aeps = 0.5 /. r1;
  for (i in 0 to 5) {
    let a0 = float(i) /. 6.0 *. C.pi *. 2.0 -. aeps;
    let a1 = (float(i) +. 1.0) /. 6.0 *. C.pi *. 2.0 +. aeps;
    let sx = cx +. cos(a0) *. (r0 +. r1) *. 0.5;
    let sy = cy +. sin(a0) *. (r0 +. r1) *. 0.5;
    let ex = cx +. cos(a1) *. (r0 +. r1) *. 0.5;
    let ey = cy +. sin(a1) *. (r0 +. r1) *. 0.5;
    /*Printf.printf "sx=%f, sy=%f, ex=%f, ey=%f\n%!" sx sy ex ey;*/
    C.draw'(
      vg,
      xf,
      Paint.linear_gradient(
        ~sx,
        ~sy,
        ~ex,
        ~ey,
        ~inner=Color.hsl(a0 /. (2.0 *. C.pi), 1.0, 0.55),
        ~outer=Color.hsl(a1 /. (2.0 *. C.pi), 1.0, 0.55)
      ),
      C.fill_path(
        (t) => {
          P.arc(t, ~cx, ~cy, ~r=r0, ~a0, ~a1, `CW);
          P.arc(t, ~cx, ~cy, ~r=r1, ~a0=a1, ~a1=a0, `CCW);
          P.close(t);
        }
      )
    );
  };
  C.draw'(
    vg,
    xf,
    Paint.color(Color.v(0.0, 0.0, 0.0, 0.25)),
    C.stroke_path(
      Outline.{...default, stroke_width: 1.0},
      (t) => {
        P.circle(t, ~cx, ~cy, ~r=r0 -. 0.5);
        P.circle(t, ~cx, ~cy, ~r=r1 +. 0.5);
      }
    )
  );
  let xf = Transform.(rotate(hue *. 2.0 *. C.pi, translate(~x=cx, ~y=cy, xf)));
  /* Selector */
  C.draw'(
    vg,
    xf,
    Paint.color(Color.gray(~a=0.75, 1.0)),
    C.stroke_path(
      Outline.{...default, stroke_width: 2.0},
      (t) => P.rect(t, r0 -. 1.0, -3.0, r1 -. r0 +. 2., 6.0)
    )
  );
  C.draw'(
    vg,
    xf,
    Paint.box_gradient(
      ~x=r0 -. 3.0,
      ~y=-5.0,
      ~w=r1 -. r0 +. 6.0,
      ~h=10.0,
      ~r=2.0,
      ~f=4.0,
      ~inner=Color.gray(~a=0.5, 0.0),
      ~outer=Color.gray(~a=0.0, 0.0)
    ),
    C.fill_path(
      (t) => {
        P.rect(
          t,
          ~x=r0 -. 2.0 -. 10.0,
          ~y=(-4.0) -. 10.0,
          ~w=r1 -. r0 +. 4.0 +. 20.0,
          ~h=8.0 +. 20.0
        );
        P.rect(t, ~x=r0 -. 2.0, ~y=-4.0, ~w=r1 -. r0 +. 4.0, ~h=8.0);
        P.set_winding(t, `HOLE);
      }
    )
  );
  /* Center triangle */
  let r = r0 -. 6.0;
  let ax = cos(120.0 /. 180.0 *. C.pi) *. r;
  let ay = sin(120.0 /. 180.0 *. C.pi) *. r;
  let bx = cos((-120.0) /. 180.0 *. C.pi) *. r;
  let by = sin((-120.0) /. 180.0 *. C.pi) *. r;
  let path =
    C.path(
      (t) => {
        P.move_to(t, r, 0.0);
        P.line_to(t, ax, ay);
        P.line_to(t, bx, by);
        P.close(t);
      }
    );
  /*Printf.printf "sx=%f, sy=%f, ex=%f, ey=%f\n%!" r 0.0 ax ay;*/
  let fill = C.fill(path);
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(
      ~sx=r,
      ~sy=0.0,
      ~ex=ax,
      ~ey=ay,
      ~inner=Color.hsl(hue, 1.0, 0.5),
      ~outer=Color.white
    ),
    fill
  );
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(
      ~sx=(r +. ax) *. 0.5,
      ~sy=(0.0 +. ay) *. 0.5,
      ~ex=bx,
      ~ey=by,
      ~inner=Color.gray(~a=0.0, 0.0),
      ~outer=Color.gray(~a=1.0, 0.0)
    ),
    fill
  );
  C.draw'(vg, xf, Paint.color(Color.gray(~a=0.25, 0.0)), C.stroke(Outline.default, path));
  /* Select circle on triangle */
  let ax = cos(120.0 /. 180.0 *. C.pi) *. r *. 0.3;
  let ay = sin(120.0 /. 180.0 *. C.pi) *. r *. 0.4;
  C.draw'(
    vg,
    xf,
    Paint.color(Color.gray(~a=0.75, 1.0)),
    C.stroke_path(
      Outline.{...default, stroke_width: 2.0},
      (t) => P.circle(t, ~cx=ax, ~cy=ay, ~r=5.0)
    )
  );
  C.draw'(
    vg,
    xf,
    Paint.radial_gradient(
      ~cx=ax,
      ~cy=ay,
      ~inr=7.0,
      ~outr=9.0,
      ~inner=Color.gray(~a=0.25, 0.0),
      ~outer=Color.gray(~a=0.0, 0.0)
    ),
    C.fill_path(
      (t) => {
        P.rect(t, ~x=ax -. 20.0, ~y=ay -. 20.0, ~w=40.0, ~h=40.0);
        P.circle(t, ~cx=ax, ~cy=ay, ~r=7.0);
        P.set_winding(t, `HOLE);
      }
    )
  );
};

let draw_lines = (vg, xf, x, y, w, _h, t) => {
  let pad = 5.0;
  let s = w /. 9.0 -. pad *. 2.0;
  let joins = [|`MITER, `ROUND, `BEVEL|];
  let caps = [|`BUTT, `ROUND, `SQUARE|];
  let px =
    fun
    | 0 => -. s *. 0.25 +. cos(t *. 0.3) *. s *. 0.5
    | 1 => -. s *. 0.25
    | 2 => s *. 0.25
    | 3 => s *. 0.25 +. cos(-. t *. 0.3) *. s *. 0.5
    | _ => assert false;
  let py =
    fun
    | 0 => sin(t *. 0.3) *. s *. 0.5
    | 1 => 0.0
    | 2 => 0.0
    | 3 => sin(-. t *. 0.3) *. s *. 0.5
    | _ => assert false;
  for (i in 0 to 2) {
    for (j in 0 to 2) {
      let fx = x +. s *. 0.5 +. float(i * 3 + j) /. 9.0 *. w +. pad;
      let fy = y -. s *. 0.5 +. pad;
      let px = (i) => fx +. px(i);
      let py = (i) => fy +. py(i);
      C.draw'(
        vg,
        xf,
        Paint.color(Color.gray(~a=0.625, 0.0)),
        C.stroke_path(
          Outline.{...default, stroke_width: s *. 0.3, line_cap: caps[i], line_join: joins[j]},
          (t) => {
            P.move_to(t, px(0), py(0));
            P.line_to(t, px(1), py(1));
            P.line_to(t, px(2), py(2));
            P.line_to(t, px(3), py(3));
          }
        )
      );
      C.draw'(
        vg,
        xf,
        Paint.color(Color.v(0.0, 0.75, 1.0, 1.0)),
        C.stroke_path(
          Outline.{...default, stroke_width: 1.0, line_cap: `BUTT, line_join: `BEVEL},
          (t) => {
            P.move_to(t, px(0), py(0));
            P.line_to(t, px(1), py(1));
            P.line_to(t, px(2), py(2));
            P.line_to(t, px(3), py(3));
          }
        )
      );
    };
  };
};

let draw_widths = (vg, xf, x, y, w) => {
  let paint = Paint.color(Color.black);
  let y = ref(y);
  for (i in 0 to 19) {
    let y' = y^;
    C.draw'(
      vg,
      xf,
      paint,
      C.stroke_path(
        Outline.{...default, stroke_width: (float(i) +. 0.5) /. 10.0},
        (t) => {
          P.move_to(t, x, y');
          P.line_to(t, x +. w, y' +. w *. 0.3);
        }
      )
    );
    y := y^ +. 10.0;
  };
};

let draw_caps = (vg, xf, x, y, w) => {
  let caps = [|`BUTT, `ROUND, `SQUARE|];
  let stroke_width = 8.0;
  C.draw'(
    vg,
    xf,
    Paint.color(Color.gray(~a=0.125, 1.0)),
    C.fill_path(
      (t) => {
        P.rect(t, x, y, w, 40.0);
        P.rect(t, x -. stroke_width /. 2.0, y, w +. stroke_width, 40.0);
      }
    )
  );
  for (i in 0 to 2) {
    C.draw'(
      vg,
      xf,
      Paint.black,
      C.stroke_path(
        Outline.{...default, stroke_width, line_cap: caps[i]},
        (t) => {
          P.move_to(t, x, y +. float(i * 10 + 5));
          P.line_to(t, x +. w, y +. float(i * 10 + 5));
        }
      )
    );
  };
};

let draw_scissor = (vg, xf, x, y, t) => {
  let frame = Frame.default;
  let xf = Transform.(rotate(5.0 /. 180.0 *. C.pi, translate(~x, ~y, xf)));
  /* Draw first rect and set scissor to it's area. */
  C.draw'(
    vg,
    xf,
    Paint.color(Color.v(1.0, 0.0, 0.0, 1.0)),
    C.fill_path((t) => P.rect(t, -20.0, -20.0, 60.0, 40.0))
  );
  /* Draw second rectangle with offset and rotation. */
  let frame = Frame.set_scissor(-20.0, -20.0, 60.0, 40.0, xf, frame);
  let xf = Transform.(rotate(t, translate(40.0, 0.0, xf)));
  /* Draw the intended second rectangle without any scissoring. */
  let shape = C.fill_path((t) => P.rect(t, -20.0, -10.0, 60.0, 30.0));
  C.draw'(vg, xf, Paint.color(Color.v(1.0, 0.5, 0.0, 0.25)), shape);
  /* Draw second rectangle with combined scissoring. */
  let frame = Frame.intersect_scissor(-20.0, -10.0, 60.0, 30.0, xf, frame);
  C.draw'(vg, xf, Paint.color(Color.v(1.0, 0.5, 0.0, 1.0)), shape, ~frame);
};

let draw_window = (vg, xf, title, x, y, w, h) => {
  let cornerRadius = 3.0;
  /* Window */
  C.draw'(
    vg,
    xf,
    Paint.color(Color.v(0.110, 0.118, 0.133, 0.75)),
    C.fill_path((t) => P.round_rect(t, x, y, w, h, cornerRadius))
  );
  /* Drop shadow */
  C.draw'(
    vg,
    xf,
    Paint.box_gradient(
      x,
      y +. 2.0,
      w,
      h,
      cornerRadius *. 2.0,
      10.0,
      Color.gray(~a=0.5, 0.0),
      Color.gray(~a=0.0, 0.0)
    ),
    C.fill_path(
      (t) => {
        P.rect(t, x -. 10.0, y -. 10.0, w +. 20.0, h +. 30.0);
        P.round_rect(t, x, y, w, h, cornerRadius);
        P.set_winding(t, `HOLE);
      }
    )
  );
  /* Header */
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(x, y, x, y +. 15.0, Color.gray(~a=0.04, 1.0), Color.gray(~a=0.08, 1.0)),
    C.fill_path((t) => P.round_rect(t, x +. 1.0, y +. 1.0, w -. 2.0, 30.0, cornerRadius -. 1.0))
  );
  C.draw'(
    vg,
    xf,
    Paint.color(Color.gray(~a=0.125, 0.0)),
    C.stroke_path(
      Outline.default,
      (t) => {
        P.move_to(t, x +. 0.5, y +. 0.5 +. 30.0);
        P.line_to(t, x +. 0.5 +. w -. 1.0, y +. 0.5 +. 30.0);
      }
    )
  );
  let font = Lazy.force(font_sans_bold);
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.6, 0.9)),
    Font.make(~blur=2.0, ~size=18.0, font),
    ~valign=`MIDDLE,
    ~halign=`CENTER,
    ~x=x +. w /. 2.,
    ~y=y +. 16. +. 1.0,
    title
  );
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.6, 0.9)),
    Font.make(~size=18.0, font),
    ~valign=`MIDDLE,
    ~halign=`CENTER,
    ~x=x +. w /. 2.,
    ~y=y +. 16.,
    title
  );
};

let draw_searchbox = (vg, xf, text, x, y, w, h) => {
  let cornerRadius = h /. 2.0 -. 1.0;
  /* Edit */
  C.draw'(
    vg,
    xf,
    Paint.box_gradient(
      x,
      y +. 1.5,
      w,
      h,
      h /. 2.0,
      5.0,
      Color.gray(~a=0.08, 0.0),
      Color.gray(~a=0.375, 0.0)
    ),
    C.fill_path((t) => P.round_rect(t, x, y, w, h, cornerRadius))
  );
  C.draw'(
    vg,
    xf,
    Paint.color(Color.gray(~a=0.2, 0.0)),
    C.stroke_path(
      Outline.default,
      (t) => P.round_rect(t, x +. 0.5, y +. 0.5, w -. 1.0, h -. 1.0, cornerRadius -. 0.5)
    )
  );
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.25, 1.0)),
    Font.make(~size=h *. 1.3, Lazy.force(font_icons)),
    ~valign=`MIDDLE,
    ~halign=`CENTER,
    ~x=x +. h *. 0.55,
    ~y=y +. h *. 0.55,
    "\240\159\148\141"
  );
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.125, 1.0)),
    Font.make(~size=20.0, Lazy.force(font_sans)),
    ~valign=`MIDDLE,
    ~halign=`LEFT,
    ~x=x +. h *. 1.05,
    ~y=y +. h *. 0.5,
    text
  );
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.125, 1.0)),
    Font.make(~size=h *. 1.3, Lazy.force(font_icons)),
    ~valign=`MIDDLE,
    ~halign=`CENTER,
    ~x=x +. w -. h *. 0.55,
    ~y=y +. h *. 0.55,
    "\226\156\150"
  );
};

let draw_dropdown = (vg, xf, text, x, y, w, h) => {
  let cornerRadius = 4.0;
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(x, y, x, y +. h, Color.gray(~a=0.08, 1.0), Color.gray(~a=0.08, 0.0)),
    C.fill_path(
      (t) => P.round_rect(t, x +. 1.0, y +. 1.0, w -. 2.0, h -. 2.0, cornerRadius -. 1.0)
    )
  );
  C.draw'(
    vg,
    xf,
    Paint.color(Color.gray(~a=0.1875, 0.0)),
    C.stroke_path(
      Outline.default,
      (t) => P.round_rect(t, x +. 0.5, y +. 0.5, w -. 1.0, h -. 1.0, cornerRadius -. 0.5)
    )
  );
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.8, 1.0)),
    Font.make(~size=20.0, Lazy.force(font_sans)),
    ~valign=`MIDDLE,
    ~halign=`LEFT,
    ~x=x +. h *. 0.3,
    ~y=y +. h *. 0.5,
    text
  );
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.8, 1.0)),
    Font.make(~size=h *. 1.3, Lazy.force(font_icons)),
    ~valign=`MIDDLE,
    ~halign=`CENTER,
    ~x=x +. w -. h *. 0.5,
    ~y=y +. h *. 0.5,
    "\238\157\158 "
  );
};

let draw_label = (vg, xf, text, x, y, w, h) =>
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.5, 1.0)),
    Font.make(~size=18.0, Lazy.force(font_sans)),
    ~valign=`MIDDLE,
    ~halign=`LEFT,
    ~x,
    ~y=y +. h *. 0.5,
    text
  );

let draw_editboxbase = (vg, xf, x, y, w, h) => {
  C.draw'(
    vg,
    xf,
    Paint.box_gradient(
      x +. 1.0,
      y +. 1.0 +. 1.5,
      w -. 2.0,
      h -. 2.0,
      3.0,
      4.0,
      Color.gray(~a=0.125, 1.0),
      Color.gray(~a=0.125, 0.125)
    ),
    C.fill_path((t) => P.round_rect(t, x +. 1.0, y +. 1.0, w -. 2.0, h -. 2.0, 4.0 -. 1.0))
  );
  C.draw'(
    vg,
    xf,
    Paint.color(Color.gray(~a=0.1875, 0.0)),
    C.stroke_path(
      Outline.default,
      (t) => P.round_rect(t, x +. 0.5, y +. 0.5, w -. 1.0, h -. 1.0, 4.0 -. 0.5)
    )
  );
};

let draw_editbox = (vg, xf, text, x, y, w, h) => {
  draw_editboxbase(vg, xf, x, y, w, h);
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.25, 1.0)),
    Font.make(~size=20.0, Lazy.force(font_sans)),
    ~valign=`MIDDLE,
    ~halign=`LEFT,
    ~x=x +. h *. 0.3,
    ~y=y +. h *. 0.5,
    text
  );
};

let draw_editboxnum = (vg, xf, text, units, x, y, w, h) => {
  draw_editboxbase(vg, xf, x, y, w, h);
  let ufont = Font.make(~size=18.0, Lazy.force(font_sans));
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.25, 1.0)),
    ~valign=`MIDDLE,
    ufont,
    ~halign=`RIGHT,
    ~x=x +. w -. h *. 0.3,
    ~y=y +. h *. 0.5,
    units
  );
  let uw = Font.text_width(ufont, units);
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.5, 1.0)),
    Font.make(~size=20.0, Lazy.force(font_sans)),
    ~valign=`MIDDLE,
    ~halign=`RIGHT,
    ~x=x +. w -. uw -. h *. 0.5,
    ~y=y +. h *. 0.5,
    text
  );
};

let draw_checkbox = (vg, xf, text, x, y, w, h) => {
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.66, 1.0)),
    Font.make(~size=18.0, Lazy.force(font_sans)),
    ~valign=`MIDDLE,
    ~x=x +. 28.,
    ~y=y +. h *. 0.5,
    text
  );
  C.draw'(
    vg,
    xf,
    Paint.box_gradient(
      x +. 1.0,
      y +. floor(h /. 2.0) -. 9.0 +. 1.0,
      18.0,
      18.0,
      3.0,
      3.0,
      Color.gray(~a=0.125, 0.0),
      Color.gray(~a=0.375, 0.0)
    ),
    C.fill_path((t) => P.round_rect(t, x +. 1.0, y +. floor(h /. 2.0) -. 9.0, 18.0, 18.0, 3.0))
  );
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.5, 1.0)),
    Font.make(~size=40.0, Lazy.force(font_icons)),
    ~valign=`MIDDLE,
    ~halign=`CENTER,
    ~x=x +. 11.,
    ~y=y +. h *. 0.5,
    "\226\156\147"
  );
};

let cp_to_utf8 = (cp) => {
  let n =
    if (cp < 0x80) {
      1;
    } else if (cp < 0x800) {
      2;
    } else if (cp < 0x10000) {
      3;
    } else if (cp < 0x200000) {
      4;
    } else if (cp < 0x4000000) {
      5;
    } else if (cp <= 0x7fffffff) {
      6;
    } else {
      assert false;
    };
  let str = Bytes.create(n);
  let cp = ref(cp);
  try {
    if (n > 5) {
      str.[5] = Char.chr(0x80 lor (cp^ land 0x3f));
      cp := cp^ lsr 6 lor 0x4000000;
    };
    if (n > 4) {
      str.[4] = Char.chr(0x80 lor (cp^ land 0x3f));
      cp := cp^ lsr 6 lor 0x200000;
    };
    if (n > 3) {
      str.[3] = Char.chr(0x80 lor (cp^ land 0x3f));
      cp := cp^ lsr 6 lor 0x10000;
    };
    if (n > 2) {
      str.[2] = Char.chr(0x80 lor (cp^ land 0x3f));
      cp := cp^ lsr 6 lor 0x800;
    };
    if (n > 1) {
      str.[1] = Char.chr(0x80 lor (cp^ land 0x3f));
      cp := cp^ lsr 6 lor 0xc0;
    };
    str.[0] = Char.chr(cp^);
  } {
  | exn =>
    prerr_endline("cp: " ++ string_of_int(cp^));
    raise(exn);
  };
  Bytes.to_string(str);
};

let draw_button = (vg, xf, preicon, text, x, y, w, h, col) => {
  let is_black = Color.a(col) > 0.0;
  let cornerRadius = 4.0;
  let shape =
    C.fill_path(
      (t) => P.round_rect(t, x +. 1.0, y +. 1.0, w -. 2.0, h -. 2.0, cornerRadius -. 1.0)
    );
  if (is_black) {
    C.draw'(vg, xf, Paint.color(col), shape);
  };
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(
      x,
      y,
      x,
      y +. h,
      Color.gray(1.0, ~a=if (is_black) {0.125} else {0.25}),
      Color.gray(0.0, ~a=if (is_black) {0.125} else {0.25})
    ),
    shape
  );
  C.draw'(
    vg,
    xf,
    Paint.color(Color.gray(~a=0.375, 0.0)),
    C.stroke_path(
      Outline.default,
      (t) => P.round_rect(t, x +. 0.5, y +. 0.5, w -. 1.0, h -. 1.0, cornerRadius -. 0.5)
    )
  );
  let font = Font.make(~size=20.0, Lazy.force(font_sans_bold));
  let tw = Font.text_width(font, text);
  let iw =
    if (preicon == 0) {
      0.0;
    } else {
      let font = Font.make(~size=h *. 1.3, Lazy.force(font_icons));
      let icon = cp_to_utf8(preicon);
      let iw = Font.text_width(font, icon);
      C.text'(
        vg,
        xf,
        Paint.color(Gg.Color.gray(~a=0.40, 1.0)),
        font,
        ~halign=`LEFT,
        ~valign=`MIDDLE,
        ~x=x +. w *. 0.5 -. tw *. 0.5 -. iw *. 0.75,
        ~y=y +. h *. 0.5,
        icon
      );
      iw;
    };
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.66, 0.0)),
    font,
    ~valign=`MIDDLE,
    ~halign=`LEFT,
    ~x=x +. w *. 0.5 -. tw *. 0.5 +. iw *. 0.25,
    ~y=y +. h *. 0.5 -. 0.5,
    text
  );
  C.text'(
    vg,
    xf,
    Paint.color(Gg.Color.gray(~a=0.66, 1.0)),
    font,
    ~valign=`MIDDLE,
    ~halign=`LEFT,
    ~x=x +. w *. 0.5 -. tw *. 0.5 +. iw *. 0.25,
    ~y=y +. h *. 0.5,
    text
  );
};

let draw_slider = (vg, xf, pos, x, y, w, h) => {
  let cy = y +. floor(h *. 0.5);
  let kr = floor(h *. 0.25);
  /* Slot */
  C.draw'(
    vg,
    xf,
    Paint.box_gradient(
      x,
      cy -. 2.0 +. 1.0,
      w,
      4.0,
      2.0,
      2.0,
      Color.gray(~a=0.125, 0.0),
      Color.gray(~a=0.5, 0.0)
    ),
    C.fill_path((t) => P.round_rect(t, x, cy -. 2., w, 4.0, 2.0))
  );
  /* Knob Shadow */
  C.draw'(
    vg,
    xf,
    Paint.radial_gradient(
      x +. floor(pos *. w),
      cy +. 1.0,
      kr -. 3.0,
      kr +. 3.0,
      Color.gray(~a=0.25, 0.0),
      Color.gray(~a=0.0, 0.0)
    ),
    C.fill_path(
      (t) => {
        P.rect(
          t,
          x +. floor(pos *. w) -. kr -. 5.0,
          cy -. kr -. 5.0,
          kr *. 2.0 +. 5.0 +. 5.0,
          kr *. 2.0 +. 5.0 +. 5.0 +. 3.0
        );
        P.circle(t, x +. floor(pos *. w), cy, kr);
        P.set_winding(t, `HOLE);
      }
    )
  );
  /* Knob */
  let shape = C.fill_path((t) => P.circle(t, x +. floor(pos *. w), cy, kr -. 1.0));
  C.draw'(vg, xf, Paint.color(Color.v_srgbi(40, 43, 48)), shape);
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(
      x,
      cy -. kr,
      x,
      cy +. kr,
      Color.gray(~a=0.0625, 1.0),
      Color.gray(~a=0.0625, 0.0)
    ),
    shape
  );
  C.draw'(
    vg,
    xf,
    Paint.color(Color.gray(~a=0.375, 0.0)),
    C.stroke_path(Outline.default, (t) => P.circle(t, x +. floor(pos *. w), cy, kr -. 0.5))
  );
  ();
};

let image_size = (image) => (Wall_tex.width(image), Wall_tex.height(image));

let image_texture = (image) => image;

let load_demo_data = () =>
  Array.init(
    12,
    (i) => {
      let name = Printf.sprintf("images/image%d.jpg", i + 1);
      switch (Wall_tex.load_image(~alpha=false, ~name, name)) {
      | Result.Ok(image) => image
      | Result.Error(`Msg(msg)) =>
        Printf.eprintf("error loading %s: %s\n%!", name, msg);
        exit(1);
      };
    }
  );

let draw_thumbnails = (vg, xf, x, y, w, h, images, t) => {
  let cornerRadius = 3.0
  and thumb = 60.0
  and arry = 30.5;
  let stackh = float(Array.length(images) / 2) *. (thumb +. 10.0) +. 10.0;
  let u = (1.0 +. cos(t *. 0.5)) *. 0.5;
  let u2 = (1.0 -. cos(t *. 0.2)) *. 0.5;
  /* Drop shadow */
  C.draw'(
    vg,
    xf,
    Paint.box_gradient(
      x,
      y +. 4.0,
      w,
      h,
      cornerRadius *. 2.0,
      20.0,
      Color.gray(~a=0.5, 0.0),
      Color.gray(~a=0.0, 0.0)
    ),
    C.fill_path(
      (t) => {
        P.rect(t, x -. 10.0, y -. 10.0, w +. 20.0, h +. 30.0);
        P.round_rect(t, x, y, w, h, cornerRadius);
        P.set_winding(t, `HOLE);
      }
    )
  );
  /* Window */
  C.draw'(
    vg,
    xf,
    Paint.color(Color.gray(0.8)),
    C.fill_path(
      (t) => {
        P.round_rect(t, x, y, w, h, cornerRadius);
        P.move_to(t, x -. 10.0, y +. arry);
        P.line_to(t, x +. 1.0, y +. arry -. 11.0);
        P.line_to(t, x +. 1.0, y +. arry +. 11.0);
      }
    )
  );
  let frame = Frame.set_scissor(x, y, w, h, xf, Frame.default);
  let xf' = Transform.translate(0.0, -. (stackh -. h) *. u, xf);
  let dv = 1.0 /. float(Array.length(images) - 1);
  Array.iteri(
    (i, image) => {
      let tx = x +. 10.0 +. float(i mod 2) *. (thumb +. 10.0);
      let ty = y +. 10.0 +. float(i / 2) *. (thumb +. 10.0);
      let (imgw, imgh) = image_size(image);
      let (imgw, imgh) = (float(imgw), float(imgh));
      let (iw, ih, ix, iy) =
        if (imgw < imgh) {
          let iw = thumb;
          let ih = iw *. imgh /. imgw;
          (iw, ih, 0.0, -. (ih -. thumb) *. 0.5);
        } else {
          let ih = thumb;
          let iw = ih *. imgw /. imgh;
          (iw, ih, -. (iw -. thumb) *. 0.5, 0.0);
        };
      let v = float(i) *. dv;
      let a = max(0.0, min(1.0, (u2 -. v) /. dv));
      if (a < 1.0) {
        draw_spinner(~frame, vg, xf', tx +. thumb /. 2.0, ty +. thumb /. 2.0, thumb *. 0.25, t);
      };
      C.draw'(
        ~frame,
        vg,
        xf',
        Paint.image_pattern(
          Gg.P2.v(tx +. ix, ty +. iy),
          Gg.Size2.v(iw, ih),
          0.0,
          a,
          image_texture(image)
        ),
        C.fill_path((t) => P.round_rect(t, tx, ty, thumb, thumb, 5.0))
      );
      C.draw'(
        ~frame,
        vg,
        xf',
        Paint.box_gradient(
          tx -. 1.0,
          ty,
          thumb +. 2.0,
          thumb +. 2.0,
          5.0,
          3.0,
          Color.gray(~a=0.5, 0.0),
          Color.gray(~a=0.0, 0.0)
        ),
        C.fill_path(
          (t) => {
            P.rect(t, tx -. 5.0, ty -. 5.0, thumb +. 10.0, thumb +. 10.0);
            P.round_rect(t, tx, ty, thumb, thumb, 6.0);
            P.set_winding(t, `HOLE);
          }
        )
      );
      C.draw'(
        ~frame,
        vg,
        xf',
        Paint.color(Color.gray(~a=0.75, 1.0)),
        C.stroke_path(
          Outline.{...default, stroke_width: 1.0},
          (t) => P.round_rect(t, tx +. 0.5, ty +. 0.5, thumb -. 1.0, thumb -. 1.0, 4.0 -. 0.5)
        )
      );
    },
    images
  );
  /* Hide fades */
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(x, y, x, y +. 6.0, Color.gray(~a=1.0, 0.8), Color.gray(~a=0.0, 0.8)),
    C.fill_path((t) => P.rect(t, x +. 4.0, y, w -. 8.0, 6.0))
  );
  C.draw'(
    vg,
    xf,
    Paint.linear_gradient(
      x,
      y +. h -. 6.0,
      x,
      y +. 6.0,
      Color.gray(~a=1.0, 0.8),
      Color.gray(~a=0.0, 0.8)
    ),
    C.fill_path((t) => P.rect(t, x +. 4.0, y +. h -. 6.0, w -. 8.0, 6.0))
  );
  /* Scroll bar */
  C.draw'(
    vg,
    xf,
    Paint.box_gradient(
      x +. w -. 12.0 +. 1.0,
      y +. 4.0 +. 1.0,
      8.0,
      h -. 8.0,
      3.0,
      4.0,
      Color.gray(~a=0.125, 0.0),
      Color.gray(~a=0.375, 0.0)
    ),
    C.fill_path((t) => P.round_rect(t, x +. w -. 12.0, y +. 4.0, 8.0, h -. 8.0, 3.0))
  );
  let scrollh = h /. stackh *. (h -. 8.0);
  C.draw'(
    vg,
    xf,
    Paint.box_gradient(
      x +. w -. 12. -. 1.,
      y +. 4. +. (h -. 8. -. scrollh) *. u -. 1.,
      8.,
      scrollh,
      3.,
      4.,
      Color.gray(~a=0.9, 1.0),
      Color.gray(~a=0.5, 1.0)
    ),
    C.fill_path(
      (t) =>
        P.round_rect(
          t,
          x +. w -. 12. +. 1.,
          y +. 4. +. 1. +. (h -. 8. -. scrollh) *. u,
          8. -. 2.,
          scrollh -. 2.,
          2.
        )
    )
  );
};

let images = lazy (load_demo_data());

let draw_demo = (vg, xf, mx, my, w, h, t) => {
  draw_eyes(vg, xf, w -. 250.0, 50.0, 150.0, 100.0, mx, my, t);
  draw_graph(vg, xf, 0.0, h /. 2.0, w, h /. 2.0, t);
  draw_colorwheel(vg, xf, w -. 300.0, h -. 300.0, 250.0, 250.0, t);
  draw_lines(vg, xf, 120.0, h -. 50.0, 600.0, 50.0, t);
  draw_widths(vg, xf, 10.0, 50.0, 30.0);
  draw_caps(vg, xf, 10.0, 300.0, 30.0);
  draw_scissor(vg, xf, 50.0, h -. 80.0, t);
  /* Widgets */
  draw_window(vg, xf, "Widgets `n Stuff", 50.0, 50.0, 300.0, 400.0);
  let x = 60.0
  and y = 95.0;
  draw_searchbox(vg, xf, "Search", x, y, 280.0, 25.0);
  let y = y +. 40.0;
  draw_dropdown(vg, xf, "Effects", x, y, 280.0, 28.0);
  let popy = y +. 14.0;
  let y = y +. 45.0;
  /* Form */
  draw_label(vg, xf, "login", x, y, 280.0, 20.0);
  let y = y +. 25.0;
  draw_editbox(vg, xf, "Email", x, y, 280.0, 28.0);
  let y = y +. 35.0;
  draw_editbox(vg, xf, "Password", x, y, 280.0, 28.0);
  let y = y +. 38.0;
  draw_checkbox(vg, xf, "Remember me", x, y, 140.0, 28.0);
  draw_button(
    vg,
    xf,
    /*ICON_LOGIN*/ 0xE740,
    "Sign in",
    x +. 138.0,
    y,
    140.0,
    28.0,
    Color.v(0.0, 0.375, 0.5, 1.0)
  );
  let y = y +. 45.0;
  /* Slider */
  draw_label(vg, xf, "Diameter", x, y, 280.0, 20.0);
  let y = y +. 25.0;
  draw_editboxnum(vg, xf, "123.00", "px", x +. 180.0, y, 100.0, 28.0);
  draw_slider(vg, xf, 0.4, x, y, 170.0, 28.0);
  let y = y +. 55.0;
  draw_button(
    vg,
    xf,
    /*ICON_TRASH*/ 0xE729,
    "Delete",
    x,
    y,
    160.0,
    28.0,
    Color.v(0.5, 0.0625, 0.03125, 1.0)
  );
  draw_button(vg, xf, 0, "Cancel", x +. 170.0, y, 110.0, 28.0, Color.gray(~a=0.0, 0.0));
  draw_thumbnails(vg, xf, 365.0, popy -. 30.0, 160.0, 300.0, Lazy.force(images), t);
  ();
};

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

let render = (context, t) => {
  let vg = C.new_frame(context);
  let (_, (x, y)) = Sdl.get_mouse_state();
  let x = float(x) /. f
  and y = float(y) /. f;
  draw_demo(vg, Transform.scale(f, f), x, y, lw, lh, t);
  C.flush_frame(context, Gg.V2.v(pw, ph));
};

open Tgles2;

let main = () => {
  Printexc.record_backtrace(true);
  switch (Sdl.init(Sdl.Init.video)) {
  | Error(`Msg(e)) =>
    Sdl.log("Init error: %s", e);
    exit(1);
  | Ok () =>
    switch (Sdl.create_window(~w=fw, ~h=fh, "SDL OpenGL", Sdl.Window.(opengl + allow_highdpi))) {
    | Error(`Msg(e)) =>
      Sdl.log("Create window error: %s", e);
      exit(1);
    | Ok(w) =>
      /*Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;*/
      /*Sdl.gl_set_attribute Sdl.Gl.context_major_version 2;*/
      /*Sdl.gl_set_attribute Sdl.Gl.context_minor_version 1;*/
      let (ow, oh) = Sdl.gl_get_drawable_size(w);
      Sdl.log("window size: %d,%d\topengl drawable size: %d,%d", fw, fh, ow, oh);
      ignore(Sdl.gl_set_attribute(Sdl.Gl.stencil_size, 1));
      switch (Sdl.gl_create_context(w)) {
      | Error(`Msg(e)) =>
        Sdl.log("Create context error: %s", e);
        exit(1);
      | Ok(ctx) =>
        let context = C.create_gl(~antialias=false);
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
          Gl.viewport(0, 0, fw, fh);
          Gl.clear_color(0.3, 0.3, 0.32, 1.0);
          Gl.(clear(color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit));
          Gl.enable(Gl.blend);
          Gl.blend_func_separate(Gl.one, Gl.src_alpha, Gl.one, Gl.one_minus_src_alpha);
          Gl.enable(Gl.cull_face_enum);
          Gl.disable(Gl.depth_test);
          render(context, t^);
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
