open Tsdl;

open Wall;

module I = Image;

module P = Path;

module Text = Wall_text;

let b2 = (x, y, w, h) => Gg.Box2.v(Gg.P2.v(x, y), Gg.Size2.v(w, h));

let normalize = ((dx, dy)) => {
  let d = sqrt(dx *. dx +. dy *. dy);
  if (d > 1.0) {
    (dx /. d, dy /. d);
  } else {
    (dx, dy);
  };
};

let load_font = name => {
  let ic = open_in_bin(name);
  let dim = in_channel_length(ic);
  let fd = Unix.descr_of_in_channel(ic);
  let buffer =
    Bigarray.Array1.map_file(
      fd,
      Bigarray.int8_unsigned,
      Bigarray.c_layout,
      false,
      dim,
    );
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

let draw_eyes = (x, y, w, h, mx, my, t) => {
  let ex = w *. 0.23;
  let ey = h *. 0.5;
  let lx = x +. ex;
  let ly = y +. ey;
  let rx = x +. w -. ex;
  let ry = y +. ey;
  let br = min(ex, ey) *. 0.5;
  let blink = 1.0 -. sin(t *. 0.5) ** 200.0 *. 0.8;
  let (dx, dy) =
    normalize(((mx -. rx) /. (ex *. 10.0), (my -. ry) /. (ey *. 10.0)));
  let dx = dx *. ex *. 0.4;
  let dy = dy *. ey *. 0.5;
  I.seq([
    I.paint(
      Paint.linear_gradient(
        ~sx=x,
        ~sy=y +. h *. 0.5,
        ~ex=x +. w *. 0.1,
        ~ey=y +. h,
        ~inner=Color.v(0.0, 0.0, 0.0, 0.125),
        ~outer=Color.v(0.0, 0.0, 0.0, 0.0625),
      ),
      I.fill_path @@
      (
        t => {
          P.ellipse(t, ~cx=lx +. 3.0, ~cy=ly +. 16.0, ~rx=ex, ~ry=ey);
          P.ellipse(t, ~cx=rx +. 3.0, ~cy=ry +. 16.0, ~rx=ex, ~ry=ey);
        }
      ),
    ),
    I.paint(
      Paint.linear_gradient(
        ~sx=x,
        ~sy=y +. h *. 0.25,
        ~ex=x +. w *. 0.1,
        ~ey=y +. h,
        ~inner=Color.v(0.86, 0.86, 0.86, 1.0),
        ~outer=Color.v(0.5, 0.5, 0.5, 1.0),
      ),
      I.fill_path @@
      (
        t => {
          P.ellipse(t, ~cx=lx, ~cy=ly, ~rx=ex, ~ry=ey);
          P.ellipse(t, ~cx=rx, ~cy=ry, ~rx=ex, ~ry=ey);
        }
      ),
    ),
    I.paint(
      Paint.color(Color.v(0.125, 0.125, 0.125, 1.0)),
      I.fill_path @@
      (
        t =>
          P.ellipse(
            t,
            ~cx=lx +. dx,
            ~cy=ly +. dy +. ey *. 0.25 *. (1.0 -. blink),
            ~rx=br,
            ~ry=br *. blink,
          )
      ),
    ),
    I.paint(
      Paint.color(Color.v(0.125, 0.125, 0.125, 1.0)),
      I.fill_path @@
      (
        t =>
          P.ellipse(
            t,
            ~cx=rx +. dx,
            ~cy=ry +. dy +. ey *. 0.25 *. (1.0 -. blink),
            ~rx=br,
            ~ry=br *. blink,
          )
      ),
    ),
    /* Gloss */
    I.paint(
      Paint.radial_gradient(
        ~cx=lx -. ex *. 0.25,
        ~cy=ry -. ey *. 0.5,
        ~inr=ex *. 0.1,
        ~outr=ex *. 0.75,
        ~inner=Color.v(1.0, 1.0, 1.0, 0.5),
        ~outer=Color.v(1.0, 1.0, 1.0, 0.0),
      ),
      I.fill_path @@ (t => P.ellipse(t, ~cx=lx, ~cy=ly, ~rx=ex, ~ry=ey)),
    ),
    I.paint(
      Paint.radial_gradient(
        ~cx=rx -. ex *. 0.25,
        ~cy=ry -. ey *. 0.5,
        ~inr=ex *. 0.1,
        ~outr=ex *. 0.75,
        ~inner=Color.v(1.0, 1.0, 1.0, 0.5),
        ~outer=Color.v(1.0, 1.0, 1.0, 0.0),
      ),
      I.fill_path @@ (t => P.ellipse(t, ~cx=rx, ~cy=ry, ~rx=ex, ~ry=ey)),
    ),
  ]);
};

let draw_graph = (x, y, w, h, t) => {
  let samples = [|
    (1.0 +. sin(t *. 1.2345 +. cos(t *. 0.33457) *. 0.44)) *. 0.5,
    (1.0 +. sin(t *. 0.68363 +. cos(t *. 1.3) *. 1.55)) *. 0.5,
    (1.0 +. sin(t *. 1.1642 +. cos(t *. 0.33457) *. 1.24)) *. 0.5,
    (1.0 +. sin(t *. 0.56345 +. cos(t *. 1.63) *. 0.14)) *. 0.5,
    (1.0 +. sin(t *. 1.6245 +. cos(t *. 0.254) *. 0.3)) *. 0.5,
    (1.0 +. sin(t *. 0.345 +. cos(t *. 0.03) *. 0.6)) *. 0.5,
  |];
  let dx = w /. 5.0;
  let sx = i => x +. float(i) *. dx;
  let sy = i => y +. h *. samples[i] *. 0.8;
  I.seq([
    /* Graph background */
    I.paint(
      Paint.linear_gradient(
        ~sx=x,
        ~sy=y,
        ~ex=x,
        ~ey=y +. h,
        ~inner=Color.v(0.00, 0.60, 0.75, 0.00),
        ~outer=Color.v(0.00, 0.60, 0.75, 0.25),
      ),
      I.fill_path @@
      (
        t => {
          P.move_to(t, ~x=sx(0), ~y=sy(0));
          for (i in 1 to 5) {
            P.bezier_to(
              t,
              ~c1x=sx(i - 1) +. dx *. 0.5,
              ~c1y=sy(i - 1),
              ~c2x=sx(i) -. dx *. 0.5,
              ~c2y=sy(i),
              ~x=sx(i),
              ~y=sy(i),
            );
          };
          P.line_to(t, ~x=x +. w, ~y=y +. h);
          P.line_to(t, ~x, ~y=y +. h);
        }
      ),
    ),
    /* Graph line */
    I.paint(
      Paint.color(Color.v(0.0, 0.0, 0.0, 0.125)),
      I.stroke_path(Outline.{...default, stroke_width: 3.0}) @@
      (
        t => {
          P.move_to(t, sx(0), sy(0) +. 2.0);
          for (i in 1 to 5) {
            P.bezier_to(
              t,
              ~c1x=sx(i - 1) +. dx *. 0.5,
              ~c1y=sy(i - 1) +. 2.0,
              ~c2x=sx(i) -. dx *. 0.5,
              ~c2y=sy(i) +. 2.0,
              ~x=sx(i),
              ~y=sy(i) +. 2.0,
            );
          };
        }
      ),
    ),
    I.paint(
      Paint.color(Color.v(0.0, 0.60, 0.75, 1.0)),
      I.stroke_path(Outline.{...default, stroke_width: 3.0}) @@
      (
        t => {
          P.move_to(t, sx(0), sy(0));
          for (i in 1 to 5) {
            P.bezier_to(
              t,
              ~c1x=sx(i - 1) +. dx *. 0.5,
              ~c1y=sy(i - 1),
              ~c2x=sx(i) -. dx *. 0.5,
              ~c2y=sy(i),
              ~x=sx(i),
              ~y=sy(i),
            );
          };
        }
      ),
    ),
    {
      /* Graph sample pos */
      let node = ref(I.empty);
      for (i in 0 to 5) {
        node :=
          I.impose(
            node^,
            I.paint(
              Paint.radial_gradient(
                ~cx=sx(i),
                ~cy=sy(i) +. 2.0,
                ~inr=3.0,
                ~outr=8.0,
                ~inner=Color.v(0.0, 0.0, 0.0, 0.125),
                ~outer=Color.v(0.0, 0.0, 0.0, 0.0),
              ),
              I.fill_path @@
              (
                t =>
                  P.rect(
                    t,
                    ~x=sx(i) -. 10.0,
                    ~y=sy(i) -. 10.0 +. 2.0,
                    ~w=20.0,
                    ~h=20.0,
                  )
              ),
            ),
          );
      };
      node^;
    },
    I.paint(
      Paint.color(Color.v(0.0, 0.6, 0.75, 1.0)),
      I.fill_path @@
      (
        t =>
          for (i in 0 to 5) {
            P.circle(t, ~cx=sx(i), ~cy=sy(i), ~r=4.0);
          }
      ),
    ),
    I.paint(
      Paint.color(Color.v(0.8, 0.8, 0.8, 1.0)),
      I.fill_path @@
      (
        t =>
          for (i in 0 to 5) {
            P.circle(t, ~cx=sx(i), ~cy=sy(i), ~r=2.0);
          }
      ),
    ),
  ]);
};

let draw_spinner = (cx, cy, r, t) => {
  let a0 = 0.0 +. t *. 6.0;
  let a1 = pi +. t *. 6.0;
  let r0 = r;
  let r1 = r *. 0.75;
  let sx = cx +. cos(a0) *. (r0 +. r1) *. 0.5;
  let sy = cy +. sin(a0) *. (r0 +. r1) *. 0.5;
  let ex = cx +. cos(a1) *. (r0 +. r1) *. 0.5;
  let ey = cy +. sin(a1) *. (r0 +. r1) *. 0.5;
  I.paint(
    Paint.linear_gradient(
      ~sx,
      ~sy,
      ~ex,
      ~ey,
      ~inner=Color.v(0.0, 0.0, 0.0, 0.0),
      ~outer=Color.v(0.0, 0.0, 0.0, 0.5),
    ),
    I.fill_path @@
    (
      t => {
        P.arc(t, ~cx, ~cy, ~r=r0, ~a0, ~a1, `CW);
        P.arc(t, ~cx, ~cy, ~r=r1, ~a0=a1, ~a1=a0, `CCW);
        P.close(t);
      }
    ),
  );
};

let draw_colorwheel = (x, y, w, h, t) => {
  let cx = x +. w *. 0.5;
  let cy = y +. h *. 0.5;
  let hue = sin(t *. 0.12);
  let r1 = min(w, h) *. 0.5 -. 5.0;
  let r0 = r1 -. 20.0;
  let aeps = 0.5 /. r1;
  let node = ref(I.empty);
  for (i in 0 to 5) {
    let a0 = float(i) /. 6.0 *. pi *. 2.0 -. aeps;
    let a1 = (float(i) +. 1.0) /. 6.0 *. pi *. 2.0 +. aeps;
    let sx = cx +. cos(a0) *. (r0 +. r1) *. 0.5;
    let sy = cy +. sin(a0) *. (r0 +. r1) *. 0.5;
    let ex = cx +. cos(a1) *. (r0 +. r1) *. 0.5;
    let ey = cy +. sin(a1) *. (r0 +. r1) *. 0.5;
    /*Printf.printf "sx=%f, sy=%f, ex=%f, ey=%f\n%!" sx sy ex ey;*/
    node :=
      I.impose(
        node^,
        I.paint(
          Paint.linear_gradient(
            ~sx,
            ~sy,
            ~ex,
            ~ey,
            ~inner=Color.hsl(a0 /. (2.0 *. pi), 1.0, 0.55),
            ~outer=Color.hsl(a1 /. (2.0 *. pi), 1.0, 0.55),
          ),
          I.fill_path @@
          (
            t => {
              P.arc(t, ~cx, ~cy, ~r=r0, ~a0, ~a1, `CW);
              P.arc(t, ~cx, ~cy, ~r=r1, ~a0=a1, ~a1=a0, `CCW);
              P.close(t);
            }
          ),
        ),
      );
  };
  I.seq([
    node^,
    I.paint(
      Paint.color(Color.v(0.0, 0.0, 0.0, 0.25)),
      I.stroke_path(Outline.{...default, stroke_width: 1.0}) @@
      (
        t => {
          P.circle(t, ~cx, ~cy, ~r=r0 -. 0.5);
          P.circle(t, ~cx, ~cy, ~r=r1 +. 0.5);
        }
      ),
    ),
    /* Selector */
    I.transform(
      Transform.(rotate(hue *. 2.0 *. pi, translation(~x=cx, ~y=cy))),
      I.seq([
        I.paint(
          Paint.color(Color.gray(~a=0.75, 1.0)),
          I.stroke_path(Outline.{...default, stroke_width: 2.0}) @@
          (t => P.rect(t, r0 -. 1.0, -3.0, r1 -. r0 +. 2., 6.0)),
        ),
        I.paint(
          Paint.box_gradient(
            ~x=r0 -. 3.0,
            ~y=-5.0,
            ~w=r1 -. r0 +. 6.0,
            ~h=10.0,
            ~r=2.0,
            ~f=4.0,
            ~inner=Color.gray(~a=0.5, 0.0),
            ~outer=Color.gray(~a=0.0, 0.0),
          ),
          I.fill_path @@
          (
            t => {
              P.rect(
                t,
                ~x=r0 -. 2.0 -. 10.0,
                ~y=(-4.0) -. 10.0,
                ~w=r1 -. r0 +. 4.0 +. 20.0,
                ~h=8.0 +. 20.0,
              );
              P.rect(t, ~x=r0 -. 2.0, ~y=-4.0, ~w=r1 -. r0 +. 4.0, ~h=8.0);
              P.set_winding(t, `HOLE);
            }
          ),
        ),
        {
          /* Center triangle */
          let r = r0 -. 6.0;
          let ax = cos(120.0 /. 180.0 *. pi) *. r;
          let ay = sin(120.0 /. 180.0 *. pi) *. r;
          let bx = cos((-120.0) /. 180.0 *. pi) *. r;
          let by = sin((-120.0) /. 180.0 *. pi) *. r;
          let path =
            Path.make @@
            (
              t => {
                P.move_to(t, r, 0.0);
                P.line_to(t, ax, ay);
                P.line_to(t, bx, by);
                P.close(t);
              }
            );
          /*Printf.printf "sx=%f, sy=%f, ex=%f, ey=%f\n%!" r 0.0 ax ay;*/
          let fill = I.fill(path);
          I.seq([
            I.paint(
              Paint.linear_gradient(
                ~sx=r,
                ~sy=0.0,
                ~ex=ax,
                ~ey=ay,
                ~inner=Color.hsl(hue, 1.0, 0.5),
                ~outer=Color.white,
              ),
              fill,
            ),
            I.paint(
              Paint.linear_gradient(
                ~sx=(r +. ax) *. 0.5,
                ~sy=(0.0 +. ay) *. 0.5,
                ~ex=bx,
                ~ey=by,
                ~inner=Color.gray(~a=0.0, 0.0),
                ~outer=Color.gray(~a=1.0, 0.0),
              ),
              fill,
            ),
            I.paint(
              Paint.color(Color.gray(~a=0.25, 0.0)),
              I.stroke(Outline.default, path),
            ),
            {
              /* Select circle on triangle */
              let ax = cos(120.0 /. 180.0 *. pi) *. r *. 0.3;
              let ay = sin(120.0 /. 180.0 *. pi) *. r *. 0.4;
              I.impose(
                I.paint(
                  Paint.color(Color.gray(~a=0.75, 1.0)),
                  I.stroke_path(Outline.{...default, stroke_width: 2.0}) @@
                  (t => P.circle(t, ~cx=ax, ~cy=ay, ~r=5.0)),
                ),
                I.paint(
                  Paint.radial_gradient(
                    ~cx=ax,
                    ~cy=ay,
                    ~inr=7.0,
                    ~outr=9.0,
                    ~inner=Color.gray(~a=0.25, 0.0),
                    ~outer=Color.gray(~a=0.0, 0.0),
                  ),
                  I.fill_path @@
                  (
                    t => {
                      P.rect(
                        t,
                        ~x=ax -. 20.0,
                        ~y=ay -. 20.0,
                        ~w=40.0,
                        ~h=40.0,
                      );
                      P.circle(t, ~cx=ax, ~cy=ay, ~r=7.0);
                      P.set_winding(t, `HOLE);
                    }
                  ),
                ),
              );
            },
          ]);
        },
      ]),
    ),
  ]);
};

let draw_lines = (x, y, w, _h, t) => {
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
  let node = ref(I.empty);
  for (i in 0 to 2) {
    for (j in 0 to 2) {
      let fx = x +. s *. 0.5 +. float(i * 3 + j) /. 9.0 *. w +. pad;
      let fy = y -. s *. 0.5 +. pad;
      let px = i => fx +. px(i);
      let py = i => fy +. py(i);
      node :=
        I.seq([
          node^,
          I.paint(
            Paint.color(Color.gray(~a=0.625, 0.0)),
            I.stroke_path(
              Outline.{
                ...default,
                stroke_width: s *. 0.3,
                line_cap: caps[i],
                line_join: joins[j],
              },
            ) @@
            (
              t => {
                P.move_to(t, px(0), py(0));
                P.line_to(t, px(1), py(1));
                P.line_to(t, px(2), py(2));
                P.line_to(t, px(3), py(3));
              }
            ),
          ),
          I.paint(
            Paint.color(Color.v(0.0, 0.75, 1.0, 1.0)),
            I.stroke_path(
              Outline.{
                ...default,
                stroke_width: 1.0,
                line_cap: `BUTT,
                line_join: `BEVEL,
              },
            ) @@
            (
              t => {
                P.move_to(t, px(0), py(0));
                P.line_to(t, px(1), py(1));
                P.line_to(t, px(2), py(2));
                P.line_to(t, px(3), py(3));
              }
            ),
          ),
        ]);
    };
  };
  node^;
};

let draw_widths = (x, y, w) => {
  let paint = Paint.color(Color.black);
  let y = ref(y);
  let node = ref(I.empty);
  for (i in 0 to 19) {
    let y' = y^;
    node :=
      I.impose(
        node^,
        I.paint(
          paint,
          I.stroke_path(
            Outline.{...default, stroke_width: (float(i) +. 0.5) /. 10.0},
          ) @@
          (
            t => {
              P.move_to(t, x, y');
              P.line_to(t, x +. w, y' +. w *. 0.3);
            }
          ),
        ),
      );
    y := y^ +. 10.0;
  };
  node^;
};

let draw_caps = (x, y, w) => {
  let width = 8.0;
  let f = (cap, i) =>
    I.paint(
      Paint.black,
      I.stroke_path(Outline.make(~width, ~cap, ())) @@
      (
        t => {
          P.move_to(t, x, y +. float(i * 10 + 5));
          P.line_to(t, x +. w, y +. float(i * 10 + 5));
        }
      ),
    );
  I.seq([
    I.paint(
      Paint.color(Color.gray(~a=0.125, 1.0)),
      I.fill_path @@
      (
        t => {
          P.rect(t, x, y, w, 40.0);
          P.rect(t, x -. width /. 2.0, y, w +. width, 40.0);
        }
      ),
    ),
    f(`BUTT, 0),
    f(`ROUND, 1),
    f(`SQUARE, 2),
  ]);
};

let draw_scissor = (x, y, t) => {
  let xf = Transform.(rotate(5.0 /. 180.0 *. pi, translation(~x, ~y)));
  let shape = I.fill_path @@ (t => P.rect(t, -20.0, -10.0, 60.0, 30.0));
  I.transform(
    xf,
    I.impose(
      /* Draw first rect and set scissor to it's area. */
      I.paint(
        Paint.color(Color.v(1.0, 0.0, 0.0, 1.0)),
        I.fill_path @@ (t => P.rect(t, -20.0, -20.0, 60.0, 40.0)),
      ),
      {
        /* Draw second rectangle with offset and rotation. */
        /*let frame = Frame.set_scissor (-20.0) (-20.0) 60.0 40.0 Transform.identity frame in*/
        let xf = Transform.(rotate(t, translation(40.0, 0.0)));
        I.impose(
          /* Draw the intended second rectangle without any scissoring. */
          I.transform(
            xf,
            I.paint(Paint.color(Color.v(1.0, 0.5, 0.0, 0.25)), shape),
          ),
          /* Draw second rectangle with combined scissoring. */
          I.scissor(
            b2(-20.0, -20.0, 60.0, 40.0),
            I.transform(
              xf,
              I.intersect_scissor(
                b2(-20.0, -10.0, 60.0, 30.0),
                I.paint(Paint.color(Color.v(1.0, 0.5, 0.0, 1.0)), shape),
              ),
            ),
          ),
        );
      },
    ),
  );
};

let draw_window = (title, x, y, w, h) => {
  let cornerRadius = 3.0;
  let font = Lazy.force(font_sans_bold);
  I.seq([
    /* Window */
    I.paint(
      Paint.color(Color.v(0.110, 0.118, 0.133, 0.75)),
      I.fill_path @@
      (
        t => P.round_rect'(t, x, y, w, h, cornerRadius, cornerRadius, 0.0, 0.0)
      ),
    ),
    /* Drop shadow */
    I.paint(
      Paint.box_gradient(
        x,
        y +. 2.0,
        w,
        h,
        cornerRadius *. 2.0,
        10.0,
        Color.gray(~a=0.5, 0.0),
        Color.gray(~a=0.0, 0.0),
      ),
      I.fill_path @@
      (
        t => {
          P.rect(t, x -. 10.0, y -. 10.0, w +. 20.0, h +. 30.0);
          P.round_rect'(t, x, y, w, h, cornerRadius, cornerRadius, 0.0, 0.0);
          P.set_winding(t, `HOLE);
        }
      ),
    ),
    /* Header */
    I.paint(
      Paint.linear_gradient(
        x,
        y,
        x,
        y +. 15.0,
        Color.gray(~a=0.04, 1.0),
        Color.gray(~a=0.08, 1.0),
      ),
      I.fill_path @@
      (
        t =>
          P.round_rect'(
            t,
            x +. 1.0,
            y +. 1.0,
            w -. 2.0,
            30.0,
            cornerRadius -. 1.0,
            cornerRadius -. 1.0,
            0.0,
            0.0,
          )
      ),
    ),
    I.paint(
      Paint.color(Color.gray(~a=0.125, 0.0)),
      I.stroke_path(Outline.default) @@
      (
        t => {
          P.move_to(t, x +. 0.5, y +. 0.5 +. 30.0);
          P.line_to(t, x +. 0.5 +. w -. 1.0, y +. 0.5 +. 30.0);
        }
      ),
    ),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.6, 0.9)),
      Text.(
        simple_text(
          Font.make(~blur=2.0, ~size=18.0, font),
          ~valign=`MIDDLE,
          ~halign=`CENTER,
          ~x=x +. w /. 2.,
          ~y=y +. 16. +. 1.0,
          title,
        )
      ),
    ),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.6, 0.9)),
      Text.(
        simple_text(
          Font.make(~size=18.0, font),
          ~valign=`MIDDLE,
          ~halign=`CENTER,
          ~x=x +. w /. 2.,
          ~y=y +. 16.,
          title,
        )
      ),
    ),
  ]);
};

let draw_searchbox = (text, x, y, w, h) => {
  let cornerRadius = h /. 2.0 -. 1.0;
  /* Edit */
  I.seq([
    I.paint(
      Paint.box_gradient(
        x,
        y +. 1.5,
        w,
        h,
        h /. 2.0,
        5.0,
        Color.gray(~a=0.08, 0.0),
        Color.gray(~a=0.375, 0.0),
      ),
      I.fill_path @@ (t => P.round_rect(t, x, y, w, h, cornerRadius)),
    ),
    I.paint(
      Paint.color(Color.gray(~a=0.2, 0.0)),
      I.stroke_path(Outline.default) @@
      (
        t =>
          P.round_rect(
            t,
            x +. 0.5,
            y +. 0.5,
            w -. 1.0,
            h -. 1.0,
            cornerRadius -. 0.5,
          )
      ),
    ),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.25, 1.0)),
      Text.(
        simple_text(
          Font.make(~size=h *. 1.3, Lazy.force(font_icons)),
          ~valign=`MIDDLE,
          ~halign=`CENTER,
          ~x=x +. h *. 0.55,
          ~y=y +. h *. 0.55,
          "\240\159\148\141",
        )
      ),
    ),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.125, 1.0)),
      Text.(
        simple_text(
          Font.make(~size=20.0, Lazy.force(font_sans)),
          ~valign=`MIDDLE,
          ~halign=`LEFT,
          ~x=x +. h *. 1.05,
          ~y=y +. h *. 0.5,
          text,
        )
      ),
    ),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.125, 1.0)),
      Text.(
        simple_text(
          Font.make(~size=h *. 1.3, Lazy.force(font_icons)),
          ~valign=`MIDDLE,
          ~halign=`CENTER,
          ~x=x +. w -. h *. 0.55,
          ~y=y +. h *. 0.55,
          "\226\156\150",
        )
      ),
    ),
  ]);
};

let draw_dropdown = (text, x, y, w, h) => {
  let cornerRadius = 4.0;
  I.seq([
    I.paint(
      Paint.linear_gradient(
        x,
        y,
        x,
        y +. h,
        Color.gray(~a=0.08, 1.0),
        Color.gray(~a=0.08, 0.0),
      ),
      I.fill_path @@
      (
        t =>
          P.round_rect(
            t,
            x +. 1.0,
            y +. 1.0,
            w -. 2.0,
            h -. 2.0,
            cornerRadius -. 1.0,
          )
      ),
    ),
    I.paint(
      Paint.color(Color.gray(~a=0.1875, 0.0)),
      I.stroke_path(Outline.default) @@
      (
        t =>
          P.round_rect(
            t,
            x +. 0.5,
            y +. 0.5,
            w -. 1.0,
            h -. 1.0,
            cornerRadius -. 0.5,
          )
      ),
    ),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.8, 1.0)),
      Text.(
        simple_text(
          Font.make(~size=20.0, Lazy.force(font_sans)),
          ~valign=`MIDDLE,
          ~halign=`LEFT,
          ~x=x +. h *. 0.3,
          ~y=y +. h *. 0.5,
          text,
        )
      ),
    ),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.8, 1.0)),
      Text.(
        simple_text(
          Font.make(~size=h *. 1.3, Lazy.force(font_icons)),
          ~valign=`MIDDLE,
          ~halign=`CENTER,
          ~x=x +. w -. h *. 0.5,
          ~y=y +. h *. 0.5,
          "\238\157\158 ",
        )
      ),
    ),
  ]);
};

let draw_label = (text, x, y, w, h) =>
  I.paint(
    Paint.color(Gg.Color.gray(~a=0.5, 1.0)),
    Text.(
      simple_text(
        Font.make(~size=18.0, Lazy.force(font_sans)),
        ~valign=`MIDDLE,
        ~halign=`LEFT,
        ~x,
        ~y=y +. h *. 0.5,
        text,
      )
    ),
  );

let draw_editboxbase = (x, y, w, h) =>
  I.impose(
    I.paint(
      Paint.box_gradient(
        x +. 1.0,
        y +. 1.0 +. 1.5,
        w -. 2.0,
        h -. 2.0,
        3.0,
        4.0,
        Color.gray(~a=0.125, 1.0),
        Color.gray(~a=0.125, 0.125),
      ),
      I.fill_path @@
      (
        t =>
          P.round_rect(t, x +. 1.0, y +. 1.0, w -. 2.0, h -. 2.0, 4.0 -. 1.0)
      ),
    ),
    I.paint(
      Paint.color(Color.gray(~a=0.1875, 0.0)),
      I.stroke_path(Outline.default) @@
      (
        t =>
          P.round_rect(t, x +. 0.5, y +. 0.5, w -. 1.0, h -. 1.0, 4.0 -. 0.5)
      ),
    ),
  );

let draw_editbox = (text, x, y, w, h) =>
  I.impose(
    draw_editboxbase(x, y, w, h),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.25, 1.0)),
      Text.(
        simple_text(
          Font.make(~size=20.0, Lazy.force(font_sans)),
          ~valign=`MIDDLE,
          ~halign=`LEFT,
          ~x=x +. h *. 0.3,
          ~y=y +. h *. 0.5,
          text,
        )
      ),
    ),
  );

let draw_editboxnum = (text, units, x, y, w, h) => {
  let ufont = Text.Font.make(~size=18.0, Lazy.force(font_sans));
  let uw = Text.Font.text_width(ufont, units);
  I.seq([
    draw_editboxbase(x, y, w, h),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.25, 1.0)),
      Text.(
        simple_text(
          ~valign=`MIDDLE,
          ufont,
          ~halign=`RIGHT,
          ~x=x +. w -. h *. 0.3,
          ~y=y +. h *. 0.5,
          units,
        )
      ),
    ),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.5, 1.0)),
      Text.(
        simple_text(
          Font.make(~size=20.0, Lazy.force(font_sans)),
          ~valign=`MIDDLE,
          ~halign=`RIGHT,
          ~x=x +. w -. uw -. h *. 0.5,
          ~y=y +. h *. 0.5,
          text,
        )
      ),
    ),
  ]);
};

let draw_checkbox = (text, x, y, w, h) =>
  I.seq([
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.66, 1.0)),
      Text.(
        simple_text(
          Font.make(~size=18.0, Lazy.force(font_sans)),
          ~valign=`MIDDLE,
          ~x=x +. 28.,
          ~y=y +. h *. 0.5,
          text,
        )
      ),
    ),
    I.paint(
      Paint.box_gradient(
        x +. 1.0,
        y +. floor(h /. 2.0) -. 9.0 +. 1.0,
        18.0,
        18.0,
        3.0,
        3.0,
        Color.gray(~a=0.125, 0.0),
        Color.gray(~a=0.375, 0.0),
      ),
      I.fill_path @@
      (
        t =>
          P.round_rect(
            t,
            x +. 1.0,
            y +. floor(h /. 2.0) -. 9.0,
            18.0,
            18.0,
            3.0,
          )
      ),
    ),
    I.paint(
      Paint.color(Gg.Color.gray(~a=0.5, 1.0)),
      Text.(
        simple_text(
          Font.make(~size=40.0, Lazy.force(font_icons)),
          ~valign=`MIDDLE,
          ~halign=`CENTER,
          ~x=x +. 11.,
          ~y=y +. h *. 0.5,
          "\226\156\147",
        )
      ),
    ),
  ]);

let cp_to_utf8 = cp => {
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
  try (
    {
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
    }
  ) {
  | exn =>
    prerr_endline("cp: " ++ string_of_int(cp^));
    raise(exn);
  };
  Bytes.to_string(str);
};

let draw_button = (preicon, text, x, y, w, h, col) => {
  let is_black = Color.a(col) > 0.0;
  let cornerRadius = 4.0;
  let shape =
    I.fill_path @@
    (
      t =>
        P.round_rect(
          t,
          x +. 1.0,
          y +. 1.0,
          w -. 2.0,
          h -. 2.0,
          cornerRadius -. 1.0,
        )
    );
  I.seq([
    if (is_black) {
      I.paint(Paint.color(col), shape);
    } else {
      I.empty;
    },
    I.paint(
      Paint.linear_gradient(
        x,
        y,
        x,
        y +. h,
        Color.gray(1.0, ~a=if (is_black) {0.125} else {0.25}),
        Color.gray(0.0, ~a=if (is_black) {0.125} else {0.25}),
      ),
      shape,
    ),
    I.paint(
      Paint.color(Color.gray(~a=0.375, 0.0)),
      I.stroke_path(Outline.default) @@
      (
        t =>
          P.round_rect(
            t,
            x +. 0.5,
            y +. 0.5,
            w -. 1.0,
            h -. 1.0,
            cornerRadius -. 0.5,
          )
      ),
    ),
    {
      let font = Text.Font.make(~size=20.0, Lazy.force(font_sans_bold));
      let tw = Text.Font.text_width(font, text);
      let (base, iw) =
        if (preicon == 0) {
          (I.empty, 0.0);
        } else {
          let font = Text.Font.make(~size=h *. 1.3, Lazy.force(font_icons));
          let icon = cp_to_utf8(preicon);
          let iw = Text.Font.text_width(font, icon);
          (
            I.paint(
              Paint.color(Gg.Color.gray(~a=0.40, 1.0)),
              Text.(
                simple_text(
                  font,
                  icon,
                  ~halign=`LEFT,
                  ~valign=`MIDDLE,
                  ~x=x +. w *. 0.5 -. tw *. 0.5 -. iw *. 0.75,
                  ~y=y +. h *. 0.5,
                )
              ),
            ),
            iw,
          );
        };
      I.seq([
        base,
        I.paint(
          Paint.color(Gg.Color.gray(~a=0.66, 0.0)),
          Text.(
            simple_text(
              font,
              text,
              ~valign=`MIDDLE,
              ~halign=`LEFT,
              ~x=x +. w *. 0.5 -. tw *. 0.5 +. iw *. 0.25,
              ~y=y +. h *. 0.5 -. 0.5,
            )
          ),
        ),
        I.paint(
          Paint.color(Gg.Color.gray(~a=0.66, 1.0)),
          Text.(
            simple_text(
              font,
              text,
              ~valign=`MIDDLE,
              ~halign=`LEFT,
              ~x=x +. w *. 0.5 -. tw *. 0.5 +. iw *. 0.25,
              ~y=y +. h *. 0.5,
            )
          ),
        ),
      ]);
    },
  ]);
};

let draw_slider = (pos, x, y, w, h) => {
  let cy = y +. floor(h *. 0.5);
  let kr = floor(h *. 0.25);
  I.seq([
    /* Slot */
    I.paint(
      Paint.box_gradient(
        x,
        cy -. 2.0 +. 1.0,
        w,
        4.0,
        2.0,
        2.0,
        Color.gray(~a=0.125, 0.0),
        Color.gray(~a=0.5, 0.0),
      ),
      I.fill_path @@ (t => P.round_rect(t, x, cy -. 2., w, 4.0, 2.0)),
    ),
    /* Knob Shadow */
    I.paint(
      Paint.radial_gradient(
        x +. floor(pos *. w),
        cy +. 1.0,
        kr -. 3.0,
        kr +. 3.0,
        Color.gray(~a=0.25, 0.0),
        Color.gray(~a=0.0, 0.0),
      ),
      I.fill_path @@
      (
        t => {
          P.rect(
            t,
            x +. floor(pos *. w) -. kr -. 5.0,
            cy -. kr -. 5.0,
            kr *. 2.0 +. 5.0 +. 5.0,
            kr *. 2.0 +. 5.0 +. 5.0 +. 3.0,
          );
          P.circle(t, x +. floor(pos *. w), cy, kr);
          P.set_winding(t, `HOLE);
        }
      ),
    ),
    {
      /* Knob */
      let shape =
        I.fill_path @@
        (t => P.circle(t, x +. floor(pos *. w), cy, kr -. 1.0));
      I.seq([
        I.paint(Paint.color(Color.v_srgbi(40, 43, 48)), shape),
        I.paint(
          Paint.linear_gradient(
            x,
            cy -. kr,
            x,
            cy +. kr,
            Color.gray(~a=0.0625, 1.0),
            Color.gray(~a=0.0625, 0.0),
          ),
          shape,
        ),
        I.paint(
          Paint.color(Color.gray(~a=0.375, 0.0)),
          I.stroke_path(Outline.default) @@
          (t => P.circle(t, x +. floor(pos *. w), cy, kr -. 0.5)),
        ),
      ]);
    },
  ]);
};

let image_size = image => (Texture.width(image), Texture.height(image));

let image_texture = image => image;

let load_demo_data = () =>
  Array.init(
    12,
    i => {
      let name = Printf.sprintf("images/image%d.jpg", i + 1);
      switch (Texture.load_image(~alpha=false, ~name, name)) {
      | Result.Ok(image) => image
      | Result.Error(`Msg(msg)) =>
        Printf.eprintf("error loading %s: %s\n%!", name, msg);
        exit(1);
      };
    },
  );

let draw_thumbnails = (x, y, w, h, images, t) => {
  let cornerRadius = 3.0
  and thumb = 60.0
  and arry = 30.5;
  let stackh = float(Array.length(images) / 2) *. (thumb +. 10.0) +. 10.0;
  let u = (1.0 +. cos(t *. 0.5)) *. 0.5;
  let u2 = (1.0 -. cos(t *. 0.2)) *. 0.5;
  I.seq([
    /* Drop shadow */
    I.paint(
      Paint.box_gradient(
        x,
        y +. 4.0,
        w,
        h,
        cornerRadius *. 2.0,
        20.0,
        Color.gray(~a=0.5, 0.0),
        Color.gray(~a=0.0, 0.0),
      ),
      I.fill_path @@
      (
        t => {
          P.rect(t, x -. 10.0, y -. 10.0, w +. 20.0, h +. 30.0);
          P.round_rect(t, x, y, w, h, cornerRadius);
          P.set_winding(t, `HOLE);
        }
      ),
    ),
    /* Window */
    I.paint(
      Paint.color(Color.gray(0.8)),
      I.fill_path @@
      (
        t => {
          P.round_rect(t, x, y, w, h, cornerRadius);
          P.move_to(t, x -. 10.0, y +. arry);
          P.line_to(t, x +. 1.0, y +. arry -. 11.0);
          P.line_to(t, x +. 1.0, y +. arry +. 11.0);
        }
      ),
    ),
    {
      let xf' = Transform.translation(0.0, -. (stackh -. h) *. u);
      let dv = 1.0 /. float(Array.length(images) - 1);
      let acc = ref(I.empty);
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
            acc :=
              I.impose(
                acc^,
                draw_spinner(
                  tx +. thumb /. 2.0,
                  ty +. thumb /. 2.0,
                  thumb *. 0.25,
                  t,
                ),
              );
          };
          acc :=
            I.seq([
              acc^,
              I.paint(
                Paint.image_pattern(
                  Gg.P2.v(tx +. ix, ty +. iy),
                  Gg.Size2.v(iw, ih),
                  0.0,
                  a,
                  image_texture(image),
                ),
                I.fill_path @@
                (t => P.round_rect(t, tx, ty, thumb, thumb, 5.0)),
              ),
              I.paint(
                Paint.box_gradient(
                  tx -. 1.0,
                  ty,
                  thumb +. 2.0,
                  thumb +. 2.0,
                  5.0,
                  3.0,
                  Color.gray(~a=0.5, 0.0),
                  Color.gray(~a=0.0, 0.0),
                ),
                I.fill_path @@
                (
                  t => {
                    P.rect(
                      t,
                      tx -. 5.0,
                      ty -. 5.0,
                      thumb +. 10.0,
                      thumb +. 10.0,
                    );
                    P.round_rect(t, tx, ty, thumb, thumb, 6.0);
                    P.set_winding(t, `HOLE);
                  }
                ),
              ),
              I.paint(
                Paint.color(Color.gray(~a=0.75, 1.0)),
                I.stroke_path(Outline.{...default, stroke_width: 1.0}) @@
                (
                  t =>
                    P.round_rect(
                      t,
                      tx +. 0.5,
                      ty +. 0.5,
                      thumb -. 1.0,
                      thumb -. 1.0,
                      4.0 -. 0.5,
                    )
                ),
              ),
            ]);
        },
        images,
      );
      I.scissor(b2(x, y, w, h), I.transform(xf', acc^));
    },
    /* Hide fades */
    I.paint(
      Paint.linear_gradient(
        x,
        y,
        x,
        y +. 6.0,
        Color.gray(~a=1.0, 0.8),
        Color.gray(~a=0.0, 0.8),
      ),
      I.fill_path @@ (t => P.rect(t, x +. 4.0, y, w -. 8.0, 6.0)),
    ),
    I.paint(
      Paint.linear_gradient(
        x,
        y +. h -. 6.0,
        x,
        y +. 6.0,
        Color.gray(~a=1.0, 0.8),
        Color.gray(~a=0.0, 0.8),
      ),
      I.fill_path @@ (t => P.rect(t, x +. 4.0, y +. h -. 6.0, w -. 8.0, 6.0)),
    ),
    /* Scroll bar */
    I.paint(
      Paint.box_gradient(
        x +. w -. 12.0 +. 1.0,
        y +. 4.0 +. 1.0,
        8.0,
        h -. 8.0,
        3.0,
        4.0,
        Color.gray(~a=0.125, 0.0),
        Color.gray(~a=0.375, 0.0),
      ),
      I.fill_path @@
      (t => P.round_rect(t, x +. w -. 12.0, y +. 4.0, 8.0, h -. 8.0, 3.0)),
    ),
    {
      let scrollh = h /. stackh *. (h -. 8.0);
      I.paint(
        Paint.box_gradient(
          x +. w -. 12. -. 1.,
          y +. 4. +. (h -. 8. -. scrollh) *. u -. 1.,
          8.,
          scrollh,
          3.,
          4.,
          Color.gray(~a=0.9, 1.0),
          Color.gray(~a=0.5, 1.0),
        ),
        I.fill_path @@
        (
          t =>
            P.round_rect(
              t,
              x +. w -. 12. +. 1.,
              y +. 4. +. 1. +. (h -. 8. -. scrollh) *. u,
              8. -. 2.,
              scrollh -. 2.,
              2.,
            )
        ),
      );
    },
  ]);
};

let images = lazy (load_demo_data());

let draw_demo = (mx, my, w, h, t) => {
  let node = ref(I.empty);
  let push = n => node := I.impose(node^, n);
  push @@ draw_eyes(w -. 250.0, 50.0, 150.0, 100.0, mx, my, t);
  push @@ draw_graph(0.0, h /. 2.0, w, h /. 2.0, t);
  push @@ draw_colorwheel(w -. 300.0, h -. 300.0, 250.0, 250.0, t);
  push @@ draw_lines(120.0, h -. 50.0, 600.0, 50.0, t);
  push @@ draw_widths(10.0, 50.0, 30.0);
  push @@ draw_caps(10.0, 300.0, 30.0);
  push @@ draw_scissor(50.0, h -. 80.0, t);
  /* Widgets */
  push @@ draw_window("Widgets `n Stuff", 50.0, 50.0, 300.0, 400.0);
  let x = 60.0
  and y = 95.0;
  push @@ draw_searchbox("Search", x, y, 280.0, 25.0);
  let y = y +. 40.0;
  push @@ draw_dropdown("Effects", x, y, 280.0, 28.0);
  let popy = y +. 14.0;
  let y = y +. 45.0;
  /* Form */
  push @@ draw_label("login", x, y, 280.0, 20.0);
  let y = y +. 25.0;
  push @@ draw_editbox("Email", x, y, 280.0, 28.0);
  let y = y +. 35.0;
  push @@ draw_editbox("Password", x, y, 280.0, 28.0);
  let y = y +. 38.0;
  push @@ draw_checkbox("Remember me", x, y, 140.0, 28.0);
  push @@
  draw_button(
    /*ICON_LOGIN*/ 0xE740,
    "Sign in",
    x +. 138.0,
    y,
    140.0,
    28.0,
    Color.v(0.0, 0.375, 0.5, 1.0),
  );
  let y = y +. 45.0;
  /* Slider */
  push @@ draw_label("Diameter", x, y, 280.0, 20.0);
  let y = y +. 25.0;
  push @@ draw_editboxnum("123.00", "px", x +. 180.0, y, 100.0, 28.0);
  push @@ draw_slider(0.4, x, y, 170.0, 28.0);
  let y = y +. 55.0;
  push @@
  draw_button(
    /*ICON_TRASH*/ 0xE729,
    "Delete",
    x,
    y,
    160.0,
    28.0,
    Color.v(0.5, 0.0625, 0.03125, 1.0),
  );
  push @@
  draw_button(
    0,
    "Cancel",
    x +. 170.0,
    y,
    110.0,
    28.0,
    Color.gray(~a=0.0, 0.0),
  );
  push @@
  draw_thumbnails(365.0, popy -. 30.0, 160.0, 300.0, Lazy.force(images), t);
  node^;
};

let w = 1000;

let h = 600;

let f =
  try (float_of_string(Sys.argv[1])) {
  | _ => 1.0
  };

let fw = int_of_float(f *. float(w));

let fh = int_of_float(f *. float(h));

let render = (context, sw, sh, t) => {
  let lw = float(w);
  let lh = float(h);
  let width = lw *. f *. sw;
  let height = lh *. f *. sh;
  let (_, (x, y)) = Sdl.get_mouse_state();
  let x = float(x) /. f
  and y = float(y) /. f;
  let demo = draw_demo(x, y, lw, lh, t);
  Renderer.render(
    context,
    ~width,
    ~height,
    I.seq([
      I.transform(Transform.scale(sw *. f /. 2.0, sh *. f), demo),
      I.transform(Transform.scale(sw *. f /. 1.8, sh *. f), demo),
      I.transform(Transform.scale(sw *. f /. 1.6, sh *. f), demo),
      I.transform(Transform.scale(sw *. f /. 1.4, sh *. f), demo),
      I.transform(Transform.scale(sw *. f /. 1.2, sh *. f), demo),
      I.transform(Transform.scale(sw *. f, sh *. f), demo),
    ]),
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
        let context = Renderer.create(~antialias=true, ());
        let quit = ref(false);
        let event = Sdl.Event.create();
        while (! quit^) {
          while (Sdl.poll_event(Some(event))) {
            switch (Sdl.Event.enum(Sdl.Event.get(event, Sdl.Event.typ))) {
            | `Quit => quit := true
            | _ => ()
            };
          };
          Gl.viewport(0, 0, ow, oh);
          Gl.clear_color(0.3, 0.3, 0.32, 1.0);
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
          render(context, sw, sh, Int32.to_float(Sdl.get_ticks()) /. 1000.0);
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
