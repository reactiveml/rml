
module Graphics

open System.Threading
open System.Drawing
open System.Windows
open System.Windows.Forms

let def_height = 600
let def_width = 800
let is_synchronized = ref false

exception Window_not_opened

(* colors *)
let rgb r g b = 
  let c = Color.FromArgb(r,g,b) in
    new Pen(c), new SolidBrush(c)

let green = Pens.Green, Brushes.Green
let blue = Pens.Green, Brushes.Blue
let red = Pens.Green, Brushes.Red
let cyan = Pens.Green, Brushes.Cyan
let black = Pens.Green, Brushes.Black
let magenta = Pens.Green, Brushes.Magenta
let yellow = Pens.Green, Brushes.Yellow

let br = ref blue

let set_color c =
  br := c

(*forms*)
let mouse_is_down = ref false
let click_pos = ref (-1, -1)
let key_value = ref 'c'
let key_was_pressed = ref false
let window_closed = ref false

type MyForm() =
 class
  inherit Form()

  override this.OnMouseDown e = 
    base.OnMouseDown e
    click_pos := e.X, e.Y
    mouse_is_down := true

  override this.OnMouseUp e =
    base.OnMouseUp e 
    mouse_is_down := false

  override this.OnKeyPress e =
    base.OnKeyPress e
    key_value := e.KeyChar
    key_was_pressed := true

  override this.OnClosing e =
    e.Cancel <- true
    (*base.OnClosed e
    Application.Exit () *)
 end

let form_opt = ref None
let buff = ref None

let assert_form () =
  match !form_opt with
  | None -> raise Window_not_opened
  | Some form -> form

let assert_graphics () =
  match !buff with
  | None -> raise Window_not_opened
  | Some b -> b

let open_graph s =
  let form = new MyForm()
  form.Width <- def_width
  form.Height <- def_height
  form_opt := Some form
  let currentContext = BufferedGraphicsManager.Current
  buff := Some (currentContext.Allocate(form.CreateGraphics(), form.DisplayRectangle))
  form.Show ()

let close_graph () =
  match !form_opt with
  | None -> raise Window_not_opened
  | Some form -> ()

let size_x () =
  match !form_opt with
  | None -> raise Window_not_opened
  | Some form -> form.Width

let size_y () =
  match !form_opt with
  | None -> raise Window_not_opened
  | Some form -> form.Height

let auto_synchronize b =
  is_synchronized := b

let synchronize () =
  let buf = assert_graphics ()
  let f = assert_form ()
  Monitor.Enter buf
  buf.Render (f.CreateGraphics ())
  Monitor.Exit buf
  Application.DoEvents()

let clear_graph () =
  let buf = assert_graphics ()
  let f = assert_form ()
  Monitor.Enter buf
  buf.Graphics.FillRectangle(Brushes.White, buf.Graphics.ClipBounds)
  (*buf.Graphics.Clear f.BackColor*)
  if !is_synchronized then
    buf.Render () 
  Monitor.Exit buf

let button_down () =
  !mouse_is_down

let key_pressed () =
  !key_was_pressed

let read_key () =
  key_was_pressed := false
  !key_value

let mouse_pos () =
  !click_pos

let fill_rect (x:int) y w h =
  let buf = assert_graphics ()
  Monitor.Enter buf
  buf.Graphics.FillRectangle(snd !br, x, y, w, h)
  if !is_synchronized then
    buf.Render ()
  Monitor.Exit buf

let draw_circle (x:int) y r =
  let buf = assert_graphics ()
  Monitor.Enter buf
  buf.Graphics.DrawEllipse(fst !br, x - r, y - r, 2 * r, 2 * r)
  if !is_synchronized then
    buf.Render ()
  Monitor.Exit buf  

let fill_circle (x:int) y r =
  let buf = assert_graphics ()
  Monitor.Enter buf
  buf.Graphics.FillEllipse(snd !br, x - r, y - r, 2 * r, 2 * r)
  if !is_synchronized then
    buf.Render ()
  Monitor.Exit buf  


let current_pos = ref (0,0)

let moveto x y = 
  current_pos := x,y

let lineto x y =
  let buf = assert_graphics ()
  Monitor.Enter buf
  buf.Graphics.DrawLine(fst !br, fst !current_pos, snd !current_pos, x, y)
  if !is_synchronized then
    buf.Render ()
  Monitor.Exit buf;
  moveto x y