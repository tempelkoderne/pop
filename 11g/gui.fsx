open System.Windows.Forms
open System.Drawing

type coord = (float * float)
type path = coord list
type planet = (Color * int)
// type planets = planet * path list

// convert to pixel coords
let toPx (x:coord) = (log10((fst x) + 100.0) * 100.0, log10((snd x) + 100.0) * 100.0)
let ofPx (x:coord) = (10.0**((fst x)/100.0) - 100.0, 10.0**((fst x)/100.0) - 100.0)

let scaleUp (x:coord) = (100.0 * (fst x), 100.0 * (snd x))
let scaleDown (x:coord) = ((fst x)/100.0, (fst x)/100.0)

let offset (r:int) (x:coord) = ((float r) + (fst x), (float r) + (snd x))

// compute next position
let CNPP (data : coord []) =
    let GMs = 2.959122082322128e-4
    let ( .+ ) x y = ((fst x + fst y),(snd x + snd y))
    let ( .* ) x y = (x*(fst y),x*(snd y))
    let r0 = data.[0]
    let v0 = data.[1]
    let a0 = data.[2]
    let r1 = (r0 .+ v0)
    let r0Len = sqrt (((fst r0)**2.0) + ((snd r0)**2.0))
    let a1 = -(GMs/(r0Len**3.0)) .* r0
    let v1 = (v0 .+ a0)
    let positions = [|r1;v1;a1|]
    positions

// create world
let bigBang backColor (width, height) title draw =
    let win = new Form ()
    win.Text <- title
    win.BackColor <- backColor
    win.ClientSize <- Size (width, height)
    win.Paint.Add draw
    win

// draw planets
let drawPlanet (pos:coord byref) (pl:planet) (e:PaintEventArgs) =
    let x, y = toPx pos
    // let trans_x, trans_y = toPx x + 300.0, toPx y + 300.0
    let color, radius = pl
    let brush = new SolidBrush (color)
    let shape = new Rectangle(Point(int (round x), int (round y)),
                              Size(radius, radius))
    e.Graphics.FillEllipse (brush, shape)

// god config
let mutable pos0 = scaleUp (-0.1666759033, 0.969084551)
let mutable v0 = scaleUp (-0.01720978776, -0.003125375992)
let mutable a0 = scaleUp (5.187516652e-05, -0.0003016118194)

let earth:planet = (Color.Blue, 50)
let size:(int*int) = (700,700)
let bcolor:Color = Color.Black
let title:string = "Solar System"
let solar = bigBang bcolor size title (drawPlanet &pos0 earth)

// move planets
let updateWorld (world:Form) (pos:coord byref) (v:coord byref) (a:coord byref) showtime =
    let state = CNPP [|scaleDown pos; scaleDown v; scaleDown a|]
    pos <- scaleUp state.[0]
    v <- scaleUp state.[1]
    a <- scaleUp state.[2]
    printfn "%A" state
    world.Refresh()


// setup timer
let timer = new Timer(Interval = 10,
                      Enabled = true)
timer.Tick.Add (updateWorld solar &pos0 &v0 &a0)

// run system
Application.Run solar


