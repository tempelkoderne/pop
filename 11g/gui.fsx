open System.Windows.Forms
open System.Drawing

type coord = (float * float)
type path = coord list
type planet = (Color * int)
// type planets = planet * path list

// convert to pixel coords
let toPx (x:float) = log10(x + 1.0) * 100.0
let ofPx (x:float) = 10.0**(x/100.0)-1.0

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
    let x, y = pos
    // let trans_x, trans_y = toPx x + 300.0, toPx y + 300.0
    let color, radius = pl
    let brush = new SolidBrush (color)
    let shape = new Rectangle(Point(int (round x), int (round y)),
                              Size(radius, radius))
    e.Graphics.FillEllipse (brush, shape)

// god config
let mutable pos0 = (100.0 + 0.2804392026, 100.0 + 0.1643945659)
let mutable v0 = (-0.02091908706, 0.0247536421)
let mutable a0 = (-0.002415813724, -0.001416159527)
// 8.530856837, -31.87831479

// let mutable pos0:path = [(100,100);
//                           (200,100);
//                           (200,200);
//                           (100,200);
//                           (100,100)]
let earth:planet = (Color.Blue, 30)
let size:(int*int) = (700,700)
let bcolor:Color = Color.Black
let title:string = "Solar System"
let solar = bigBang bcolor size title (drawPlanet &pos0 earth)

// position updates

let updateWorld (world:Form) (pos:coord byref) (v:coord byref) (a:coord byref) showtime =
    // pos <- [(1 + (fst (pos.[0])), (1 + snd (pos.[0])))]
    let state = CNPP [|pos; v; a|]
    pos <- state.[0]
    v <- state.[1]
    a <- state.[2]
    // pos <- calcPos(iniCoor 1.0) // calcPos under construction
    world.Refresh()


// setup timer
let timer = new Timer(Interval = 100,
                      Enabled = true)
// timer.Tick.Add (fun showtime -> updateWorld solar &pos1)                
timer.Tick.Add (updateWorld solar &pos0 &v0 &a0)

// run system
Application.Run solar


