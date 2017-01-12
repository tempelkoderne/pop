open System.Windows.Forms
open System.Drawing

type coord = (int * int)
type path = coord list
type planet = (Color * int)
// type planets = planet * path list

// create world
let bigBang backColor (width, height) title draw =
    let win = new Form ()
    win.Text <- title
    win.BackColor <- backColor
    win.ClientSize <- Size (width, height)
    win.Paint.Add draw
    win

// draw planets
let drawPlanet (pos:path byref) (pl:planet) (e:PaintEventArgs) =
    let x, y = pos.[0]
    let color, radius = pl
    let brush = new SolidBrush (color)
    let shape = new Rectangle(Point(x, y),
                              Size(radius, radius))
    e.Graphics.FillEllipse (brush, shape)

// god config
let mutable pos0:path = [(100,100);
                          (200,100);
                          (200,200);
                          (100,200);
                          (100,100)]
let earth:planet = (Color.Blue, 30)
let size:int*int = (400,400)
let bcolor:Color = Color.Black
let title:string = "Solar System"
let solar = bigBang bcolor size title (drawPlanet &pos0 earth)

// position updates
let updateWorld (world:Form) (pos:path byref) showtime =
    pos <- [(1 + (fst (pos.[0])), (1 + snd (pos.[0])))]
    // pos <- calcPos(iniCoor 1.0) // calcPos under construction
    world.Refresh()

// setup timer
let timer = new Timer(Interval = 100,
                      Enabled = true)
// timer.Tick.Add (fun showtime -> updateWorld solar &pos1)                
timer.Tick.Add (updateWorld solar &pos0)

// run system
Application.Run solar
