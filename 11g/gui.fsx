open System.Windows.Forms
open System.Drawing

// define types
type coord = (int * int)
type path = coord list
type planet = (Color * int)
// type planets = planet * point list


(***** BUILD SOLAR SYSTEM *****)

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
    let color, radius = pl
    let brush = new SolidBrush (color)
    let shape = new Rectangle(Point(x, y),
                              Size(radius, radius))
    e.Graphics.FillEllipse (brush, shape)

// god config
let mutable pos1:coord = (100,100)
let earth:planet = (Color.Blue, 30)
let world = bigBang Color.Black (400,400) "Solar System" (drawPlanet &pos1 earth)

// position updates
let updateWorld (world:Form) (pos:coord byref) showtime =
    pos <- (1 + (fst pos), 1 + (snd pos))
    world.Refresh()

// setup timer
let timer = new Timer()
timer.Interval <- 100 // milliseconds between ticks
timer.Enabled <- true // timer is now running    
timer.Tick.Add (updateWorld world &pos1)

Application.Run world
