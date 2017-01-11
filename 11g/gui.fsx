open System.Windows.Forms
open System.Drawing
// open Graph

// define types
// type point = (int * int)
type planet = (Color * int)
// type planets = planet * point list
// type polyline = Color * int list


(***** BUILD SOLAR SYSTEM *****)

// create planet
let bigBang backgroundColor (width, height) title draw =
    let win = new Form ()
    win.Text <- title
    win.BackColor <- backgroundColor
    win.ClientSize <- Size (width, height)
    win.Paint.Add draw
    win

// draw planet
(*
let drawPlanet (coords : polyline byref) (pen : pen) (e : PaintEventArgs) =
  let pairToPoint p =
    Point (int (round (fst p)), int (round (snd p)))
  let color, width = pen
  let Pen = new Pen (color, single width)
  let Points = Array.map pairToPoint (List.toArray coords)
  e.Graphics.DrawLines (Pen, Points)
*)

let drawPlanet (coord : int*int) (pl : planet) (e : PaintEventArgs) =
    let x, y = coord
    let color, radius = pl
    let brush = new SolidBrush (color)
    let shape = new Rectangle(Point(x, y),
                              Size(radius, radius))
    e.Graphics.FillEllipse (brush, shape)

// brief tests without movement
let mutable pos1 = (100,100)
let earth = (Color.Blue, 30)
let world = bigBang Color.Black (400,400) "Solar System" (drawPlanet pos1 earth)
Application.Run world

///////// TODO: add movement using Timer-module


(*
// solar system config
let title = "Solar system"
let backgroundColor = Color.Black
let size = (400, 400)
let polarToCartesian r theta = (r * cos theta, r * sin theta)
let mutable coords = List.map (polarToCartesian 100.0) [0.0;
                                                        2.0/3.0*System.Math.PI;
                                                        4.0/3.0*System.Math.PI;
                                                        0.0]

let center = scalePoint (1.0/2.0) (float (fst size), float (snd size))
coords <- translatePoints center coords
let earth = (Color.Red, 10)

let world = bigBang col size title (drawPlanet pos earth)
*)


// position updates
let updatePlanet (planet : Form) (coords : int*int list byref) (newCoords : int*int) showtime =
    // let centeredCoords = translatePoints (scalePoint -1.0 center) coords
    // let rotatedCoords = rotatePoints dtheta centeredCoords
    coords <- newCoords
    planet.Refresh ()

// setup timer
let timer = new Timer()
timer.Interval <- 100
timer.Enabled <- true
let dtheta = 0.01
timer.Tick.Add (updatePlanet earth &coords dtheta center)

Application.Run world
