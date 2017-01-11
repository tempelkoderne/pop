open System.Windows.Forms
open System.Drawing
// open Graph

// define types
type pen = Color * float
type point = (float * float)
type polyline = point list

// set helpers
let addPoints p0 p1 : point =
  (fst p0 + fst p1, snd p0 + snd p1)
let rotatePoint theta p : point =
  ((fst p) * cos theta - (snd p) * sin theta, (fst p) * sin theta + (snd p) * cos theta)
let rotatePoints theta l : polyline =
  List.map (rotatePoint theta) l
let scalePoint s p : point =
  (s * fst p, s * snd p)
let translatePoints d l : polyline =
  List.map (addPoints d) l


(***** BUILD SOLAR SYSTEM *****)

// create planet
let createPlanet backgroundColor (width, height) title draw =
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

let drawPlanet (coords : float*float []) (pen : pen) (e : PaintEventArgs) =
  let pairToPoint p = Point (int (round (fst p)), int (round (snd p)))
  let color, width = pen
  let Pen = new Pen (color, single width)
  let Points = Array.map pairToPoint (coords)
  
  let shape = new Rectangle(Point(fst coords, snd coords),
                            Size(20, 20))
  e.Graphics.FillEllipse (Pen, Points, shape)


// position updates
let updatePlanet (planet : Form) (coords : int*int) dtheta center showtime =
  let centeredCoords = translatePoints (scalePoint -1.0 center) coords
  let rotatedCoords = rotatePoints dtheta centeredCoords
  coords <- translatePoints center rotatedCoords
  planet.Refresh ()


// setup solar system details
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
let pen = (Color.Red, 1.0)

// let col = Color.Red
// let rad = 10

let earth = createPlanet backgroundColor size title (drawPlanet &coords pen) 



// setup timer
let timer = new Timer()
timer.Interval <- 100
timer.Enabled <- true
let dtheta = 0.01
timer.Tick.Add (updatePlanet earth &coords dtheta center)

Application.Run earth
