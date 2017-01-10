open System.Windows.Forms
open System.Drawing

type coordinates = (float * float) list
type pen = Color * float
type polygon = coordinates * pen

let createForm backgroundColor (width, height) title draw =
    let win = new Form ()
    win.Text <- title
    win.BackColor <- backgroundColor
    win.ClientSize <- Size (width, height)
    win.Paint.Add draw
    win

let drawPoints (polygLst:polygon list) (e:PaintEventArgs) =
    let pairToPoint (x:float, y:float) =
        Point (int (round x), int (round y))

    for polyg in polygLst do
        let coords, (color, width) = polyg
        let pen = new Pen (color, single width)
        let Points = Array.map pairToPoint (List.toArray coords)
        e.Graphics.DrawLines (pen, Points)

let translatePoint (dx, dy) (x, y) =
    (x + dx, y + dy)

let translatePoints (dx, dy) arr =
    List.map (translatePoint (dx, dy)) arr

let rotatePoint theta (x, y) =
    (x * cos theta - y * sin theta, x * sin theta + y * cos theta)

let rotatePoints theta arr =
    List.map (rotatePoint theta) arr



/// draw stuff
let title = "Transforming polygons"
let backgroundColor = Color.White
let size = (400, 200)
let points = [(0.0, 0.0); (10.0, 170.0); (320.0, 20.0); (0.0, 0.0)]
let polygLst =
    [(points, (Color.Black, 1.0));
     (translatePoints (40.0, 30.0) points, (Color.Red, 2.0));
     (rotatePoints (1.0 * System.Math.PI / 180.0) points, (Color.Green, 1.0))]

let win = createForm backgroundColor size title (drawPoints polygLst)
Application.Run win





let drawPlanet (coord:int*int, r:int) =
    let drawPlanet = new Form(Text = "die Sonne, ja",
                              BackColor = Color.Black,
                              ClientSize = Size (600,600))
    drawPlanet.Paint.Add(fun draw ->
        draw.Graphics.FillEllipse(
            new SolidBrush(Color.Red), //planet color
            new Rectangle(Point(fst coord, snd coord), //planet pos
                          Size(r, r))) //planet bounding box
            )
    Application.Run(drawPlanet)
