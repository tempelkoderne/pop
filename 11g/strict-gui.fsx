//////////////////////////
/////(* DEPRECATED *)/////
//////////////////////////

(*
// god config
let earth = (Color.Blue, 50)
let size = (700,700)
let cntr = (0.5 * float (fst size), 0.5* float (snd size))
let bcolor:Color = Color.Black
let title:string = "Solar System"

let mutable r0:vector = cntr .+ (scaleUp (-0.1666759033, 0.969084551))
let mutable v0:vector = (-0.01720978776, -0.003125375992)
let mutable a0:vector = (5.187516652e-05, -0.0003016118194)

// calc next position
let nextPos (r:vector) (v:vector) (a:vector) =
        let GMs = 2.959122082322128e-4
        let r0 = r
        let v0 = v
        let a0 = a

        let r1:vector = (r0 .+ v0)
        let r0Len = sqrt (((fst r0)**2.0) + ((snd r0)**2.0))
        let a1:vector = -(GMs/(r0Len**3.0)) .* r0
        let v1:vector = (v0 .+ a0)
        let (positions:vector []) = [|r1;v1;a1|]
        positions

// create world
let bigBang (backColor:Color) (size:int*int) (title:string) (drawFunc:PaintEventArgs -> unit) =
    let world = new Form(Text = title,
                       BackColor = backColor,
                       ClientSize = Size (Point ((fst size), (snd size))))
    world.Paint.Add drawFunc
    world

// draw planets
let drawPlanet (r:vector byref) (pl:Color*int) (e:PaintEventArgs) =
    let x, y = toPx r
    // let trans_x, trans_y = toPx x + 300.0, toPx y + 300.0
    let color, radius = pl
    let brush = new SolidBrush (color)
    let shape = new Rectangle(Point(int (round x), int (round y)),
                              Size(radius, radius))
    e.Graphics.FillEllipse (brush, shape)

// move planets
let updateWorld (world:Form) (r:vector byref) (v:vector byref) (a:vector byref) showtime =
    let state = nextPos (scaleDown (r .- cntr)) v a
    r <- cntr .+ (scaleUp state.[0])
    v <- state.[1]
    a <- state.[2]
    world.Refresh()

// build world
let solar = bigBang bcolor size title (drawPlanet &r0 (Color.Blue, 60))

// add timer events
let timer = new Timer(Interval = 10,
                      Enabled = true)
timer.Tick.Add (updateWorld solar &r0 &v0 &a0)

// run system
Application.Run solar
*)