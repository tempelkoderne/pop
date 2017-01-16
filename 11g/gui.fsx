open System.Windows.Forms
open System.Drawing

type vector = (float * float)

// helpers
let toPx (x:vector) = (log10((fst x) + 100.0) * 100.0, log10((snd x) + 100.0) * 100.0)
let ofPx (x:vector) = (10.0**((fst x)/100.0) - 100.0, 10.0**((fst x)/100.0) - 100.0)
let scaleUp (x:vector) = (300.0 * (fst x), 300.0 * (snd x))
let scaleDown (x:vector) = ((fst x)/300.0, (snd x)/300.0)
let offset (r:int) (x:vector) = ((float r) + (fst x), (float r) + (snd x))

let ( .+ ) (x:vector) (y:vector) = ((fst x + fst y), (snd x + snd y))
let ( .- ) (x:vector) (y:vector) = ((fst x - fst y), (snd x - snd y))
let ( .* ) (x:float) (y:vector) = (x * (fst y), x * (snd y))


// builds planets
type planet(name:string, color:Color, radius:int, day0:vector []) =
    let mutable state = day0 // add function computing day0 from dataset
    let mutable r, v, a = day0.[0], day0.[1], day0.[2] 
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
    
    member self.name = name
    member self.color = color
    member self.radius = radius
    member self.vectors = state
    
    member self.Move() =
        state <- nextPos r v a
        r <- state.[0]
        v <- state.[1]
        a <- state.[2]        
    member self.DrawFunc(e:PaintEventArgs) =
        let x, y = toPx r
        let brush = new SolidBrush(self.color)
        let shape = new Rectangle(Point(int(round x), int(round y)),
                                  Size(self.radius, self.radius))
        e.Graphics.FillEllipse(brush, shape)

// builds worlds
type world(bcolor:Color, width:int, height:int, title:string, planets:planet list) =
    let mutable space = new Form()
    // let timer = new Timer(Interval = 100,
    //                       Enabled = true)
    // timer.Tick.Add self.UpdateWorld()
    
    member self.name = title
    member self.dims = Size (Point (width, height))
    member self.background = bcolor
    member self.system = space

    member self.BigBang() =
        space <- new Form(Text = self.name,
                          BackColor = self.background,
                          ClientSize = self.dims)
        for pl in planets do
            space.Paint.Add (pl.DrawFunc)
    member self.UpdateWorld() =
        for pl in planets do
            pl.Move()
        space.Refresh()

// create planet instances
let mars0 = [|(-1.648393944, 0.1701297755);
             (-0.0008611603679, -0.012730565);
             (0.0001071860543, -1.106261002e-05)|]
let mars = planet("Mars", Color.Red, 30, mars0)

let earth0 = [|(-0.1666759033, 0.969084551);
              (-0.01720978776, -0.003125375992);
              (5.187516652e-05, -0.0003016118194)|]
let earth = planet("Earth", Color.Blue, 60, earth0)

let sun0 = [|(300.0, 300.0);
            (0.0, 0.0);
            (0.0, 0.0)|]
let sun = planet("Sun", Color.Yellow, 10, sun0)

let planets = [earth; mars; sun]

// create solar system
let solar = world(Color.Black, 600, 600, "Solar System", planets)
solar.BigBang()

// create time
let timer = new Timer(Interval = 100,
                      Enabled = true)
timer.Tick.Add (fun showtime -> solar.UpdateWorld())

// let there be light
Application.Run solar.system


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
