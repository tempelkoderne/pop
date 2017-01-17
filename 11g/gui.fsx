// TODO:
// 1) (OPTIONAL) consider making offset a function of window size
// 2) move helper functions to separate .fs file
// 3) update UML-diagram
// 4) produce brief report
// 5) document code and implement black box testing

open System.Windows.Forms
open System.Drawing

type vector = (float * float)

// helpers
let toPx (x:vector) = (log10((fst x) + 1.0) * 100.0, log10((snd x) + 1.0) * 100.0)
let ofPx (x:vector) = (10.0**((fst x)/100.0) - 1.0, 10.0**((fst x)/100.0) - 1.0)
let scaleUp (x:vector) = (100.0 * (fst x), 100.0 * (snd x))
let scaleDown (x:vector) = ((fst x)/100.0, (snd x)/100.0)
let offset (r:int) (x:vector) = ((float r) + (fst x), (float r) + (snd x))

let ( .+ ) (x:vector) (y:vector) = ((fst x + fst y), (snd x + snd y))
let ( .- ) (x:vector) (y:vector) = ((fst x - fst y), (snd x - snd y))
let ( .* ) (x:float) (y:vector) = (x * (fst y), x * (snd y))


// reads data
type DataReader() =
    member self.inputDay() =
        let rec inputDay() =
            printfn "How many days would you like to simulate?"
            try
                int (System.Console.ReadLine())
            with
                | _ -> inputDay()
        inputDay()

    member self.loadData(planet:string, days:int) =
        let plusDays = min days 365 // since start is always 1/1/16
        let stop = 2457388 + (int plusDays)
        
        if System.IO.File.Exists ("data/" + planet + ".txt") then
            let readFile (stream:System.IO.StreamReader) = stream.ReadToEnd()
            let inputStream = System.IO.File.OpenText ("data/" + planet + ".txt")
            let text = (readFile inputStream)
            inputStream.Close()
            let data = text.Split([|"2457387.500000000";(stop.ToString())|],
                                  System.StringSplitOptions.None)
            let entries = data.[1].Split([|'\r'|],
                                         System.StringSplitOptions.None)
            let sliceOnce = (entries.[1..(entries.Length - 2)])
            let mutable sliceTwice = Array.create (sliceOnce.Length) (0.0,0.0)
            for i = 0 to sliceOnce.Length - 1 do
                let degToRad (deg : float) = (deg) * (System.Math.PI)/180.0
                let longDeg = float ((sliceOnce.[i]).[22..29]) // x, longtitude
                let latDeg = float ((sliceOnce.[i]).[31..38]) // y, latitude
                let rad = float ((sliceOnce.[i]).[40..54]) // radius
                let long = (degToRad longDeg)
                let lat = (degToRad (latDeg + 90.0))
                let x = (rad) * System.Math.Sin(lat) * System.Math.Cos(long)
                let y = (rad) * System.Math.Sin(lat) * System.Math.Sin(long)
                sliceTwice.[i] <- (x,y)
            sliceTwice
        else
            printfn "The file %s could not be found!" (planet + ".txt")
            [||]
    member self.initVec(planet:string) =
        let initState = self.loadData(planet, 2)
        let GMs = 2.959122082322128e-4
        let r0 = initState.[0]
        let r1 = initState.[1]
        let r0Len = sqrt (((fst r0)**2.0) + ((snd r0)**2.0))
        let a0 = -(GMs/(r0Len**3.0)) .* r0
        let v0 = r1 .- r0
        let vectors = [|r0;v0;a0|]
        vectors
    member self.calcData(planet:string, days:int) =
        let GMs = 2.959122082322128e-4
        let state0 = self.initVec(planet)
        let rec compute (days:int) (state0:(float*float) []) =
            if days > 0 then
                let r1 = (state0.[0] .+ state0.[1])
                let r0Len = sqrt(((fst state0.[0])**2.0) + ((snd state0.[0])**2.0))
                let a1 = -(GMs/(r0Len**3.0)) .* state0.[0]
                let v1 = (state0.[1] .+ state0.[2])
                let state1 = [|r1;v1;a1|]
                
                Array.concat [[|state0.[0]|]; (compute (days-1) state1)]
            else
                [||]
        compute days state0
        
// builds planets
type Planet(name:string, color:Color, radius:int) =
    inherit DataReader()
    
    let mutable state = base.initVec(name)
    let mutable r, v, a = state.[0], state.[1], state.[2] 
    let nextPos (r:vector) (v:vector) (a:vector) =
        let GMs = 2.959122082322128e-4
        let r0 = r
        let v0 = v
        let a0 = a

        // if r0 = (0.0, 0.0) then
        //     let r1:vector = (0.0, 0.0)
        //     let v1:vector = (0.0, 0.0)
        //     let a1:vector = (0.0, 0.0)
        //     let (positions:vector []) = [|r1;v1;a1|]
        //     positions
        // else
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
        let x, y = offset 300 (scaleUp r)
        let brush = new SolidBrush(self.color)
        let shape = new Rectangle(Point(int(round x), int(round y)),
                                  Size(self.radius, self.radius))
        e.Graphics.FillEllipse(brush, shape)

// builds worlds
type World(bcolor:Color, width:int, height:int, title:string, planets:Planet list) =
    inherit DataReader()

    let mutable obsDataMap = Map.empty
    let mutable simDataMap = Map.empty
    let mutable errorMap = Map.empty
    let mutable space = new Form()

    member self.name = title
    member self.dims = Size (Point (width, height))
    member self.background = bcolor
    member self.system = space
    member self.obsData = obsDataMap
    member self.simData = simDataMap
    member self.dError = errorMap

    member self.BigBang() =
        space <- new Form(Text = self.name,
                          BackColor = self.background,
                          ClientSize = self.dims)
        for pl in planets do
            space.Paint.Add (pl.DrawFunc)
        let drawSun(e:PaintEventArgs) =
            let brush = new SolidBrush(Color.Yellow)
            let shape = new Rectangle(Point(300, 300),
                                      Size(10, 10))
            e.Graphics.FillEllipse(brush, shape)
        space.Paint.Add (drawSun)
    member self.UpdateWorld() =
        for pl in planets do
            pl.Move()
        space.Refresh()
    member self.LoadData() =
        let days = base.inputDay()
        for pl in planets do
            obsDataMap <- obsDataMap.Add (pl.name,
                                          (base.loadData(pl.name, days)))
            simDataMap <- simDataMap.Add (pl.name,
                                          (base.calcData(pl.name, days)))
            let compLenVec = Array.map2 (fun x y -> x .- y) simDataMap.[pl.name] obsDataMap.[pl.name]
            let compDist = Array.map (fun x -> sqrt (((fst x)**2.0) + ((snd x)**2.0))) compLenVec
            errorMap <- errorMap.Add (pl.name, compDist)


// create planet instances
let mars = Planet("Mars", Color.Red, 20)
let earth = Planet("Earth", Color.Blue, 20)

let planets = [Planet("Mercury", Color.Brown, 23);
               Planet("Venus", Color.Orange, 60);
               Planet("Earth", Color.Blue, 64);
               Planet("Mars", Color.Red, 34);
               Planet("Jupiter", Color.Gray, 70);
               Planet("Saturn", Color.Yellow, 58);
               Planet("Uranus", Color.LightBlue, 25);
               Planet("Neptune", Color.Blue, 25);
               Planet("Pluto", Color.LightSlateGray, 10)]

// create solar system
let solar = World(Color.Black, 600, 600, "Solar System", planets)
solar.BigBang()

// create time
let timer = new Timer(Interval = 100,
                      Enabled = true)
timer.Tick.Add (fun showtime -> solar.UpdateWorld())

// let there be light
Application.Run solar.system

solar.LoadData()


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
