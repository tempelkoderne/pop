// TODO:
// 1) add hardcoded Sun to type world
// 2) add day0-reader to type planet
//      > Add new class : dataTools containing readData, convertData and CIV.
//      > Have planet and world inherit from dataTools class.
//      > Modify planet class to load from file rather than having initial vectors given as argument. 
// 3) add simulated data reader to planet maps in type world
//      > Add new map : simData to world.
//      > Modify world to overwrite simData for every planet at every UpdateWorld call.  
// 4) build euclidian-distance comparison tool for simulated vs empirical data in type world
// 5) (OPTIONAL) consider making offset a function of window size
// 6) move helper functions to separate .fs file
// 7) update UML-diagram
// 8) produce brief report
// 9) document code and implement black box testing


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

// builds planets
type planet(name:string, color:Color, radius:int, day0:vector []) =
    let mutable state = day0 // add function computing day0 from dataset
    let mutable r, v, a = day0.[0], day0.[1], day0.[2] 
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
type world(bcolor:Color, width:int, height:int, title:string, planets:planet list) =
    let rec inputDay() =
       printfn "%A" "How many days would you like to simulate?"
       try
           int(System.Console.ReadLine())
       with
           | _ -> inputDay()    
    let readData (filename : string, days:int) =
        let plusDays = min days 365
        let stop = (2457388 + (int plusDays) + 1)
        let file = filename + ".txt"
        if System.IO.File.Exists ("data/" + file) then
            let readFile (stream:System.IO.StreamReader) = stream.ReadToEnd()
            let inputStream = System.IO.File.OpenText ("data/" + file)
            let text = (readFile inputStream)
            inputStream.Close()
            let data = text.Split([|"2457387.500000000"; (stop.ToString())|],
                                  System.StringSplitOptions.None)
            let entries = data.[1].Split([|'\r'|], System.StringSplitOptions.None)
            entries
        else
            printfn "The file %s could not be found! Returning empty array." file
            let entries = [||]
            entries
    let convertData (data : string []) =
        let sliceOnce = (data.[1..(data.Length - 2)])
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
    let mutable dataMap = Map.empty
    let mutable space = new Form()

    member self.name = title
    member self.dims = Size (Point (width, height))
    member self.background = bcolor
    member self.system = space
    member self.data = dataMap

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
    member self.LoadData() =
        let days = inputDay()
        for pl in planets do
            let data = readData("Mars", days) |> convertData
            dataMap <- dataMap.Add (pl.name, data)

            // dataMap.Add ((pl.name, x))
            // dataMap.Add (pl.name, ) |> ignore


// create planet instances
let mars0 = [|(-1.648393944, 0.1701297755);
             (-0.0008611603679, -0.012730565);
             (0.0001071860543, -1.106261002e-05)|]
let mars = planet("Mars", Color.Red, 20, mars0)

let earth0 = [|(-0.1666759033, 0.969084551);
              (-0.01720978776, -0.003125375992);
              (5.187516652e-05, -0.0003016118194)|]
let earth = planet("Earth", Color.Blue, 20, earth0)

let sun0 = [|(0.0, 0.0);
            (0.0, 0.0);
            (0.0, 0.0)|]
let sun = planet("Sun", Color.Yellow, 20, sun0)

let planets = [earth; mars]

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
// solar.LoadData()
printfn "%A" earth.vectors