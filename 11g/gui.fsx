// TODO:
// 1) (OPTIONAL) consider making offset a function of window size
// 2) produce brief report
// 3) document code and implement black box testing

(*OBS! Programmet køres med fsharpi -r pop11g.dll gui.fsx*)

open System.Windows.Forms
open System.Drawing
open Pop11g
        
// builds planets
type Planet(name:string, color:Color, radius:int) =  
    let mutable state = initVec(name)
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
        let days = inputDay()
        for pl in planets do
            obsDataMap <- obsDataMap.Add (pl.name,
                                          (loadData(pl.name, days)))
            simDataMap <- simDataMap.Add (pl.name,
                                          (calcData(pl.name, days)))
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
// Application.Run solar.system

// initiates comparison of observed and simulated data
solar.LoadData()