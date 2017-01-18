// TODO:
// 1) (OPTIONAL) consider making offset a function of window size
// 2) produce brief report
// 3) document code and implement black box testing
// 4) print table of average error distances per planet over time

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
        let r0, v0, a0 = r, v, a
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
                                      Size(20, 20))
            e.Graphics.FillEllipse(brush, shape)
        space.Paint.Add (drawSun)
    member self.UpdateWorld() =
        for pl in planets do
            pl.Move()
        space.Refresh()
    member self.Simulate() =
        let days = inputDay()
        let mutable table = ""
        for pl in planets do
            // Load observed and calculate simulated positions for each planet.
            obsDataMap <- obsDataMap.Add (pl.name,
                                          (loadData(pl.name, days)))
            simDataMap <- simDataMap.Add (pl.name,
                                          (calcData(pl.name, days)))
            // Compute average offset (distance between observed/simulated positions) for each planet.
            let compLenVec = Array.map2 (fun x y -> x .- y) simDataMap.[pl.name] obsDataMap.[pl.name]
            let compDist = Array.map (fun x -> sqrt (((fst x)**2.0) + ((snd x)**2.0))) compLenVec
            errorMap <- errorMap.Add (pl.name, compDist)
            
            // Write average offset to table for each planet.
            let ar = errorMap.[(pl.name)]
            let arLen = float ar.Length 
            let sum = float (Array.sum ar)
            let avg = sum / arLen
            table <- table + (sprintf "    %-10s" (pl.name)) + (sprintf "%20s" (avg.ToString())) + "  AU\n"
        // Print table.
        printfn "Press any key to continue.\n"
        System.Console.ReadKey() |> ignore
        System.Console.Clear()
        printfn "Average distance between observed and simulated planetary positions over %A days.\n" days
        printfn "%s" table


// Planet instances.
let planets = [Planet("Mercury", Color.Brown, 5);
               Planet("Venus", Color.Orange, 10);
               Planet("Earth", Color.Blue, 10);
               Planet("Mars", Color.Red, 6);
               Planet("Jupiter", Color.Gray, 15);
               Planet("Saturn", Color.Yellow, 11);
               Planet("Uranus", Color.LightBlue, 11);
               Planet("Neptune", Color.Blue, 11);
               Planet("Pluto", Color.LightSlateGray, 4)]

// create solar system
let solar = World(Color.Black, 600, 600, "Solar System", planets)
solar.BigBang()

// create time
let timer = new Timer(Interval = 100,
                      Enabled = true)
timer.Tick.Add (fun showtime -> solar.UpdateWorld())

//Add the buttons
solar.system.Controls.Add Zibutton
solar.system.Controls.Add Zobutton

// initiates comparison of observed and simulated data
solar.Simulate()

// let there be light
printfn "Launching graphical simulation of solar system, showing planetary movement from 01-01-2016 and indefinitely."
Application.Run solar.system
