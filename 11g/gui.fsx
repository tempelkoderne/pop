open System.Windows.Forms
open System.Drawing
open Pop11g

(*Run program using: fsharpi -r pop11g.dll gui.fsx*)



(* Planet *)
///<summary>
/// Planet class. Allows creation of planets with various attributes.
///</summary>
///<params name="name">
/// String. Name of the planet. Used for various computations.
///</params>
///<params name="color">
/// Color-method. Color to be used in a GUI.
///</params>
///<params name="radius">
/// Integer.  Size of planet in a GUI.
///</params>
///<returns>
/// A Planet-type instance.
///</returns>
type Planet(name:string, color:Color, radius:int) =  
    // Attributes.
    let mutable state = initVec(name)   // State of planet. Positional vector r, as well as speed and acceleration vectors, v and a.
    let mutable r, v, a = state.[0], state.[1], state.[2] // Extracts vectors.

    // Helper function.
    // Calculates the next position of a planet based on the previous position r, as well as the speed and acceleration, v and a.
    let nextPos (r:vector) (v:vector) (a:vector) =        
        let GMs = 2.959122082322128e-4
        let r0, v0, a0 = r, v, a
        let r1:vector = (r0 .+ v0)
        let r0Len = sqrt (((fst r0)**2.0) + ((snd r0)**2.0))
        let a1:vector = -(GMs/(r0Len**3.0)) .* r0
        let v1:vector = (v0 .+ a0)
        let (positions:vector []) = [|r1;v1;a1|]
        positions
    
    // Methods.
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



(* World *)
///<summary>
/// World class. Allows creation of a universe of Planet-types and continuous drawing of these in a GUI.
///</summary>
///<params name="bcolor">
/// Color-method. Color of background in a GUI.
///</params>
///<params name="width">
/// Integer. Initial width of window in pixels.
///</params>
///<params name="height">
/// Integer. Initial height of window in pixels.
///</params>
///<params name="title">
/// String. Title of a GUI window.
///</params>
///<params name="planets">
/// A Planet list. The planets to be drawn in a GUI.
///</params>
///<returns>
/// A World-type instance.
///</returns>
type World(bcolor:Color, width:int, height:int, title:string, planets:Planet list) =
    // Attributes.
    // Maps containing observed and simulated data, as well as map containing difference between obs and sim.
    let mutable obsDataMap = Map.empty
    let mutable simDataMap = Map.empty
    let mutable errorMap = Map.empty

    let mutable space = new Form()

    // Methods
    member self.name = title
    member self.dims = Size (Point (width, height))
    member self.background = bcolor
    member self.system = space
    member self.obsData = obsDataMap
    member self.simData = simDataMap
    member self.dError = errorMap

    // Method for initializing GUI (form). Paints all planets and sun at initial positions.
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
    // Computes and moves all planets. Updates GUI (form) 
    member self.UpdateWorld() =
        for pl in planets do
            pl.Move()
        space.Refresh()
    
    // Computes simulated movement of World-instance (solar system) and average offset of
    // simulated data compared to empirical data from NASA. Prints table of avg. offset in AU.
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
        printfn "Press any key to show results of simulation."
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

// World instance. Creates solar system.
let solar = World(Color.Black, 600, 600, "Solar System", planets)
solar.BigBang()

// Initializes a timer in order to update the World instance indefinitely.
let timer = new Timer(Interval = 100,
                      Enabled = true)
timer.Tick.Add (fun showtime -> solar.UpdateWorld())

// Adds the zoom-buttons.
solar.system.Controls.Add Zibutton
solar.system.Controls.Add Zobutton

// Initiates comparison of observed and simulated data.
solar.Simulate()

// Let there be light!
printfn "Launching graphical simulation of solar system, showing planetary movement from 01-01-2016 and indefinitely."
Application.Run solar.system



(*UNIT TESTS*)
printfn "\nPress any key to show additional unit tests."
System.Console.ReadKey() |> ignore
System.Console.Clear()

printfn "\nPlanet class tests:"
printfn "- Using \"Mercury\" as example."
printfn "    self.name   = \"Mercury\" : %b" (planets.[0].name = "Mercury")
printfn "    self.color  = Color.Brown : %b" (planets.[0].color = (Color.Brown))
printfn "    self.radius = 5           : %b" (planets.[0].radius = 5)
printfn "1)  self.vectors              : %A" planets.[0].vectors
planets.[0].Move()
printfn "    self.Move()               : Notice difference between 1) and 2) self.vectors."
printfn "2)  self.vectors              : %A" planets.[0].vectors
printfn "    self.DrawFunc(e) tested previously via GUI - otherwise GUI animation would be impossible."

printfn "\nWorld class tests:"
printfn "- Using \"Mercury\" as example where needed."
printfn "    self.name = \"Solar System\"    : %b" (solar.name = "Solar System")
printfn "    self.dims                     : %A" solar.dims
printfn "    self.background = Color.Black : %b" (solar.background = (Color.Black))
printfn "    self.system                   : %A" solar.system
printfn "    self.BigBang() tested previously via GUI - otherwise GUI would not have launched properly."
printfn "    self.UpdateWorld() tested previously via GUI - otherwise GUI animation would be impossible."
printfn "    self.simData                  : %A" solar.simData.["Mercury"]
printfn "    self.obsdata                  : %A" solar.obsData.["Mercury"]
printfn "    self.dError                   : %A" solar.dError.["Mercury"]
printfn "    self.Simulate() tested previously. Otherwise no comparison table would have been printed."