module Pop11g
open System
open System.Windows.Forms
open System.Drawing

(*OBS! .dll filen compiles via fsharpc -a pop11g.fs*)

type vector = (float * float)

// Zoom knapper
let mutable zoom = 10.0
let Zibutton = new Button()
Zibutton.Size <- new Size (100, 40)
Zibutton.Location <- new Point (20,20)
Zibutton.Text <- "Zoom in"
Zibutton.BackColor <- Color.White
let ZibuttonClicked (e: EventArgs) =
    if zoom >= 0.0 then
        zoom <- zoom + 5.0
    else
        zoom <- zoom
Zibutton.Click.Add ZibuttonClicked

let Zobutton = new Button()
Zobutton.Size <- new Size (100, 40)
Zobutton.Location <- new Point (20,65)
Zobutton.Text <- "Zoom out"
Zobutton.BackColor <- Color.White
let ZobuttonClicked (e: EventArgs) =
    if zoom > 5.0 then
        zoom <- zoom - 5.0
    else 
        zoom <- zoom
Zobutton.Click.Add ZobuttonClicked


// helpers
let toPx (x:vector) = (log10((fst x) + 1.0) * 100.0, log10((snd x) + 1.0) * 100.0)
let ofPx (x:vector) = (10.0**((fst x)/100.0) - 1.0, 10.0**((fst x)/100.0) - 1.0)
let scaleUp (x:vector) = ((zoom) * (fst x), (zoom) * (snd x))
let scaleDown (x:vector) = ((fst x)/100.0, (snd x)/100.0)
let offset (r:int) (x:vector) = ((float r) + (fst x), (float r) + (snd x))

// custom operators
let ( .+ ) (x:vector) (y:vector) = ((fst x + fst y), (snd x + snd y))
let ( .- ) (x:vector) (y:vector) = ((fst x - fst y), (snd x - snd y))
let ( .* ) (x:float) (y:vector) = (x * (fst y), x * (snd y))

// reads data
let inputDay() =
    printf "How many days would you like to simulate?"
    let rec getInput() =
        printfn " Enter an integer between 1 and 365."
        let input =
            try
                int (System.Console.ReadLine())
            with
                | _ -> printf "Invalid input." ; getInput()
        if input < 1 || input > 365 then
            printf "Invalid input."
            getInput()
        else
            input
    getInput()
    

let loadData(planet:string, days:int) =
    let stop = 2457388 + (int days)
    
    if System.IO.File.Exists ("data/" + planet + ".txt") then
        let readFile (stream:System.IO.StreamReader) = stream.ReadToEnd()
        let inputStream = System.IO.File.OpenText ("data/" + planet + ".txt")
        let text = (readFile inputStream)
        inputStream.Close()
        let data = text.Split([|"2457387.500000000";(stop.ToString() + ".500000000")|],
                                System.StringSplitOptions.None)
        let entries = data.[1].Split([|'\r'|],
                                        System.StringSplitOptions.None)
        let sliceOnce = (entries.[1..(entries.Length - 2)])
        let mutable sliceTwice = Array.create (sliceOnce.Length) (0.0,0.0)
        for i = 0 to sliceOnce.Length - 1 do
            // printfn "%A" i
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

let initVec(planet:string) =
    let initState = loadData(planet, 2)
    let GMs = 2.959122082322128e-4
    let r0 = initState.[0]
    let r1 = initState.[1]
    let r0Len = sqrt (((fst r0)**2.0) + ((snd r0)**2.0))
    let a0 = -(GMs/(r0Len**3.0)) .* r0
    let v0 = r1 .- r0
    let vectors = [|r0;v0;a0|]
    vectors

let calcData(planet:string, days:int) =
    let GMs = 2.959122082322128e-4
    let state0 = initVec(planet)
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