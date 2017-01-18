module Pop11g
open System
open System.Windows.Forms
open System.Drawing

(*Compile file using: fsharpc -a pop11g.fs*)

type vector = (float * float)

// Zoom-in button.
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

// Zoom-out button.
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


// Various helper functions for converting positional vectors to pixel coordinates, as well as scaling.
let toPx (x:vector) = (log10((fst x) + 1.0) * 100.0, log10((snd x) + 1.0) * 100.0)
let ofPx (x:vector) = (10.0**((fst x)/100.0) - 1.0, 10.0**((fst x)/100.0) - 1.0)
let scaleUp (x:vector) = ((zoom) * (fst x), (zoom) * (snd x))
let scaleDown (x:vector) = ((fst x)/100.0, (snd x)/100.0)
let offset (r:int) (x:vector) = ((float r) + (fst x), (float r) + (snd x))

// Custom operators for computing vectors.
let ( .+ ) (x:vector) (y:vector) = ((fst x + fst y), (snd x + snd y))
let ( .- ) (x:vector) (y:vector) = ((fst x - fst y), (snd x - snd y))
let ( .* ) (x:float) (y:vector) = (x * (fst y), x * (snd y))


(* inputDay *)
///<summary>
/// Allows user to enter an amount of days to simulate.
///</summary>
///<returns>
/// An integer.
///</returns>
///<remarks>
/// Handles positive and negative integers as well as floats.
///</remarks>
let inputDay() =
    printf "How many days would you like to simulate?"
    let rec getInput() =
        printfn " Enter an integer between 1 and 365."
        let input =
            try // Prevents system from attempting to convert a float.
                int (System.Console.ReadLine())
            with
                | _ -> printf "Invalid input." ; getInput()
        if input < 1 || input > 365 then    // Prevents function from returning integers that would result in index out of bounds errors.
            printf "Invalid input."
            getInput()
        else
            input
    getInput()
    
(* loadData *)
///<summary>
/// Retrieves data from a file.
///</summary>
///<params name="planet">
/// String. Name of the file to retrieve data from.
///</params>
///<params name="days">
/// Integer. Correlates to the amount of data to be retrieved.
///</params>
///<returns>
/// A vector array.
///</returns>
let loadData(planet:string, days:int) =
    let stop = 2457388 + (int days) // Calculates last date to be read.
    
    if System.IO.File.Exists ("data/" + planet + ".txt") then
        // Loads contents of file.
        let readFile (stream:System.IO.StreamReader) = stream.ReadToEnd()
        let inputStream = System.IO.File.OpenText ("data/" + planet + ".txt")
        let text = (readFile inputStream)
        inputStream.Close()

        // Slices loaded string to allow selection of the data of interest.
        let data = text.Split([|"2457387.500000000";(stop.ToString() + ".500000000")|],
                                System.StringSplitOptions.None)
        let entries = data.[1].Split([|'\r'|],
                                        System.StringSplitOptions.None)
        let sliceOnce = (entries.[1..(entries.Length - 2)]) // Removes superfluous first and last entry.

        // Converts spherical coordinates to cartesian and loads them into an array as vectors.
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

(* initVec *)
///<summary>
/// Computes initial vectors of a given planet.
///</summary>
///<params name="planet">
/// String. Name of the file to retrieve data from and compute vectors.
///</params>
///<returns>
/// A vector array.
///</returns>
///<remarks>
/// Dependencies: loadData().
///</remarks>
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

(* calcData *)
///<summary>
/// Recursively computes all positions of a given planet over a specified number of days.
///</summary>
///<params name="planet">
/// String. Name of the file to retrieve data from and compute vectors.
///</params>
///<params name="days">
/// Integer. Correlates to the amount of data to be retrieved.
///</params>
///<returns>
/// A vector array.
///</returns>
///<remarks>
/// Dependencies: initVec() (and loadData() indirectly).
///</remarks>
let calcData(planet:string, days:int) =
    let GMs = 2.959122082322128e-4
    let state0 = initVec(planet)    // Retrieves the initial vectors of the planet, r0, v0 and a0.
    let rec compute (days:int) (state0:(float*float) []) =  // Recursive computation of vectors using previous vectors while days > 0.
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