type vector = (float * float)
let ( .* ) x y = (x*(fst y),x*(snd y))
let ( .- ) (x:vector) (y:vector) = ((fst x - fst y), (snd x - snd y)) 

// Skal modificeres, så den loader det antal dage fra 1. januar 2016 og frem, som brugeren ønsker.
let loadData (filename : string) =
    let plusDays = "20"
    let stop = (2457388 + (int plusDays) + 1)

    if System.IO.File.Exists ("data/" + filename) then
        let readFile (stream:System.IO.StreamReader) = stream.ReadToEnd () // Function to read a stream to end.

        let inputStream = System.IO.File.OpenText ("data/" + filename) // Opens stream from file.

        let text = (readFile inputStream)   // Saves stream from file as string.
        inputStream.Close()                 // Closes stream.

        let data = text.Split([|"2457387.500000000";(stop.ToString())|], System.StringSplitOptions.None) // Splits text at 01 jan and x days ahead.
        let entries = data.[1].Split([|'\r'|], System.StringSplitOptions.None) // Splits data at line break.
        entries
    else
        printfn "The file could not be found! Returning empty array."
        let entries = [||]
        entries

(*convertData*)
// Converts array of strings to an array of 3x-float arrays. Longtitude, latitude and radius.
// To be implemented in loadData once approved.
let convertData (data : string []) =
    let sliceOnce = (data.[1..(data.Length - 2)]) // Removes 1st and last element.
    let mutable sliceTwice = Array.create (sliceOnce.Length) (0.0,0.0) // Creates a new array of positional vectors.
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

let CIV (positions : vector []) =
    let GMs = 2.959122082322128e-4 // AU^3 / dag^2
   
    let r0 = positions.[0] // Begyndelsesposition, r0.
    let r1 = positions.[1]
    let r0Len = sqrt (((fst r0)**2.0) + ((snd r0)**2.0))
    let a0 = -(GMs/(r0Len**3.0)) .* r0  // Accelerationsvektor.
    let v0 = r1 .- r0

    let vectors = [|r0;v0;a0|]
    vectors

let testData = (loadData "Mercury.txt")
let convertedData = convertData testData
let initialVectors = CIV (convertData testData)

printfn "%A" convertedData
printfn "%A" initialVectors