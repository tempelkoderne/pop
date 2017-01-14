
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
        let arr = data.[1].Split([|'\r'|], System.StringSplitOptions.None) // Splits data at line break.
        arr
    else
        printfn "The file could not be found! Returning empty array."
        let arr = [||]
        arr

(*convertData*)
// Converts array of strings to an array of 3x-float arrays. Longtitude, latitude and radius.
// To be implemented in loadData once approved.
let convertData (data : string []) =
    let sliceOnce = (data.[1..(data.Length - 2)]) // Removes 1st and last element.
    let mutable sliceTwice = Array2D.create (sliceOnce.Length) 3 0.0 // Creates an array of 3-element-arrays.
    for i = 0 to sliceOnce.Length - 1 do
        sliceTwice.[i,0] <- float ((sliceOnce.[i]).[22..29])
        sliceTwice.[i,1] <- float ((sliceOnce.[i]).[31..38])
        sliceTwice.[i,2] <- float ((sliceOnce.[i]).[40..54])
    sliceTwice
let testData = (loadData "Mercury.txt")
let convertedData = (convertData testData)

printfn "%A" convertedData