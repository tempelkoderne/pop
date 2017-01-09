let opendata (filename:string) =
    if System.IO.File.Exists ("data/" + filename) then
        let readFile (stream:System.IO.StreamReader) =
            stream.ReadToEnd ()

        let inputStream = System.IO.File.OpenText ("data/" + filename)

        let text = (readFile inputStream)
        inputStream.Close()
        let data = text.Split([|"$$SOE";"$$EOE"|], System.StringSplitOptions.None)
        let arr = data.[1].Split([|"     ";"  "|], System.StringSplitOptions.None)
        arr
    else
        failwith "the file could not be found!"
