
type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer


printfn "__          __  __                            _          __  __           _              __  __ _           _ 
 \ \        / / | |                          | |        |  \/  |         | |            |  \/  (_)         | |
  \ \  /\  / /__| | ___ ___  _ __ ___   ___  | |_ ___   | \  / | __ _ ___| |_ ___ _ __  | \  / |_ _ __   __| |
   \ \/  \/ / _ \ |/ __/ _ \| '_ ` _ \ / _ \ | __/ _ \  | |\/| |/ _` / __| __/ _ \ '__| | |\/| | | '_ \ / _` |
    \  /\  /  __/ | (_| (_) | | | | | |  __/ | || (_) | | |  | | (_| \__ \ ||  __/ |    | |  | | | | | | (_| |
     \/  \/ \___|_|\___\___/|_| |_| |_|\___| \__\____/  |_|  |_|\__,_|___/\__\___|_|    |_|  |_|_|_| |_|\__,_|
                                                                                                              
                                                                                                              "

(*Intro skærm kan altid ændres*)

printf "Press any key to continue:"
System.Console.ReadKey() |> ignore
System.Console.Clear();;

(*Vi er gået væk fra indeksering da det kan crashe programmet hvis man bare trykker enter. *)
(* Vi er gået tilbage til indeksering fordi det er coolt, og fordi vi har fixet det med for loops. *)

let whoCodeMaker =
    printf "Who should be the code maker? (Computer (C)/ Human (H)) \n" 
    let valg = ((System.Console.ReadLine ()).ToLower())
    if valg.Length > 0 then
        if valg.[0] = 'c' then
            printfn "Code Maker: Computer"
            Computer
        else
            printfn "Code Maker: Human"
            Human
    else
            printfn "Code Maker: Human"
            Human

let whoCodeBreaker =
    printf "Who should be the code breaker? (Computer (C)/ Human (H)) \n" 
    let valg = ((System.Console.ReadLine ()).ToLower())
    if valg.Length > 0 then
        if valg.[0] = 'c' then
            printfn "Code Maker: Computer"
            Computer
        else
            printfn "Code Maker: Human"
            Human
    else
            printfn "Code Maker: Human"
            Human

let rec toColour (str : string) =
    if str.Length > 0 then 
        match (str.[0]) with    
        | 'r' -> Red
        | 'g' -> Green
        | 'y' -> Yellow
        | 'p' -> Purple
        | 'w' -> White
        | 'b' -> Black
        | _ -> printf "Invalid input. Try again:"; toColour((System.Console.ReadLine ()).ToLower())
    else 
        printf "Invalid input. Try again:"; toColour((System.Console.ReadLine ()).ToLower())

let kodevalg = 
    if whoCodeMaker = Human then
        let enterCode =
            printfn "\nPick your colours!"
            printfn "(red / green / yellow / purple / white / black)"
            printf "1st colour: "
            let col1 =  toColour((System.Console.ReadLine ()).ToLower())    // Til type (Til lower case (Kalder brugerinput))
            printf "2nd colour: "
            let col2 =  toColour((System.Console.ReadLine ()).ToLower())
            printf "3rd colour: "
            let col3 =  toColour((System.Console.ReadLine ()).ToLower())
            printf "4th colour: "
            let col4 =  toColour((System.Console.ReadLine ()).ToLower())
            let colours = [col1] @ [col2] @ [col3] @ [col4]
            printfn "Your pick: %A \n" colours
            colours
        enterCode
    else        
        let random = System.Random()
        let tal1 minNum maxNum = 
            let first = random.Next(minNum, maxNum)
            match first with 
            | 1 -> Red
            | 2 -> Green
            | 3 -> Yellow
            | 4 -> Purple
            | 5 -> White
            | 6 -> Black
            | _ -> Black 
        let pcol1 = (tal1 1 7)
        let pcol2 = (tal1 1 7)
        let pcol3 = (tal1 1 7)
        let pcol4 = (tal1 1 7)
        let colours = [pcol1] @ [pcol2] @ [pcol3] @ [pcol4]
        printfn "Secret Code created!\n"
        colours

printf "Press any key to continue:"
System.Console.ReadKey() |> ignore
System.Console.Clear();;