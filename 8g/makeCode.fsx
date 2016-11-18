// makeCode : player -> code
// som tager en spillertype og returnerer en opgave enten ved at få input fra brugeren eller ved at beregne en opgave.


type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

let whoCodeMaker =
    printfn "Who code maker? (Computer / Human)"
    if ((System.Console.ReadLine ()).ToLower()).[0] = 'c' then
        printfn "Code Maker: Computer"
        Computer
    else
        printfn "Code Maker: Human"
        Human

let whoCodeBreaker =
    printfn "Who code breaker? (Computer / Human)"
    if ((System.Console.ReadLine ()).ToLower()).[0] = 'c' then
        printfn "Code Breaker: Computer"
        Computer
    else
        printfn "Code Breaker: Human"
        Human



(*
Modtag input skal bruge:
En variabel, som gemmer brugerens input som string, f.eks. "red green yellow purple"
En funktion, som kan splitte denne string i et array à fire elementer (ved mellemrum eller anden character)
En funktion, som via pattern matching læser array-elementerne og indsætter den tilsvarende type i en liste.
*)


// Virker, men er besværlig på flere områder. F.eks. ift. rigtigt antal farver og rigtig stavning.
// let enterOneString =
//     printf "Enter four colours: "
//     let str = (System.Console.ReadLine ()).ToLower()    // Lader brugeren indtaste 
//     let splitStr = str.Split(([|' '|]), System.StringSplitOptions.None)
//     if splitStr.Length = 4 then 
//         splitStr
//     else
//         printfn "You must enter four colours." 
//         [|"You must "|]
// printfn "%A" (enterOneString)

// toColour Matcher første character i en string med en farve-type / codeColour.
let rec toColour (str : string) =
    match (str.[0]) with
    | 'r' -> Red
    | 'g' -> Green
    | 'y' -> Yellow
    | 'p' -> Purple
    | 'w' -> White
    | 'b' -> Black
    | _ -> printf "Invalid input. Try again: "; toColour((System.Console.ReadLine ()).ToLower())

// testCode virker, men er ikke særlig elegant.
let enterCode () =
    printfn "Pick your colours!"
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
    printfn "Your pick: %A" colours
    colours

let generateCode () =
    let colors = [Red; Green; Yellow; Purple; White; Black]
    let rand = System.Random()
    let code = [colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)]]
    code

// makeCode tager en spillertype og kalder en passende funktion, som returnerer en code.
let makeCode (user : player) =
    if user = Human then
        enterCode ()
    else
        generateCode ()

// printfn "%A" (makeCode (Human))
// printfn "%A" (makeCode (Computer))


// bygger på enterCode, der bygger på toColour

