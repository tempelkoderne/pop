(* Aflevering 8g - Mastermind *)
// Tempelkoderne
//      Anders Geil
//      Peter Lim
//      Tobias Stannius

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* Types *)
///<summary>
/// Various types used throughout the program.
///</summary>

type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* makeCode *)
///<summary>
/// Depending on the player type supplied to the function:
/// - Human: Prompts the user to enter several characters. The input is then
///   matched with the allowed codeColor types and appended to a list.
/// - Computer: Generates a random code.
///</summary>
///<params name="user">
/// A player type.
///</params>
///<returns>
/// A code (codeColor list) containing four elements.
///</returns>

let makeCode (user : player) =
    if user = Human then
        let rec inputCode () =
            printfn "Pick your colors!"
            printfn "([R]ed; [G]reen; [Y]ellow; [P]urple; [W]hite; [B]lack)"
            let input = System.Console.ReadLine().ToLower()
            let sep = [|" "; "; "; ", "; ";"; ","; "-"|]
            let splinput = input.Split (sep, System.StringSplitOptions.None)
            if splinput.Length <> 4 || (Array.exists ((=) "") splinput) then
                printfn "You need to type 4 colors!"
                inputCode ()
            else
                formatCode (Array.toList splinput)
        and formatCode input =
            match input with
            | [] -> []
            | h::t when h.[0] = 'r' -> Red::(formatCode t)
            | h::t when h.[0] = 'g' -> Green::(formatCode t)
            | h::t when h.[0] = 'y' -> Yellow::(formatCode t)
            | h::t when h.[0] = 'p' -> Purple::(formatCode t)
            | h::t when h.[0] = 'w' -> White::(formatCode t)
            | h::t when h.[0] = 'b' -> Black::(formatCode t)
            | h::t -> printfn "Your color must match one of the options above. \nWhat did you mean by \"%s\"?" h; colFixer t
        and colFixer (t:string list) =
            match System.Console.ReadLine().ToLower()::t with
            | h::t when h.Length > 0 -> formatCode (h::t)
            | h::t when h.Length < 1 -> colFixer t
            | _ -> [Black]
        inputCode()
    else
        let colors = [Red; Green; Yellow; Purple; White; Black]
        let rand = System.Random()
        let code = [colors.[rand.Next(0,5)];
                    colors.[rand.Next(0,5)];
                    colors.[rand.Next(0,5)];
                    colors.[rand.Next(0,5)]]
        code

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* validate *)
///<summary>
/// Validates the guess of the player against the secret code.
/// Increments the number of blacks if the same codeColor is at the same index in both guess and code.
/// Increments the number of whites if the same codeColor exists in guess and code.
/// Subtracts the number of blacks from whites.
///</summary>
///<params name="guess">
/// A code of four elements.
///</params>
///<params name="code">
/// A code of four elements.
///</params>
///<returns>
/// An answer (int * int) - a tuple of integers. Blacks first, whites second.
///</returns>

let validate (guess : code) (code : code) =
    // Count blacks.
    let blacks (guess : code) (code : code) =
        let rec hits l1 l2 =
            match (l1, l2) with
            | ([],_) -> 0
            | (_,[]) -> 0
            | (gh::gt, sh::st) -> if gh = sh then 1 + (hits gt st)
                                  else 0 + (hits gt st)
        let blacks = hits guess code
        blacks

    // Count whites.
    let whites (guess : code) (code : code) =
        let rec notBlacks l1 l2 =
            match (l1, l2) with
            | ([],_) -> []
            | (_,[]) -> []
            | (gh::gt,sh::st) -> if gh = sh then notBlacks gt st
                                 else gh::(notBlacks gt st)
        let sortedNotBlacks = (List.sortBy (id) (notBlacks guess code),
                               List.sortBy (id) (notBlacks code guess))
        let rec intersect (lists : codeColor list * codeColor list) =
            match lists with
            | ([],_) -> 0
            | (_,[]) -> 0
            | (h1::t1, h2::t2) when h1=h2 -> 1 + (intersect (t1,t2))
            | (h1::t1, h2::t2) when h1<h2 -> (intersect (t1,(h2::t2)))
            | (h1::t1, h2::t2) when h1>h2 -> (intersect ((h1::t1), t2))
            | _ -> 666
        intersect sortedNotBlacks

    // Return guess with blacks & whites.
    (((blacks guess code), (whites guess code)))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* printBoard *)
///<summary>
/// Takes a board and creates an equivalent string with column titles.
///</summary>
///<params name="board">
/// A board / (code * answer) list.
///</params>
///<returns>
/// A string.
///</returns>

let printBoard (board : board) =
    let mutable stringBoard = (sprintf "%-6s%-10s%-10s%-10s%-10s %-5s\n----------------------------------------------------\n" "Turn" "Col1" "Col2" "Col3" "Col4" "B, W")
    for i = 0 to board.Length - 1 do   // Løber igennem hvert element, altså tuple, (code * answer) i et board.
        stringBoard <- stringBoard + (sprintf "%-6d" (i+1))
        for j = 0 to 3 do               // Løber gennem det første element, en code, i hver tuple i board og skriver hvert element i code til stringBoard.
            stringBoard <- stringBoard + (sprintf "%-10s" (sprintf "%A" (fst (board.[i])).[j]))
        stringBoard <- stringBoard + (sprintf "%-6s" (sprintf "%A" (snd (board.[i])))) +  "\n" // Løber gennem det andet element, et answer, i hver tuple i board og skriver det til stringBoard
    stringBoard

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// codePermutations is overwritten with the return value of generatePermutations and 
// used by botGuess to narrow down the possible permutations of the secret code.

let config (role:string)  =
    printfn "Choose a %s ([C]omputer/[H]uman):" role
    let choice = ((System.Console.ReadLine ()).ToLower())
    if choice.Length > 0 then
        if choice.[0] = 'c' then
            printfn "%s: Computer\n" role
            Computer
        else
            printfn "%s: Human\n" role
            Human
    else
        printfn "%s: Human\n" role
        Human

let generatePermutations () =
    let cols = [Red; Green; Yellow; Purple; Black; White]
    let mutable (perms:code list) = []
    for i in cols do
        for j in cols do
            for k in cols do
                for l in cols do
                    perms <- [i;j;k;l]::perms
    Set.ofList perms

let mutable validGuess : code Set = Set.empty

let botGuess (currentBoard : board) =
    // Infer & exclude impossible guesses via the previous guess and answer.
    if currentBoard.Length >= 1 then
        let prevGuess = (fst currentBoard.[(currentBoard.Length - 1)])
        let prevAns = (snd currentBoard.[(currentBoard.Length - 1)])
        let remainingPerms = Set.filter (fun permsElement -> (validate prevGuess permsElement) = prevAns) validGuess
        validGuess <- remainingPerms
        let nextGuess = Set.minElement remainingPerms
        nextGuess
    else
        // The standard guess, if no guesses have previously been made.
        [Red;Red;Green;Green]

let guess (player : player) (board : board) =
    if board.Length < 20 then
        if player = Human then
            makeCode (player)
        else
            botGuess (board)
    else
        [Black;Black;Black;Black]


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(*Introduction art and tutorial*)

let startGame () =
    printfn "__          __  __                            _          __  __           _              __  __ _           _ 
 \ \        / / | |                          | |        |  \/  |         | |            |  \/  (_)         | |
  \ \  /\  / /__| | ___ ___  _ __ ___   ___  | |_ ___   | \  / | __ _ ___| |_ ___ _ __  | \  / |_ _ __   __| |
   \ \/  \/ / _ \ |/ __/ _ \| '_ ` _ \ / _ \ | __/ _ \  | |\/| |/ _` / __| __/ _ \ '__| | |\/| | | '_ \ / _` |
    \  /\  /  __/ | (_| (_) | | | | | |  __/ | || (_) | | |  | | (_| \__ \ ||  __/ |    | |  | | | | | | (_| |
     \/  \/ \___|_|\___\___/|_| |_| |_|\___| \__\____/  |_|  |_|\__,_|___/\__\___|_|    |_|  |_|_|_| |_|\__,_|
                                                                                                              
                                                                                                              "
    printfn "%70s" "Press any key to continue.\n"
    // Prevents the program from continuing until the user interacts with the console through their keyboard.
    System.Console.ReadKey() |> ignore
    System.Console.Clear();;


(* tutorial *)
///<summary>
/// A tutorial on how to interact with the program.
///</summary>
let rec tutorial () =
    printfn "Immerse yourself through the power of your keyboard! During this game you will be asked to enter one or several characrs. Your choice is confirmed by pressing Return. Let's try it out!\n"    
    printfn "Enter [M]astermind."
    printfn "(You can either type 'mastermind' or 'm' and press [Return] to confirm)."

    let input = ((System.Console.ReadLine ()).ToLower())
    if input.Length > 0 then
        if input.[0] = 'm' then
            System.Console.Clear()
        else
            printfn "You must type the character 'm' and press [Enter] to continue."
            tutorial ()
    else
        printfn "You must type the character 'm' and press [Enter] to continue."
        tutorial ()


let rec play () =
    System.Console.Clear()

    let codemaker = (config "codemaker")
    let codebreaker = (config "codebreaker")
    let secretCode = (makeCode codemaker)
    System.Console.Clear ()

    printfn "Press any key to start the game!"
    System.Console.ReadKey() |> ignore
    System.Console.Clear()
    
    let mutable masterBoard = []
    let mutable attempts = 12

    while attempts > 0 do
        System.Console.Clear()
        printfn "Attempts left: %A\n" attempts
        printfn "%s" (printBoard masterBoard)
        let mutable guessCode = (guess codebreaker masterBoard)
        let mutable validateGuess = (validate secretCode guessCode)
        masterBoard <- masterBoard @ [(guessCode), (validateGuess)]
        attempts <- attempts - 1

        if validateGuess = (4,0) then
            attempts <- attempts - 13

    if attempts > 0 then
        printfn "%s" (printBoard masterBoard)
        printfn "The secret code was: %A" secretCode
        printfn "Game over! %A won!" codemaker
        replay()
    else
        printfn "%s" (printBoard masterBoard)
        printfn "The secret code was: %A" secretCode
        printfn "Game over! %A won!" codebreaker
        replay()

and replay () =
    printfn "Do you want to play again? ([Y]es/[N]o)"
    let toggle = ((System.Console.ReadLine ()).ToLower())
    if toggle.Length > 0 then
        match toggle with
        | x when x.[0] = 'y' -> validGuess <- generatePermutations (); play ()
        | x when x.[0] = 'n' -> System.Console.Clear(); printfn "Goodbye."
        | _ -> replay ()
    else
        replay ()

        
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* gameFlow *)
///<summary>
/// Initializes and controls the flow of the game by setting dependencies
/// and variables (E.G. codeMaker, codeBreaker, codePermutations, etc.).
/// See comments for details.
///</summary>

let mastermind () =
    startGame()
    tutorial()
    validGuess <- generatePermutations ()
    play()
    // credits()?
