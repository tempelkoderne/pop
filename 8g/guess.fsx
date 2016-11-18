// guess : player -> board -> code
// som tager en spillertype, et bræt bestående af et spils tidligere gæt og svar og returnerer et nyt gæt enten ved input fra brugeren eller ved at programmet beregner et gæt.

type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int // sort * hvid
type board = (code * answer) list
type player = Human | Computer

(*Diverse funktioner / dependencies er importeret hertil. guess står nederst*)


// Dependency for guess.
let generateCode () =
    let colors = [Red; Green; Yellow; Purple; White; Black]
    let rand = System.Random()
    let code = [colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)]]
    code

// Dependency for guess.
let endGame () =
    printfn "GAME OVER"
    [Red;Red;Red;Red] // placeholder



// Håndterer og retter human input. Spørger ved tvivlstilfælde.
let enterCode () =
    let rec inputCode() =
        printfn "Pick your colors!"
        printfn "([R]ed; [G]reen; [Y]ellow; [P]urple; [W]hite; [B]lack)"
        let input = System.Console.ReadLine().ToLower()
        let sep = [|" "; "; "; ", "; ";"; ","; "-"|]
        let splinput = input.Split (sep, System.StringSplitOptions.None)
        if splinput.Length <> 4 then
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
        | h::t -> printfn "What did you mean by \"%s\"?" h;
                  formatCode (System.Console.ReadLine().ToLower()::t)
    inputCode()


 
// guess : Tager en playertype og board for det nuværende spil og kalder en passende funktion, der genererer en kode.
let guess (playerType : player) (currentBoard : board) =
    if currentBoard.Length < 20 && playerType = Human then
        enterCode ()
    elif currentBoard.Length < 20 && playerType = Computer then
        generateCode ()
    else
        endGame ()
let testBoard = [([Red;Red;Red;Red],(0,0))]


// Tester
let testPlayer = Human
printfn "%A" (guess testPlayer testBoard)