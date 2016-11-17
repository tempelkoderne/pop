// printBoard : board -> string
// Hjælpefunktion.

type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int // sort * hvid
type board = (code * answer) list
type player = Human | Computer

// Omdanner et board til en string, som kan printes.
let printBoard (aBoard : board) =
    let mutable stringBoard = (sprintf "%-6s%-10s%-10s%-10s%-10s %-5s\n----------------------------------------------------\n" "Turn" "Col1" "Col2" "Col3" "Col4" "B, W")
    for i = 0 to aBoard.Length - 1 do   // Løber igennem hvert element, altså tuple, (code * answer) i et board.
        stringBoard <- stringBoard + (sprintf "%-6d" (i+1))
        for j = 0 to 3 do               // Løber gennem det første element, en code, i hver tuple i board og skriver hvert element i code til stringBoard.
            stringBoard <- stringBoard + (sprintf "%-10s" (sprintf "%A" (fst (aBoard.[i])).[j]))
        stringBoard <- stringBoard + (sprintf "%-6s" (sprintf "%A" (snd (aBoard.[i])))) +  "\n" // Løber gennem det andet element, et answer, i hver tuple i board og skriver det til stringBoard
    stringBoard

// Et board til test af printBoard.
let testBoard : board = [([Red;Red;Green;Green],(1,1));
                        ([Yellow;Yellow;Purple;Purple],(1,1));
                        ([White;Black;Purple;Purple],(0,0));]

// Tester printBoard med testBoard.
printfn "%s" (printBoard testBoard)