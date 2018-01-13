//optionHelpTest
open Chess
open Pieces
(*
 * Funktion der tager en liste af chessPieces som input, 
 * og returnerer en liste af deres positioner udpakket 
 * af deres option type. Hvis et chessPiece indeholder 
 * en værdi, dvs "Some Position", udpakkes denne 
 * fra sin option type og lægges ind i en ny liste,
 * der returneres. Hvis positionen er "None", bliver
 * den ikke inkluderet i den nye liste.
 *)
let optionHelp (lst : chessPiece list) : Position list =
    let pieces = List.map (fun (x : chessPiece) -> x.position) lst
    let positions = (pieces |> List.choose id) // vælger elementer ud som "har et id", dvs er "Some p"
    positions

// TEST

// Laver et spil
let board = Chess.Board () // bræt
// Brikker i et array
let pieces = [|
  king (White) :> chessPiece;
  rook (White) :> chessPiece;
  king (Black) :> chessPiece;
  rook (Black) :> chessPiece |]
// Putter brikker på brættet
board.[0,0] <- Some pieces.[0]
board.[1,1] <- Some pieces.[1]
board.[4,1] <- Some pieces.[2]
// Laver en liste med alle brikkerne
// Laver den mutable så vi kan tilføje og fjerne brikker i vores tests
let mutable piecesList = board.piecesOnBoard()


// Test med Some Position (hvor alle brikker har en position):
let test1 = 
    let unpackedOrigPos = piecesList |> List.map (fun (x : chessPiece) -> match x.position with Some x -> x | None -> (-100,-100))
    let newPos = optionHelp(piecesList)
    let success : bool = 
        let isSame = List.compareWith (fun elem1 elem2 -> if elem1 = elem2 then 0 else 1) unpackedOrigPos newPos
        if isSame = 0 then
            true
        else
            false
    printfn "Brikker: %A \noriginal pos= %A \noptionHelp pos=%A \ntest succes=%b \n" piecesList unpackedOrigPos newPos success

// Test med en brik der ikke har en position
// Lidt mærkeligt scenarie, men vi vil sikre os, at den ikke kaster en exception
let test2 = 
    piecesList <- [pieces.[3]]
    let unpackedOrigPos = piecesList |> List.map (fun (x : chessPiece) -> match x.position with Some x -> x | None -> (-100,-100))
    let newPos = optionHelp(piecesList)
    //vi forventer at kun værdier med Some Position bliver gemt i den endelige liste.
    //vi fjerner derfor nu værdien med None, så vi kan sammenligne de to lister.
    let goodPos = List.filter (fun (x : Position) -> (fst(x) >= 0) && (snd(x) >= 0)) unpackedOrigPos
    let success : bool = 
        let isSame = List.compareWith (fun elem1 elem2 -> if elem1 = elem2 then 0 else 1) goodPos newPos
        if isSame = 0 then
            true
        else
            false
    printfn "Brik: %A \noriginal pos= %A \noptionHelp pos=%A \ntest succes=%b \n" piecesList unpackedOrigPos newPos success

// Test med en brik + en brik der ikke har en position
// Lidt mærkeligt scenarie, men vi vil sikre os, at den ikke kaster en exception
let test3 = 
    piecesList <- [pieces.[3];pieces.[2]]
    let unpackedOrigPos = piecesList |> List.map (fun (x : chessPiece) -> match x.position with Some x -> x | None -> (-100,-100))
    let newPos = optionHelp(piecesList)
    //vi forventer at kun værdier med Some Position bliver gemt i den endelige liste.
    //vi fjerner derfor nu værdien med None, så vi kan sammenligne de to lister.
    let goodPos = List.filter (fun (x : Position) -> (fst(x) >= 0) && (snd(x) >= 0)) unpackedOrigPos
    let success : bool = 
        let isSame = List.compareWith (fun elem1 elem2 -> if elem1 = elem2 then 0 else 1) goodPos newPos
        if isSame = 0 then
            true
        else
            false
    printfn "Brikker: %A \noriginal pos= %A \noptionHelp pos=%A \ntest succes=%b" piecesList unpackedOrigPos newPos success