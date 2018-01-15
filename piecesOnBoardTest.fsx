// Test af funktionen piecesOnBoard. Vi ser om den returnerer en liste af chessPieces.
open Chess
open Pieces


//TEST
(*
 * Vi forventer at piecesOnBoard returnerer indholdet
 * af pieces, eftersom alle brikker er blevet placeret
 * på brættet (linje 13-15). Dog er pieces defineret
 * som et array, så vi konverterer først arrayet til
 * en liste. Vores tests består af at undersøge om 
 * hvert element i de forventede resultater er 
 * inkluderet i resultatet af piecesOnBoard.
*)

let test1 = 
  // Create a game
  let board = Chess.Board () // Create a board
  // Pieces are kept in an array for easy testing
  let pieces = [|
    king (White) :> chessPiece;
    rook (White) :> chessPiece;
    king (Black) :> chessPiece |]
  // Place pieces on the board
  board.[0,0] <- Some pieces.[0]
  board.[1,1] <- Some pieces.[1]
  board.[4,1] <- Some pieces.[2]

  let exp = pieces |> List.ofArray
  let res = board.piecesOnBoard()
  let test1 = List.map (fun x -> List.contains x exp) res
  printfn "Brikker på brættet: %A, exp=%A, success=%A" res exp (test1)

let test2 = 
  //Vi fjerner nu en brik fra brættet og ser om funktionen stadig virker
  // Create a game
  let board = Chess.Board () // Create a board
  // Pieces are kept in an array for easy testing
  let pieces = [|
    king (White) :> chessPiece;
    king (Black) :> chessPiece |]
  // Place pieces on the board
  board.[0,0] <- Some pieces.[0]
  board.[1,1] <- Some pieces.[1]
  //defaulten er None når der ikke er placeret en brik, så vi tester automatisk for dette

  let exp = pieces |> List.ofArray
  let res = board.piecesOnBoard()
  let test = List.map (fun x -> List.contains x exp) res
  printfn "Brikker på brættet: %A, exp=%A, success=%A" res exp (test)
