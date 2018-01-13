// playerTest
open Chess
open Pieces
open Players
/// Print various information about a piece
let printPiece (board : Board) (p : chessPiece) : unit =
  printfn "%A: %A %A" p p.position (p.availableMoves board)

// Create a game
let board = Chess.Board () // Create a board
// Pieces are kept in an array for easy testing
let pieces = [|
  king (White) :> chessPiece;
  rook (White) :> chessPiece;
  king (Black) :> chessPiece |]
// Place pieces on the board
board.[0,0] <- Some pieces.[0]
board.[3,1] <- Some pieces.[1]
board.[4,1] <- Some pieces.[2]
//Show board
printfn "%A" board
Array.iter (printPiece board) pieces

let test1 = 
    // Test af at Human fungerer
    let spiller1 = new Human(Black)
    spiller1.nextMove(board)

let test2 = 
    // Test af at Computer fungerer
    let spiller2 = new Computer(White)
    // Printer computerens codestring til konsollen.
    printfn "Computerens codestring: %A" (spiller2.nextMove(board))
