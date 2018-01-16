// playerTest. 

open Chess
open Pieces
open Players

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

let test1 = 
    printfn "Test 1:"
    // Test af at Human fungerer
    let spiller1 = new Human(Black)
    let spiller2 = new Human(White)
    // har de en farve?
    let exp = [|Black ; White|]
    let col1 = spiller1.getColor()
    let col2 = spiller2.getColor()
    printfn "Spiller 1s farve: %A, Spiller 2s farve: %A, success= %b" col1 col2 (col1 = exp.[0] && col2 = exp.[1])
    // har de et spillernr (playernumber)?
    let exp = [|1 ; 2|]
    let no1 = spiller1.playerNumber
    let no2 = spiller2.playerNumber
    printfn "Spiller 1s spillernr: %A, Spiller 2s spillernr: %A, success= %b" no1 no2 (no1 = exp.[0] && no2 = exp.[1])

let test2 = 
    printfn "\nTest 2:"
    // Test af at Computer fungerer
    let spiller1 = new Human(Black)
    let spiller2 = new Computer(White)
    // har de en farve?
    let exp = [|Black ; White|]
    let col1 = spiller1.getColor()
    let col2 = spiller2.getColor()
    printfn "Spiller 1s farve: %A, Spiller 2s farve: %A, success= %b" col1 col2 (col1 = exp.[0] && col2 = exp.[1])
    // har de et spillernr (playernumber)?
    let exp = [|1 ; 2|]
    let no1 = spiller1.playerNumber
    let no2 = spiller2.playerNumber
    printfn "Spiller 1s spillernr: %A, Spiller 2s spillernr: %A, success= %b" no1 no2 (no1 = exp.[0] && no2 = exp.[1])

let test3 = 
    printfn "\nTest 3:"
    // Test af at Computer fungerer
    let spiller1 = new Computer(Black)
    let spiller2 = new Computer(White)
    // har de en farve?
    let exp = [|Black ; White|]
    let col1 = spiller1.getColor()
    let col2 = spiller2.getColor()
    printfn "Spiller 1s farve: %A, Spiller 2s farve: %A, success= %b" col1 col2 (col1 = exp.[0] && col2 = exp.[1])
    // har de et spillernr (playernumber)?
    let exp = [|1 ; 2|]
    let no1 = spiller1.playerNumber
    let no2 = spiller2.playerNumber
    printfn "Spiller 1s spillernr: %A, Spiller 2s spillernr: %A, success= %b" no1 no2 (no1 = exp.[0] && no2 = exp.[1])

