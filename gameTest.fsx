//gameTest
open Chess
open Pieces
open Players
open Game

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

let test1 = //human vs pc
    // initialiserer spillere
    let spiller1 = new Human(Black)
    let spiller2 = new Computer(White)
    // initialiserer spillet
    // Test med en human og en computer.
    let spil = new Game(spiller1, spiller2)
    spil.run(board, pieces)

// Laver et nyt spil til test 2
let board2 = Chess.Board () // Create a board
// Pieces are kept in an array for easy testing
let pieces2 = [|
  king (White) :> chessPiece;
  rook (White) :> chessPiece;
  king (Black) :> chessPiece |]
// Place pieces on the board
board2.[0,0] <- Some pieces2.[0]
board2.[3,1] <- Some pieces2.[1]
board2.[4,1] <- Some pieces2.[2]

let test2 = //human vs human
    // initialiserer spillere
    let spiller1 = new Human(Black)
    let spiller2 = new Human(White)
    // initialiserer spillet
    let spil2 = new Game(spiller1, spiller2)
    spil2.run(board2, pieces2)