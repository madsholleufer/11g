open Chess
open Pieces
open Players
open Game

let test1 = // Create a game
    let board = Chess.Board () // Create a board
    // Pieces are kept in an array for easy testing
    let pieces = [|
      king (White) :> chessPiece;
      rook (White) :> chessPiece;
      king (Black) :> chessPiece |]
    // Place pieces on the board
    board.[2,2] <- Some pieces.[0]
    board.[3,1] <- Some pieces.[1]
    board.[4,2] <- Some pieces.[2]

    //human vs pc
    // initialiserer spillere
    let spiller1 = new Human(Black)
    let spiller2 = new Computer(White)
    // initialiserer spillet
    let spil = new Game(spiller1, spiller2)
    spil.run(board, pieces)

let test2 =
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

    //human vs pc
    // initialiserer spillere
    let spiller1 = new Human(Black)
    let spiller2 = new Computer(White)
    // initialiserer spillet
    let spil = new Game(spiller1, spiller2)
    spil.run(board, pieces)

let test3 =
    let board = Chess.Board () // Create a board
    // Pieces are kept in an array for easy testing
    let pieces = [|
      king (White) :> chessPiece;
      rook (White) :> chessPiece;
      king (Black) :> chessPiece |]
    // Place pieces on the board
    board.[0,0] <- Some pieces.[0]
    board.[7,7] <- Some pieces.[1]
    board.[7,0] <- Some pieces.[2]

    //human vs pc
    // initialiserer spillere
    let spiller1 = new Human(Black)
    let spiller2 = new Computer(White)
    // initialiserer spillet
    let spil = new Game(spiller1, spiller2)
    spil.run(board, pieces)

let test4 =
    let board = Chess.Board () // Create a board
    // Pieces are kept in an array for easy testing
    let pieces = [|
      king (White) :> chessPiece;
      rook (White) :> chessPiece;
      king (Black) :> chessPiece |]
    // Place pieces on the board
    board.[0,7] <- Some pieces.[0]
    board.[0,0] <- Some pieces.[1]
    board.[7,0] <- Some pieces.[2]

    //human vs pc
    // initialiserer spillere
    let spiller1 = new Human(Black)
    let spiller2 = new Computer(White)
    // initialiserer spillet
    let spil = new Game(spiller1, spiller2)
    spil.run(board, pieces)
