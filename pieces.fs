module Pieces
open Chess

(*
 * Definerer en metode, der tager en liste af skakbrikker
 * som input og pakker hver briks position ud af dens 
 * option type. På denne måde får vi udtrukket brikkernes 
 * positionener.
 *)
let optionHelp (lst : chessPiece list) : Position list =
   let pieces = List.map (fun (x : chessPiece) -> x.position) lst
   let positions = (pieces |> List.choose id) // vælger elementer ud som "har et id", dvs er "Some p"
   positions

/// A king is a chessPiece which moves 1 square in any direction
type king(col : Color) =
  inherit chessPiece(col)
  override this.nameOfType = "king"
  // king has runs of 1 in 8 directions: (N, NE, E, SE, S, SW, W, NW)
  override this.candiateRelativeMoves =
      [[(-1,0)];[(-1,1)];[(0,1)];[(1,1)];
      [(1,0)];[(1,-1)];[(0,-1)];[(-1,-1)]]
  // Ændrer i implementationen af availableMoves
  override this.availableMoves (board : Board) : (Position list * chessPiece list) = 
    // Gemmer de mulige moves for kongen
    let moves : (Position list * chessPiece list) = board.getVacantNNeighbours this
    // Henter alle brikker på brættet
    let piecesList = board.piecesOnBoard()
    // Filtrerer modstanderbrikkerne ud i en liste
    let opponentPiecesList = piecesList |> List.filter (fun (x : chessPiece) -> (this.color <> x.color))
    //Nu laver vi en liste af tupler med deres available moves
    let opponentAvailableMoves = List.map (fun x -> board.getVacantNNeighbours x) (opponentPiecesList)
    //Udtrækker positionerne
    let opponentAvailablePositions = List.map (fun x -> fst(x)) opponentAvailableMoves
    //Samler til én position list i stedet for en Position list list
    let opPos1 = List.concat opponentAvailablePositions
    //Udtrækker brikkerne
    let opponentPiecesToKill = List.map (fun x -> snd(x)) opponentAvailableMoves
    //Samler til én chessPiece list i stedet for en chessPiece list list
    let opPos2 = List.concat opponentPiecesToKill

    // Nu tjekker vi kongens moves op imod modstandermoves og tager dem ud, der IKKE overlapper
    let a = List.filter (fun (x : Position) -> not (List.contains x opPos1)) (fst(moves))
    let b = List.filter (fun (x : chessPiece) -> not (List.contains x opPos2)) (snd(moves))
    (a,b)

/// A rook is a chessPiece which moves horisontally and vertically
type rook(col : Color) =
  inherit chessPiece(col)
  // rook can move horisontally and vertically
  // Make a list of relative coordinate lists. We consider the
  // current position and try all combinations of relative moves
  // (1,0); (2,0) ... (7,0); (-1,0); (-2,0); ...; (0,-7).
  // Some will be out of board, but will be assumed removed as
  // illegal moves.
  // A list of functions for relative moves
  let indToRel = [
    fun elm -> (elm,0); // South by elm
    fun elm -> (-elm,0); // North by elm
    fun elm -> (0,elm); // West by elm
    fun elm -> (0,-elm) // East by elm
    ]
  // For each function in indToRel, we calculate List.map f [1..7].
  // swap converts (List.map fct indices) to (List.map indices fct).
  let swap f a b = f b a
  override this.candiateRelativeMoves =
    List.map (swap List.map [1..7]) indToRel (*//§\label{chessPieceSwapApp}§*)
  override this.nameOfType = "rook"