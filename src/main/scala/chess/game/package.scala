import scala.collection.immutable.HashMap

package object game {

  sealed trait Piece
  case object Queen extends Piece
  case object King extends Piece
  case object Rook extends Piece
  case object Bishop extends Piece
  case object Knight extends Piece
  case object Pawn extends Piece

  sealed trait Player
  case object Black extends Player
  case object White extends Player

  // col: 1-8 incl
  // row: a-h incl
  case class Location(col: Int, row: Char)

  type Board = Map[Location, (Piece, Player)]

  type Prison = List[Piece]

  /** (board, which turn it is, white's taken pieces, black's taken pieces)
    */
  type GameState = (Board, Player, Prison, Prison)

  sealed trait MoveResult
  case class ValidMove(s: GameState) extends MoveResult
  case object InvalidMove extends MoveResult

  val initialBoard: Board = HashMap(
    Location(2, 'a') -> (Pawn, White),
    Location(2, 'b') -> (Pawn, White),
    Location(2, 'c') -> (Pawn, White),
    Location(2, 'd') -> (Pawn, White),
    Location(2, 'e') -> (Pawn, White),
    Location(2, 'f') -> (Pawn, White),
    Location(2, 'g') -> (Pawn, White),
    Location(2, 'h') -> (Pawn, White),
    Location(1, 'a') -> (Rook, White),
    Location(1, 'b') -> (Bishop, White),
    Location(1, 'c') -> (Knight, White),
    Location(1, 'd') -> (Queen, White),
    Location(1, 'e') -> (King, White),
    Location(1, 'f') -> (Knight, White),
    Location(1, 'g') -> (Bishop, White),
    Location(1, 'h') -> (Rook, White)
  )

  private def posChangeIsLegal(
      piece: Piece,
      src: Location,
      dst: Location
  ): Boolean = {
    piece match {
      case Bishop =>
        (dst.col - src.col.toInt).abs == (dst.row - src.row.toInt).abs
      case _ => false
    }
  }

  private def flipPlayer(player: Player): Player = {
    player match {
      case White => Black
      case Black => White
    }
  }

  def move(state: GameState, src: Location, dst: Location): MoveResult = {
    val (board, turn, whiteTakes, blackTakes) = state

    // find the piece in srcLoc.
    // if there is no piece, invalidmove.
    // if there is one, see if the dstloc is a legal one.
    // if it is, validmove/make appropriate updates to prisons/turn
    // otherwise, invalidmove.

    val piece = board.get(src)
    if (piece.isEmpty || !posChangeIsLegal(piece.get._1, src, dst)) {
      return InvalidMove
    }

    // if the dst piece is the same as whose turn it is, the move is illegal
    //
    ValidMove(
      (
        board.updated(dst, (piece.get._1, turn)),
        flipPlayer(turn),
        whiteTakes,
        blackTakes
      )
    )

  }

}
