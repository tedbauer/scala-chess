import scala.collection.immutable.HashMap
import scala.collection.mutable.HashSet

package object game {

  sealed trait Player
  case object Black extends Player
  case object White extends Player

  sealed trait PieceType
  case object Queen extends PieceType
  case object King extends PieceType
  case object Rook extends PieceType
  case object Bishop extends PieceType
  case object Knight extends PieceType
  case object Pawn extends PieceType

  case class Piece(pieceType: PieceType, owner: Player)

  // col: 1-8 incl
  // row: a-h incl
  case class Location(col: Int, row: Char)

  type Board = Map[Location, Piece]

  type Prison = List[Piece]

  case class GameState(
      board: Board,
      currentTurn: Player,
      whitePrison: Prison,
      blackPrison: Prison
  )

  sealed trait MoveResult
  case class ValidMove(s: GameState) extends MoveResult
  case object InvalidMove extends MoveResult

  case class Move(src: Location, dst: Location)

  type PossibleMovesMap = Map[Piece, Set[Location]]

  val initialBoard: Board = HashMap(
    Location(2, 'a') -> Piece(Pawn, White),
    Location(2, 'b') -> Piece(Pawn, White),
    Location(2, 'c') -> Piece(Pawn, White),
    Location(2, 'd') -> Piece(Pawn, White),
    Location(2, 'e') -> Piece(Pawn, White),
    Location(2, 'f') -> Piece(Pawn, White),
    Location(2, 'g') -> Piece(Pawn, White),
    Location(2, 'h') -> Piece(Pawn, White),
    Location(1, 'a') -> Piece(Rook, White),
    Location(1, 'b') -> Piece(Bishop, White),
    Location(1, 'c') -> Piece(Knight, White),
    Location(1, 'd') -> Piece(Queen, White),
    Location(1, 'e') -> Piece(King, White),
    Location(1, 'f') -> Piece(Knight, White),
    Location(1, 'g') -> Piece(Bishop, White),
    Location(1, 'h') -> Piece(Rook, White),
    Location(7, 'a') -> Piece(Pawn, Black),
    Location(7, 'b') -> Piece(Pawn, Black),
    Location(7, 'c') -> Piece(Pawn, Black),
    Location(7, 'd') -> Piece(Pawn, Black),
    Location(7, 'e') -> Piece(Pawn, Black),
    Location(7, 'f') -> Piece(Pawn, Black),
    Location(3, 'd') -> Piece(King, Black), //delete me
    Location(7, 'g') -> Piece(Pawn, Black),
    Location(7, 'h') -> Piece(Pawn, Black),
    Location(8, 'a') -> Piece(Rook, Black),
    Location(8, 'b') -> Piece(Bishop, Black),
    Location(8, 'c') -> Piece(Knight, Black),
    Location(8, 'd') -> Piece(Queen, Black),
    Location(8, 'e') -> Piece(King, Black),
    Location(8, 'f') -> Piece(Knight, Black),
    Location(8, 'g') -> Piece(Bishop, Black),
    Location(8, 'h') -> Piece(Rook, Black)
  )

  val initialGame: GameState = GameState(initialBoard, White, List(), List())

  private def flipPlayer(player: Player): Player = {
    player match {
      case White => Black
      case Black => White
    }
  }

  def possibleMoves(
      piece: Piece,
      location: Location,
      board: Board
  ): HashSet[Location] = {
    val moves: HashSet[Location] = HashSet()

    val spotIsEmpty = (i: Int, j: Int) => {
      (location.col + i) <= 8 && (location.row.toInt - 96 + j) <= 8 &&
        (location.col + i) >= 1 && (location.row.toInt - 96 + j) >= 1 &&
        board
          .get(Location(location.col + i, (location.row + j).toChar))
          .isEmpty
    }

    val addMove = (i: Int, j: Int) =>
      moves.add(Location(location.col + i, (location.row + j).toChar))

    val attackableTarget = (i: Int, j: Int, targetPlayer: Player) => {
      board
        .get(Location(location.col + i, (location.row + j).toChar))
        .exists(_.owner == targetPlayer)
    }

    // todo: turn this into a cool fold
    val addDiagonalLocations = () => {
      var i = 1;
      i = 1; while (spotIsEmpty(i, i)) { addMove(i, i); i += 1 }
      i = 1; while (spotIsEmpty(i, -i)) { addMove(i, -i); i += 1 }
      i = 1; while (spotIsEmpty(-i, i)) { addMove(-i, i); i += 1 }
      i = 1; while (spotIsEmpty(-i, -i)) { addMove(-i, -i); i += 1 }
    }

    val addHorizontalAndVerticalLocations = () => {
      var i = 1;
      i = 1; while (spotIsEmpty(i, 0)) { addMove(i, 0); i += 1 }
      i = 1; while (spotIsEmpty(0, i)) { addMove(0, i); i += 1 }
      i = 1; while (spotIsEmpty(-i, 0)) { addMove(-i, 0); i += 1 }
      i = 1; while (spotIsEmpty(0, -i)) { addMove(0, -i); i += 1 }
    }

    // TODO: make a helper that's something like, addVerticals/addDiags/addHorizs
    piece.pieceType match {
      case Bishop =>
        addDiagonalLocations()
      case Rook =>
        addHorizontalAndVerticalLocations()
      case Queen =>
        addHorizontalAndVerticalLocations()
        addDiagonalLocations()
      case King =>
        val offsets = List(-1, 0, 1)
        for (offsetRow <- offsets) {
          for (offsetCol <- offsets) {
            addMove(offsetRow, offsetCol)
          }
        }
      case Pawn =>
        piece.owner match {
          case White => {
            addMove(1, 0)
            if (attackableTarget(1, 1, Black)) addMove(1, 1)
            if (attackableTarget(1, -1, Black)) addMove(1, -1)
          }
          case Black => {
            addMove(-1, 0)
            if (attackableTarget(-1, 1, White)) addMove(-1, 1)
            if (attackableTarget(-1, -1, White)) addMove(-1, -1)
          }
        }
      case Knight =>
        val offsets = List(-2, -1, 1, 2)
        for (offsetRow <- offsets) {
          for (offsetCol <- offsets) {
            if (offsetRow.abs != offsetCol.abs) {
              addMove(offsetRow, offsetCol)
            }
          }
        }
    }
    println(moves)
    moves
  }

  private def legalMove(state: GameState, move: Move): Boolean = {
    state.board.get(move.src) map { srcPiece =>
      srcPiece.owner == state.currentTurn && possibleMoves(
        srcPiece,
        move.src,
        state.board
      ).contains(move.dst)
    } getOrElse false
  }

  def makeMove(state: GameState, move: Move): MoveResult = {
    state.board.get(move.src) map { srcPiece =>
      state.board.get(move.dst) map { dstPiece =>
        if (dstPiece.owner == state.currentTurn) {
          InvalidMove
        } else {
          if (legalMove(state, move)) {
            ValidMove(
              GameState(
                (state.board - move.dst).updated(move.dst, srcPiece) - move.src,
                flipPlayer(state.currentTurn),
                if (state.currentTurn == White) dstPiece :: state.whitePrison
                else state.whitePrison,
                if (state.currentTurn == Black) dstPiece :: state.blackPrison
                else state.blackPrison
              )
            )
          } else {
            InvalidMove
          }
        }
      } getOrElse {
        if (legalMove(state, move)) {
          ValidMove(
            GameState(
              state.board.updated(move.dst, srcPiece) - move.src,
              flipPlayer(state.currentTurn),
              state.whitePrison,
              state.blackPrison
            )
          )
        } else { InvalidMove }
      }
    } getOrElse InvalidMove
  }
}
