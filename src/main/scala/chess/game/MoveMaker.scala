package movemakers

import game._
import scala.util.Random
import scala.collection.mutable.HashSet

trait MoveMaker {
  def color: Player
  def getNextMove(gameState: GameState): Option[Move]
}

class RandomMoveMaker(playerColor: Player) extends MoveMaker {
  override def color: Player = playerColor
  override def getNextMove(gameState: GameState) = {
    val canUsePiece = (piece: Piece, location: Location) => {
      piece.owner == playerColor && !possibleMoves(
        piece,
        location,
        gameState.board
      ).isEmpty
    }

    val boardList = gameState.board.toList
    var seen: HashSet[Int] = HashSet()
    var done = false
    var p: Option[(Location, Piece)] = None
    while (!done) {
      val idx = Random.nextInt(boardList.length)
      if (!seen.contains(idx)) {
        val (location, piece) = boardList(idx)
        if (
          piece.owner == playerColor && !possibleMoves(
            piece,
            location,
            gameState.board
          ).isEmpty
        ) {
          done = true
          p = Some((location, piece))
          seen.add(idx)
        }

        if (boardList.length == seen.size) {
          done = true
        }
      }
    }

    p map { case (location, piece) =>
      val moves = possibleMoves(piece, location, gameState.board)
      val movesList = moves.toList
      val res = movesList(Random.nextInt(movesList.length))
      Move(location, res)
    }
  }
}
