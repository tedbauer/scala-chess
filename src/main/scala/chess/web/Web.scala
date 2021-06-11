package chess.web

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html

import game._

object Web {

  type Ctx2D = dom.CanvasRenderingContext2D

  val CanvasLength = 500
  val Padding = .2
  val BoardCoords = ((Padding / 2) * CanvasLength, (Padding / 2) * CanvasLength)
  val BoardLength = (1 - Padding) * CanvasLength
  val GridLength = 8
  val TileSize = BoardLength.toFloat / GridLength

  sealed trait Command
  case object DoNothing extends Command
  case class SelectTile(tile: (Int, Int)) extends Command
  case class SubmitMove(tile1: (Int, Int), tile2: (Int, Int)) extends Command

  sealed trait ViewState
  case object NothingSelected extends ViewState
  case class TileSelected(tile: (Int, Int)) extends ViewState

  def main(args: Array[String]): Unit = {
    document.addEventListener(
      "DOMContentLoaded",
      { (e: dom.Event) =>
        val canvas = document
          .createElement("canvas")
          .asInstanceOf[html.Canvas]
        canvas.width = CanvasLength
        canvas.height = CanvasLength
        val canvasCtx = canvas
          .getContext("2d")
          .asInstanceOf[Ctx2D]

        val canvasRect = canvas.getBoundingClientRect()
        val canvasLeftTop = (canvasRect.left, canvasRect.top)

        document.body.appendChild(canvas)

        var gameState: GameState = initialGame
        var viewState: ViewState = NothingSelected
        drawGame(canvasCtx, gameState, viewState)

        canvas.onclick = { (e: dom.MouseEvent) =>
          val command = getCommand(e, canvasLeftTop, viewState)
          gameState = updateGameState(gameState, command)
          viewState = updateViewState(viewState, command)

          drawGame(canvasCtx, gameState, viewState)
        }

      }
    )
  }

  def getCommand(
      e: dom.MouseEvent,
      canvasLeftTop: (Double, Double),
      viewState: ViewState
  ): Command = {
    viewState match {
      case NothingSelected => SelectTile(findClickedTile(e, canvasLeftTop))
      case TileSelected(prevTile) =>
        SubmitMove(prevTile, findClickedTile(e, canvasLeftTop))
    }
  }

  def updateViewState(viewState: ViewState, command: Command): ViewState = {
    (viewState, command) match {
      case (NothingSelected, SelectTile(tile)) => TileSelected(tile)
      case (TileSelected(_), SelectTile(_))    => println("wat"); NothingSelected

      case (TileSelected(_), SubmitMove(_, _)) =>
        println("impossible"); NothingSelected // Impossible
      case (NothingSelected, SubmitMove(_, _)) =>
        println("impossible"); NothingSelected // Impossible

      case (s, DoNothing) => s
    }
  }

  def tileToLocation(tile: (Int, Int)): Location = {
    Location(GridLength - tile._2, (tile._1 + 97).toChar)
  }

  def submitMove(
      tile1: (Int, Int),
      tile2: (Int, Int),
      gameState: GameState
  ): GameState = {
    val src = tileToLocation(tile1)
    val dst = tileToLocation(tile2)

    println(src);
    println(dst);

    move(gameState, src, dst) match {
      case InvalidMove             => gameState
      case ValidMove(newGameState) => newGameState
    }
  }

  def updateGameState(gameState: GameState, command: Command): GameState = {
    command match {
      case DoNothing     => gameState
      case SelectTile(_) => gameState
      case SubmitMove(t1, t2) =>
        println(t1); println(t2); submitMove(t1, t2, gameState)
    }
  }

  def pieceToString(p: Piece): String = {
    p.pieceType match {
      case Queen  => "Queen"
      case King   => "King"
      case Rook   => "Rook"
      case Bishop => "Bishop"
      case Knight => "Knight"
      case Pawn   => "Pawn"
    }
  }

  def findClickedTile(
      e: dom.MouseEvent,
      canvasLeftTop: (Double, Double)
  ): (Int, Int) = {
    val x = e.clientX - canvasLeftTop._1
    val y = e.clientY - canvasLeftTop._2
    println("==")
    println(x)
    println(y)
    println(canvasLeftTop._2)
    println("==")
    val res = (
      ((e.clientX - canvasLeftTop._1 - BoardCoords._1) / TileSize).toInt,
      ((e.clientY - canvasLeftTop._2 - BoardCoords._2) / TileSize).toInt
    )
    println("clicked tile:")
    println(res)
    return res
  }

  def drawGame(
      ctx: Ctx2D,
      gameState: GameState,
      viewState: ViewState
  ) = {

    ctx.clearRect(0, 0, CanvasLength, CanvasLength)

    ctx.strokeStyle = "black"
    ctx.lineWidth = 2
    ctx.beginPath()

    ctx.rect(0, 0, CanvasLength, CanvasLength)

    var i = 0
    var j = 0
    for (i <- 0 until GridLength) {
      for (j <- 0 until GridLength) {
        ctx.fillText(
          (GridLength - j).toString(),
          BoardCoords._1 / 2,
          j * TileSize + BoardCoords._2 + TileSize / 2
        )

        ctx.fillText(
          (i + 97).toChar.toString,
          i * TileSize + BoardCoords._1 + TileSize / 2,
          BoardCoords._1 / 2
        )
        ctx.rect(
          BoardCoords._1 + i * TileSize,
          BoardCoords._2 + j * TileSize,
          TileSize,
          TileSize
        )

        if (i % 2 != 0 && j % 2 == 0 || i % 2 == 0 && j % 2 != 0) {
          ctx.fillRect(
            i * TileSize + BoardCoords._1,
            j * TileSize + BoardCoords._2,
            TileSize,
            TileSize
          )
        }

        viewState match {
          case NothingSelected => ()
          case TileSelected(tile) =>
            if ((i, j) == tile) {
              ctx.fillStyle = "green"
              ctx.fillRect(
                i * TileSize + BoardCoords._1,
                j * TileSize + BoardCoords._2,
                TileSize,
                TileSize
              )
              ctx.fillStyle = "black"
            }
        }

        val piece = gameState.board.get(tileToLocation((i, j)))
        piece match {
          case Some(p) =>
            ctx.fillStyle = "brown"
            ctx.fillText(
              pieceToString(p),
              i * TileSize + BoardCoords._1 + TileSize / 4,
              (j + 1) * TileSize + BoardCoords._2 - TileSize / 2
            )
            ctx.fillStyle = "black"
          case None => ()
        }
      }
    }

    ctx.stroke()
  }
}
