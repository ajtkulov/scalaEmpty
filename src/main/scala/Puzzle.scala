package puzzle

import Types._

object Types {
  type Value = Int
  type Index = Int
  val size: Int = 4
}

case class Cell(row: Index, col: Index) {
  protected def inSide: Boolean = row >= 0 && row < size && col >= 0 && col < size

  def neighbors: List[Cell] = List(
    copy(row = row + 1),
    copy(row = row - 1),
    copy(col = col + 1),
    copy(col = col - 1)
  ).filter(_.inSide)

  def rawIndex: Int = {
    row * size + col
  }
}

object Cell {
  def fromIdx(idx: Index): Cell = {
    Cell(idx / size, idx % size)
  }
}

object State {
  def fromList(values: List[List[Value]]): State = {
    State(values.flatten.toArray)
  }

  lazy val finalState: State = fromList(
    List(
      List(1, 2, 3, 4),
      List(5, 6, 7, 8),
      List(9, 10, 11, 12),
      List(13, 14, 15, 0)
    ))
}

case class HistoryState(value: State, prev: Cell) {}

case class State(values: Array[Value]) {
  require(values.length == size * size)

  lazy val free: Cell = {
    val idx = values.zipWithIndex.find {
      case (value, _) => value == 0
    }.get._2

    Cell.fromIdx(idx)
  }

  def cell(cell: Cell): Value = {
    values(cell.rawIndex)
  }

  def neighbors: List[HistoryState] = {
    free.neighbors.map(x => HistoryState(move(x), x))
  }

  protected def change(fst: Cell, snd: Cell): State = {
    val f = fst.rawIndex
    val s = snd.rawIndex
    val t = values(f)
    values(f) = values(s)
    values(s) = t
    this
  }

  def move(newCell: Cell): State = {
    val newState = copy(values = values.clone())
    newState.change(newCell, free)
  }

  override def toString: String = {
    val res = for (row <- 0 until 4) yield {
      val line = for (col <- 0 until 4) yield {
        cell(Cell(row, col))
      }

      line.mkString(" ")
    }

    res.mkString("\n")
  }
}

trait Strategy {
  def distance(value: State): Int
}

object Strategies {
  def manhattanDistance(fst: Cell, snd: Cell): Int = {
    Math.abs(fst.row - snd.row) + Math.abs(fst.col - snd.col)
  }

  val simplePrefix: Strategy = new Strategy {
    override def distance(state: State): Int = {
      val res = state.values.zipWithIndex.takeWhile {
        case (value, idx) => value == idx
      }.length

      res - size * size
    }
  }

  val manhattan: Strategy = new Strategy {
    override def distance(state: State): Int = {
      val res = for (row <- 0 until 4; col <- 0 until 4; if !(row == 4 && col == 4)) yield {
        val cell = Cell(row, col)
        val value = state.cell(cell)
        val bestCell = Cell.fromIdx(value - 1)
        manhattanDistance(cell, bestCell)
      }

      res.sum
    }
  }

  val combine: Strategy = new Strategy {
    override def distance(value: State): Int = {
      simplePrefix.distance(value) + manhattan.distance(value) * 10
    }
  }
}

object Puzzle {
  def solution(state: State) = {

  }
}



