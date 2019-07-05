package main

import java.awt.image.BufferedImage

import Handler._
import main.Handler.{rotate, rotationAngle}

import scala.util.Try

object Model {
  type M = List[List[ItemInfo]]

  def toString1(m: M): String = {
    m.map(_.mkString(" / ")).mkString("\n")
  }

  def reflect(model: M): M = {
    model.map(_.reverse).reverse
  }

  def inside(model: M, pos: Pos): Boolean = {
    Try {
      model(pos.y)(pos.x)
    }.isSuccess
  }

  def read(fileName: String) = {
    val input: M = scala.io.Source.fromFile(fileName).getLines().toList.map(_.filterNot(_ == ' ').split("/").toList.map(toItem))
    input
  }

  def toItem(str: String): ItemInfo = {
    str.split("\\.").toList.map(_.toInt) match {
      case num :: Nil => ItemInfo(num, None, None)
      case num :: idx :: Nil => ItemInfo(num, Some(idx), None)
      case num :: idx :: rot :: Nil => ItemInfo(num, Some(idx), Some(rot))
    }
  }

  def draw(values: M): BufferedImage = {
    val res = new BufferedImage(2 * 2048, 2 * 2048, BufferedImage.TYPE_INT_RGB)


    for {i <- values.indices
         list = values(i)
         j <- list.indices
         z = values(i)(j)
         if z.full
         img = rotateItem(Holder.r(Main.pairToIdx(z.num, z.idx.get)), z.rotation.get)
    } {
      for {
        x <- 100 to 900
        y <- 100 to 900
      } {
        val c = img.getColor(x, y)
        if (Handler.nonEmpty(c)) {
          res.setRGB(j * 500 + x, i * 500 + y, img.getRGB(x, y))
        }
      }
    }
    res
  }

  def rotateItem(fw: WItem, rotation: Int) = {

    val fst = fw.item

    val line1 = fst.line2((rotation + 2) % 4)
    val r1 = rotationAngle(line1.fst, line1.snd)
    rotate(fw, r1, fst.center)
  }

  lazy val neighborhoods = List[Pos](Pos(0, 1), Pos(0, -1), Pos(1, 0), Pos(-1, 0))
  lazy val neRot = List[Int](2, 0, 3, 1)

  lazy val outputShift = List[Int](0, 2, 3, 1)

  def trySelect(model: M, pos: Pos)(implicit params: MatchParams): Map[Int, List[ItemInfo]] = {
    val res = scala.collection.mutable.ListBuffer[(Int, ItemInfo)]()
    val cur = model(pos.y)(pos.x)
    for {
      idx <- 0 until 3
      neighIdx <- 0 until 4
      rotate <- 0 until 4
    } {
      val neighborhood = pos + neighborhoods(neighIdx)
      if (inside(model, neighborhood)) {
        val nn = model(neighborhood.y)(neighborhood.x)
        if (nn.idx.isDefined && nn.rotation.isDefined &&
          Matcher.basicMatch(Holder.r(Main.pairToIdx(cur.num, idx)), Holder.r(Main.pairToIdx(nn.num, nn.idx.get)), rotate, (neRot(neighIdx) + nn.rotation.get) % 4)(params, FakeMatcher)) {
          res.append((idx, ItemInfo(cur.num, Some(idx), Some((rotate + outputShift(neighIdx)) % 4))))
        }
      }
    }

    res.groupBy(_._1).mapValues(_.toList.map(_._2))
  }

  def tryOne(model: M)(implicit params: MatchParams): Option[(Pos, ItemInfo)] = {
    for {
      col <- model.indices
      row <- model.head.indices
      cell = model(col)(row)
      if cell.idx.isEmpty && cell.rotation.isEmpty && cell.num != 0
    } {
      val pos = Pos(row, col)
      val r = trySelect(model, pos)
      val rr = r.toList.flatMap(_._2).distinct
      if (rr.size == 1) {
        println(s"${cell.num} $col $row $r")
        return Some((pos, rr.head))
      }
    }

    None
  }

  def replace(m: M, pos: Pos, item: ItemInfo): M = {
    val mutable = m.toArray.map(_.toArray)
    mutable(pos.y)(pos.x) = item

    mutable.map(_.toList).toList
  }

  def replace(model: M)(implicit params: MatchParams): M = {
    val res = tryOne(model)
    val rr: Option[M] = res.map {
      case (pos, item) =>
        println("new")
        replace(model, pos, item)
    }

    val r = rr.getOrElse(model)
    FileUtils.write("model.txt", toString1(r))
    r
  }
}

case class ItemInfo(num: Int, idx: Option[Int], rotation: Option[Int]) {
  override def toString: String = (idx, rotation) match {
    case (Some(i), Some(r)) => s"$num.$i.$r"
    case (Some(i), None) => s"$num.$i"
    case (None, None) => s"$num"
  }

  def full: Boolean = idx.isDefined && rotation.isDefined
}