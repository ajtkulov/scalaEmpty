package main

import java.awt.Font
import java.awt.image.BufferedImage
import java.io.File
import scala.sys.process._
import Handler._
import javax.imageio.ImageIO
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

  def draw(values: M, k: Int = 1): BufferedImage = {
    val res = new BufferedImage(2048 * k, 2048, BufferedImage.TYPE_INT_RGB)

    val g2d = res.createGraphics()
    g2d.setFont(new Font("TimesRoman", Font.PLAIN, 32))

    for {i <- values.indices
         list = values(i)
         j <- list.indices
         z = values(i)(j)
         if z.full
         img = rotateItem(Holder.r(Main.pairToIdx(z.num, z.idx.get)), z.rotation.get)
    } {
      for {
        x <- 100 to 900 by 2
        y <- 100 to 900 by 2
      } {
        val c = img.getColor(x, y)
        if (Handler.nonEmpty(c)) {
          res.setRGB(j * 250 + x / 2, i * 250 + y / 2, img.getRGB(x, y))
        }
      }
      g2d.drawString(s"${z.num}", j * 250 + 150, i * 250 + 150)

    }

    g2d.dispose()
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
  lazy val invoutputShift = List[Int](0, 2, 1, 3)

  def find(model: M, value: Int): Pos = {
    val res = for {
      col <- model.indices
      row <- model.head.indices
      cell = model(col)(row)
      if cell.num == value && !cell.full
    } yield Pos(row, col)
    res.head
  }

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
        val wItem = Holder.r(Main.pairToIdx(cur.num, idx))
        if (nn.idx.isDefined && nn.rotation.isDefined &&
          Matcher.basicMatch(wItem, Holder.r(Main.pairToIdx(nn.num, nn.idx.get)), rotate, (neRot(neighIdx) + nn.rotation.get) % 4)(params, FakeMatcher)) {
          res.append((idx, ItemInfo(cur.num, Some(idx), Some((rotate + outputShift(neighIdx)) % 4))))
        }
      }
    }

    res.toList.map(_._2).distinct.foreach(x => println(info(x)))

    res.groupBy(_._1).mapValues(_.toList.map(_._2))
  }

  def neigh(model: M, pos: Pos): List[(Int, Int, ItemInfo)] = {
    val res = scala.collection.mutable.ListBuffer[(Int, Int, ItemInfo)]()
    for {
      neighIdx <- 0 until 4
    } {
      val neighborhood = pos + neighborhoods(neighIdx)
      if (inside(model, neighborhood)) {
        val nn = model(neighborhood.y)(neighborhood.x)
        if (nn.idx.isDefined && nn.rotation.isDefined) {
          res.append(((neRot(neighIdx) + nn.rotation.get) % 4, neighIdx, nn))
        }
      }
    }

    res.toList
  }

  def find(list: List[(Int, Int, ItemInfo)])(implicit params: MatchParams) = {
    val res = scala.collection.mutable.ListBuffer[WItem]()

    for {
      rot <- 0 until 4
      item <- Holder.r.values
    } {
      if (list.forall {
        case (r, ni, itemInfo) => Matcher.basicMatch(item, Holder.r(Main.pairToIdx(itemInfo.num, itemInfo.idx.get)), (invoutputShift(ni) + rot) % 4, r)(params, FakeMatcher)
//        case (r, ni, itemInfo) => Matcher.basicMatch(item, Holder.r(Main.pairToIdx(itemInfo.num, itemInfo.idx.get)), (ni + rot) % 4, r)(params, FakeMatcher)
      }) {
        res.append(item)
      }
    }

    res.distinct.toList
  }

  def check(model: M, pos: Pos)(implicit params: MatchParams): Boolean = {
    var res = true
    val cur = model(pos.y)(pos.x)
    for {
      neighIdx <- 0 until 4
//      rotate <- 0 until 4
    } {
      val neighborhood = pos + neighborhoods(neighIdx)
      if (inside(model, neighborhood)) {
        val nn = model(neighborhood.y)(neighborhood.x)
        val wItem = Holder.r(Main.pairToIdx(cur.num, cur.idx.get))
        val rotate = (neighIdx + outputShift(neighIdx)) % 4

        val nnn = Holder.r(Main.pairToIdx(nn.num, nn.idx.get))
        if (nn.idx.isDefined && nn.rotation.isDefined &&
          Matcher.basicMatch(wItem, nnn, rotate, (neRot(neighIdx) + nn.rotation.get) % 4)(params, FakeMatcher)) {
//          res.append((idx, ItemInfo(cur.num, Some(idx), Some((rotate + outputShift(neighIdx)) % 4))))
        }
      }
    }
    ???
//    res.groupBy(_._1).mapValues(_.toList.map(_._2))
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

  def info(itemInfo: ItemInfo): String = {
    val set = RealMatcher.byPair(itemInfo.num, itemInfo.idx.get)
    s"${itemInfo} -> $set"
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

  def select(value: Int): Map[Int, List[ItemInfo]] = {
    val m = Model.read("model.txt")
    Model.trySelect(m, Model.find(m, value))(MatchParams.precise)
    Model.trySelect(m, Model.find(m, value))(MatchParams.precise1)
    Model.trySelect(m, Model.find(m, value))(MatchParams.standard)
    Model.trySelect(m, Model.find(m, value))(MatchParams.all)
    Model.trySelect(m, Model.find(m, value))(MatchParams.all1)
  }

  def drawAll(fromRight: Int, width: Int, fromBottom: Int, height: Int, k: Int = 1) = {
    ImageIO.write(Model.draw(Model.reflect(Model.read("model.txt").map(_.drop(fromRight).take(width)).dropRight(25 - fromBottom)).take(height), k = k), "png", new File(s"123.jpg")); "./open1".!
  }

  def check(m: M) = {
    m.flatten.collect { case ItemInfo(a, Some(b), _) => (a, b) }.groupBy(identity).filter { case (key, value) => value.size > 1 }.keys.toList
  }

  def t(mod: M): Unit = {
    for {
      row <- 1 until mod.size
      tryAt <- 1 until mod.size
      main = mod(row)(0)
      to = mod(tryAt)(0)
      m = Holder.r(Main.pairToIdx(main.num, main.idx.get))
      t = Holder.r(Main.pairToIdx(to.num, to.idx.get))
    } if (
      Matcher.basicMatch(m, t, main.rotation.get, (to.rotation.get + 2) % 4)(MatchParams.standard, FakeMatcher)
    ) {
      println(s"$row, $tryAt")
    }
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