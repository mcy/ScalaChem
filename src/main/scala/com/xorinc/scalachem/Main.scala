package com.xorinc.scalachem

import java.io.{FileOutputStream, FileInputStream, InputStream, File}

import com.google.gson.{JsonArray, JsonParser}
import org.apache.commons.io.{IOUtils, FileUtils}
import scopt.OptionParser
import collection.JavaConverters._

object Main {

  val defaultFile = "$$default"

  object FrameType extends Enumeration {
    type FrameType = Value
    val Ascii, SingleStruck, DoubleStruck, Rounded = Value
  }
  import FrameType._
  case class Opts
  (
    noSpace: Boolean = false,
    noTiles: Boolean = false,
    noError: Boolean = false,
    fancyTile: FrameType = Ascii,
    table: Option[String] = None,
    words: List[String] = Nil
  )
  val parser = new OptionParser[Opts]("chem") {
    head("chem", "1.0")

    opt[Unit]("nospace") maxOccurs 1 action { (_, o) => o.copy(noSpace = true) } text "print without spaces"
    opt[Unit]("notiles") maxOccurs 1 action { (_, o) => o.copy(noTiles = true) } text "print without tile mode"
    opt[Unit]("noerror") maxOccurs 1 action { (_, o) => o.copy(noError = true) } text "print without tile mode"
    opt[Unit]("singleline") maxOccurs 1 action { (_, o) => o.copy(fancyTile = SingleStruck) } text "use single-struck frame characters for tiles"
    opt[Unit]("doubleline") maxOccurs 1 action { (_, o) => o.copy(fancyTile = DoubleStruck) } text "use double-struck frame characters for tiles"
    opt[Unit]("rounded") maxOccurs 1 action { (_, o) => o.copy(fancyTile = Rounded) } text "use double-struck frame characters for tiles"

    opt[String]("tableFile") maxOccurs 1 action { (s, o) => o.copy(table = Some(s))} text "generate a periodic table from file"
    opt[Unit]("table") maxOccurs 1 action { (_, o) => o.copy(table = Some(defaultFile))} text "generate a periodic table"

    opt[File]("out") maxOccurs 1 action { (f, o) => Console.setOut(new FileOutputStream(f)); o } text "redirection (because the shell doesn't like wide stuff)"

    opt[String]('w', "word") maxOccurs 1 action { (s, o) => o.copy(words = s.split("\n").toList) } text "text to convert"

    arg[File]("<file>...") optional() unbounded() action { (x, o) =>
      o.copy(words = o.words ++ FileUtils.readFileToString(x).split("\n").toList)
    } text "files to convert"

    showUsageOnError
  }

  def main(args: Array[String]): Unit = {

    parser.parse(if(Console.in.ready()) args ++ IOUtils.toString(Console.in).split("[ \n]") else args, new Opts) match {
      case Some(Opts(noSpace, noTiles, noError, frame, tableFile, words)) =>
        if(tableFile.isDefined) {
          val (_, tableOrdered) = {
            if(tableFile.get == defaultFile) (null, Main.tableOrdered)
            else makeTable(new FileInputStream(tableFile.get))
          }
          val height = tableOrdered.values.maxBy(_._5)._5
          val length = tableOrdered.values.maxBy(_._6)._6
          val tableArr = Array.tabulate(height, length){(i, j) => tableOrdered.get((i + 1, j + 1))}
          val formatted = tableArr.map{ a =>
            a.map{
              case Some(element) => makeTile(element, frame)
              case None => Array.fill(13)(" " * 23)
            }.foldLeft(Array.fill(13)("")){
              _ zip _ map { t =>
                if(noSpace){
                  val framesSet = frames(frame)
                  val v = if(t._1.endsWith(framesSet._2.toString) && !t._2.endsWith(" ")) t._1.dropRight(1) + framesSet._7 + t._2.drop(1)
                  else if(t._1.endsWith(framesSet._4.toString) && !t._2.endsWith(" ")) t._1.dropRight(1) + framesSet._8 + t._2.drop(1)
                  else if(t._1.endsWith(" ")) t._1.dropRight(1) + t._2
                  else if(t._1 == "") t._2
                  else t._1 + t._2.drop(1)
                  //println(v); v
                  v
                } else t._1 + " " + t._2
              }
            }
          }.map(_.mkString("\n")).foldLeft(" " * (length * 22 + 1)){ (top, bottom) =>
            if(noSpace){
              val framesSet = frames(frame)
              val seam =
                (top.takeRight(length * 22 + 1) zip bottom.take(length * 22 + 1)).map{ t =>
                  val (a, b) = t
                  //println(a.toString + b)
                  if(a == ' ' && b == ' ') ' '
                  else if(a == ' ') b
                  else if(b == ' ') a
                  else if(a == framesSet._3 && b == framesSet._1) framesSet._9
                  else if(a == framesSet._4 && b == framesSet._2) framesSet._10
                  else if(a == framesSet._6 && b == framesSet._6) framesSet._6
                  else framesSet._11
                }.mkString("")
              top.dropRight(length * 22 + 1) + seam + bottom.drop(length * 22 + 1)
            } else top + "\n" + bottom
          }
          println(formatted)
        }
        else
          for(word <- words) {

            object NodeException extends RuntimeException
            def buildChem(in: String, out: String, index: Int): String = {
              if(in == out.filterNot(_ == ' '))
                return out
              else if(in.length - index - 2 >= 0 && table.contains(in.substring(index, index + 2))){
                try {
                  return buildChem(in, out + " " + in.substring(index, index + 2), index + 2)
                } catch {
                  case NodeException =>
                }
              }
              if(in.length - index - 1 >= 0 && table.contains(in.substring(index, index + 1))){
                buildChem(in, out + " " + in.substring(index, index + 1), index + 1)
              } else throw NodeException
            }

            val symbols = Option({
              try{
                buildChem(word.replaceAll("[^A-Za-z]", "").toLowerCase, "", 0)
              } catch {
                case NodeException => null
              }
            }).map(_.trim.split(" ").map(_.capitalize).mkString(" "))

            symbols match {
              case Some(formula) =>
                if(noTiles)
                  println(formula.filterNot(noSpace && _ == ' '))
                else {
                  println(
                    formula.split(" ")
                      .map(makeTile(_, frame))
                      .reduce(_ zip _ map {t =>
                        if(noSpace){
                          val framesSet = frames(frame)
                          if(t._1.endsWith(framesSet._2.toString)) t._1.dropRight(1) + framesSet._7 + t._2.drop(1)
                          else if(t._1.endsWith(framesSet._4.toString)) t._1.dropRight(1) + framesSet._8 + t._2.drop(1)
                          else t._1 + t._2.drop(1)
                        } else t._1 + " " + t._2
                      }).reduce(_ + "\n" + _)
                  )
                }
              case None =>
                if (!noError) Console.err.println(s"`$word` is not chemical =(")
            }

          }

      case None =>
    }
  }

  def frames(frame: FrameType) = frame match {
    case Ascii =>        ('+', '+', '+', '+', '|', '-', '+', '+', '+', '+', '+')
    case SingleStruck => ('┏', '┓', '┗', '┛', '┃', '━', '┳', '┻', '┣', '┫', '╋')
    case DoubleStruck => ('╔', '╗', '╚', '╝', '║', '═', '╦', '╩', '╠', '╣', '╬')
    case Rounded =>      ('╭', '╮', '╰', '╯', '│', '─', '┬', '┴', '├', '┤', '┼')
  }

  def makeTile(element: String, frame: FrameType): Array[String] = makeTile(table(element.toLowerCase), frame)
  def makeTile(element: (String, String, String, String, Int, Int), frame: FrameType): Array[String] = {
    val (symbol, name, number, weight) = {
      (
        element._1,
        element._2.pad(21, ' '),
        element._3.pad(21, ' '),
        element._4.pad(21, ' ')
      )
    }
    val (ul, ur, ll, lr, v, h, _, _, _, _, _) = frames(frame)

    (
      s"$ul${h.toString * 21}$ur" ::
      s"$v$number$v" ::
      {
        if(symbol == "") Array.fill(6)(v + " " * 21 + v).toList
        else {
          for (line <-
               charset(symbol(0)) zip (if (symbol.length == 1) Iterator.fill(6)("").toList else charset(symbol(1))) map { t =>
                 t._1 + t._2
               }
          )
          yield v + line.pad(21, ' ') + v
        }
      } :::
      s"$v${" " * 21}$v" ::
      s"$v$name$v" ::
      s"$v${" " * 21}$v" ::
      s"$v$weight$v" ::
      s"$ll${h.toString * 21}$lr" ::
      Nil
    ).toArray

  }

  private val jparser = new JsonParser

  val charset = jparser.parse(
    IOUtils.toString(
      this.getClass.getResourceAsStream("/alphabet.json")
    )
  ).getAsJsonObject.entrySet().asScala.map{e => (e.getKey.charAt(0), e.getValue.getAsJsonArray.asScala.toList.map(_.getAsString))}.toMap

  val (table, tableOrdered) = makeTable(this.getClass.getResourceAsStream("/periodic.json"))

  def makeTable(in: InputStream) = {
    val tuple = jparser.parse(
      IOUtils.toString(in)
    ).getAsJsonArray.asScala.map{ case a: JsonArray =>
      (
        a.get(0).getAsString,
        a.get(1).getAsString,
        a.get(2).getAsString,
        a.get(3).getAsString,
        a.get(4).getAsInt,
        a.get(5).getAsInt
      )
    }.map(t => (t, t)).unzip
    tuple.copy(_1 = tuple._1.map(t => (t._1.toLowerCase, t)).toMap.filterNot(_._1.isEmpty), _2 = tuple._2.map(t => ((t._5, t._6), t)).toMap)
  }

  implicit class StringOps(val v: String) extends AnyVal{
    def pad(i: Int, c: Char): String = {
      def pad0(s: String, i: Int, c: Char, side: Boolean):String = {
        if(s.length >= i) s
        else if (side)
          pad0(s + c, i, c, !side)
        else
          pad0(c + s, i, c, !side)
      }
      pad0(v, i, c, true)
    }
  }
}
