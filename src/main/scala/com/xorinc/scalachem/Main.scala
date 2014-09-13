package com.xorinc.scalachem

import java.io.File

import com.google.gson.{JsonArray, JsonParser}
import org.apache.commons.io.{IOUtils, FileUtils}
import scopt.OptionParser
import collection.JavaConverters._

object Main {

  case class Opts
  (
    noSpace: Boolean = false,
    noTiles: Boolean = false,
    noError: Boolean = false,
    words: List[String] = Nil
  )
  val parser = new OptionParser[Opts]("chem") {
    head("chem", "1.0")

    opt[Unit]("nospace") maxOccurs 1 action { (_, o) => o.copy(noSpace = true) } text "print without spaces"
    opt[Unit]("notiles") maxOccurs 1 action { (_, o) => o.copy(noTiles = true) } text "print without tile mode"
    opt[Unit]("noerror") maxOccurs 1 action { (_, o) => o.copy(noError = true) } text "print without tile mode"

    opt[String]('w', "word") maxOccurs 1 action { (s, o) => o.copy(words = s.split("\n").toList) } text "text to convert"

    arg[File]("<file>...") optional() unbounded() action { (x, o) =>
      o.copy(words = o.words ++ FileUtils.readFileToString(x).split("\n").toList)
    } text "files to convert"

    showUsageOnError
  }

  def main(args: Array[String]): Unit = {

    parser.parse(if(Console.in.ready()) args ++ IOUtils.toString(Console.in).split("[ \n]") else args, new Opts) match {
      case Some(Opts(noSpace, noTiles, noError, words)) =>
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
                    .map(makeTile _)
                    .reduce(_ zip _ map (t => t._1 + (if (noSpace) "" else " ") + t._2))
                    .reduce(_ + "\n" + _)
                )
              }
            case None =>
              if (!noError) Console.err.println(s"`$word` is not chemical =(")
          }

        }

      case None =>
    }
  }

  def makeTile(element: String): Array[String] = {
    val (symbol, name, number, weight) = {
      val data = table(element.toLowerCase)
      (
        data._1,
        data._2.pad(21, ' '),
        data._3.pad(3, ' '),
        data._4.pad(7, ' ')
      )
    }
    (
       "+---------------------+" ::
      s"|         $number         |" ::
      {
        for(line <-
          charset(symbol(0)) zip (if(symbol.length == 1) Iterator.fill(6)("").toList else charset(symbol(1))) map { t =>
            t._1 + t._2
          }
        )
          yield "|" + line.pad(21, ' ') + "|"
      } :::
       "|                     |" ::
      s"|$name|" ::
       "|                     |" ::
      s"|       $weight       |" ::
       "+---------------------+" ::
      Nil
    ).toArray

  }

  private val jparser = new JsonParser

  val charset = jparser.parse(
    IOUtils.toString(
      this.getClass.getResourceAsStream("/alphabet.json")
    )
  ).getAsJsonObject.entrySet().asScala.map{e => (e.getKey.charAt(0), e.getValue.getAsJsonArray.asScala.toList.map(_.getAsString))}.toMap

  val table = jparser.parse(
    IOUtils.toString(
      this.getClass.getResourceAsStream("/periodic.json")
    )
  ).getAsJsonArray.asScala.map{ case a: JsonArray =>
    (a.get(0).getAsString.toLowerCase,
      (
        a.get(0).getAsString,
        a.get(1).getAsString,
        a.get(2).getAsString,
        a.get(3).getAsString
        )
      )
  }.toMap

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
