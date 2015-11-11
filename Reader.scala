package ren.kujoka.common
import scala.io.StdIn.readLine
object Reader {
  def readIntLoop(msg1: String, msg2: String, from: Int, to: Int): Int = {
    try {
      val i = readLine(msg1).toInt
      if (i >= from && i <= to) i else throw new NumberFormatException
    } catch {
      case ex: NumberFormatException =>
        println(msg2)
        readIntLoop(msg1, msg2, from, to)
    }
  }

  def readIntLoop(msg1: String, msg2: String): Int = {
    try {
      readLine(msg1).toInt
    } catch {
      case ex: NumberFormatException =>
        println(msg2)
        readIntLoop(msg1, msg2)
    }
  }
}
