package ren.kujoka.Domemo
import scala.util.Random.shuffle
import ren.kujoka.common.Reader.readIntLoop
import scala.io.StdIn.readLine
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

object Domemo {
  private var cards: Queue[Int] = new Queue[Int]
  private var openCards: Seq[Int] = _
  private var players: Seq[Player] = _
  private var logs: Queue[Int] = new Queue[Int]

  def main(args:Array[String]) {
    var yn = ""
    do {
      cards = new Queue[Int]
      openCards = Seq.fill(7)(0)
      players = Seq.empty[Player]
      logs = new Queue[Int]
      game()
      var end = false
      do {
        yn = readLine("\nPlay again?(y/n) > ")
        end =
          if (yn == "y" || yn == "n") {
            true
          } else {
            println("Please enter y or n")
            false
          }
      } while (end == false)
    } while (yn == "y")
  }

  def game() {
    cardsSet()
    val nop = readIntLoop("Please enter the number of people > ",
      "Please enter the correct value", 2, 5)
    playersSet(nop)
    for (card <- cards)
      openCardsAdd(card)
    var gameEnd = false
    do {
      openCardsShow(nop)
      val ans = readIntLoop("Please enter a number(1-7) > ",
          "Please enter the correct value", 1, 7)
      gameEnd = answerCheck(0, ans)
      for (i <- 1 until nop; if gameEnd == false) {
        gameEnd = answerCheck(i, players(i).think(players:_*)(openCards:_*)(logs:_*))
      }
    } while (gameEnd == false)
  }

  def cardsSet() {
    for (i <- 0 to 6) {
      for (j <- 0 to i) cards += (i + 1)
    }
    cards = shuffle(cards) 
  }

  def playersSet(nop:Int) {
    players = players :+ new Player(0)
    for (i <- 1 to nop - 1) {
      val str = if (i == 1) 
      """|Please select the nature of the computer
         |0:Accurate
         |1:Accurate(but remark ignored)
         |2:Suitable(but not weak))
         |(not much different)
         | > """.stripMargin else " > "
      val cpselect =
        readIntLoop(str, "Please enter the correct value", 0, 2)
      players =
        if (cpselect == 0) players :+ new AccurateComputer(i) 
        else if (cpselect == 1) players :+ new Player(i)
        else players :+ new RandomComputer(i)
    }
    players = players :+ new Player(nop)
    for (player <- players) {
      for (_ <- 1 to 6 / (nop / 2) + 1) {
        player.handsPlus(cards.dequeue)
      }
    }
    if (nop == 4) {
      for (i <- 0 to 3) {
        players(i).handsPlus(cards.dequeue)
      }
    }
  }

  def answerCheck(pNum:Int, num:Int):Boolean = {
    if (logs.length >= players.length - 2) logs.dequeue
    var handsTemp = ListBuffer.empty[Int]
    handsTemp = players(pNum).hands.to[ListBuffer]
    if (handsTemp.indexWhere(n => n == num) != -1) {
      println(players(pNum).playerName + " answer = " + num + " is exist")
      handsTemp.remove(handsTemp.indexWhere(n => n == num))
      players(pNum).hands = handsTemp.toSeq
      openCardsAdd(num)
    } else {
      logs += num
      println(players(pNum).playerName + " answer = " + num + " is not exist")
      players(pNum).notExist(num)
    }
    if (players(pNum).hands.length != 0) {
      false
    } else {
      if (pNum == 0) println("You Win") else println("You Lose ")
      true
    }
  }

  def openCardsAdd(num:Int) {
    openCards = openCards.updated(num - 1, openCards(num - 1) + 1)  
  }

  def openCardsShow(nop:Int) {
    print("[Hand of Other Players]")
    for (i <- 1 to nop - 1)
      players(i).show()
    var cnt = 1
    println("\n[Open Cards]")
    for (openCard <- openCards) {
      for (j <- 0 until openCard)
        print(cnt + " ")
      cnt += 1
      println
    }
  }
  
}
