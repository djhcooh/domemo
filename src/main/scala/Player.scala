package ren.kujoka.Domemo
import scala.util.Random.shuffle
import scala.collection.mutable.ArrayBuffer

class Player(private val playerNum: Int) {
  private var hands_ = ArrayBuffer.empty[Int]
  protected var exist = Array.fill(7)(true)
  private val playerName_ =
    if (playerNum == 0) "Your" else playerNum + 1 + "P's"

  def show() {
    print("\n" + (playerNum + 1) + "P:")
    for (hand <- hands)
      print(hand + " ")
  }

  def handsPlus(card: Int) {
    hands += card
  }

  def notExist(num: Int) {
    exist(num - 1) = false
  }

  def think(others: Player*)(openCards: Int*)(logs: Int*): Int = {
    var nums = ArrayBuffer.empty[Double]
    openCards.foreach(nums += _.toDouble)
    for (i <- 0 until others.length - 1 if i != playerNum) {
      for (otherHand <- others(i).hands)
        nums(otherHand - 1) = nums(otherHand - 1) + 1
    }
    var pos: Double = 0
    for (i <- 0 to 6) {
      pos = if (exist(i) == true) ((i + 1 - nums(i)) * (i + 1)) / 28 else 0
      nums(i) = pos
    }
    nums.indexWhere(_ == nums.max) + 1
  }

  def hands: ArrayBuffer[Int] = hands_

  def hands_=(newHands: ArrayBuffer[Int]) = hands_ = newHands

  def playerName: String = playerName_
}

class RandomComputer(playerNum: Int) extends Player(playerNum) {
  override def think(others: Player*)(openCards: Int*)(logs: Int*): Int = {
    var nums = ArrayBuffer.empty[Double]
    openCards.foreach(nums += _.toDouble)
    for (i <- 0 until others.length - 1 if i != playerNum) {
      for (otherHand <- others(i).hands)
        nums(otherHand - 1) = nums(otherHand - 1) + 1
    }
    var ans = ArrayBuffer.empty[Int] 
    for (i <- 0 to 6 if nums(i) != (i + 1); if exist(i) == true) {
      ans += (i + 1)
    }
    ans = shuffle(ans)
    ans.head
  } 
}

class AccurateComputer(playerNum: Int) extends Player(playerNum) {
  override def think(others: Player*)(openCards: Int*)(logs: Int*): Int = {
    var nums = ArrayBuffer.empty[Double]
    openCards.foreach(nums += _.toDouble)
    for (i <- 0 until others.length - 1 if i != playerNum) {
      for (otherHand <- others(i).hands)
        nums(otherHand - 1) = nums(otherHand - 1) + 1
    }
    for (log <- logs) {
      if (exist(log - 1) == true) {
        if (nums(log - 1) == log - 1) {
          exist(log - 1) = false
        } else if (nums(log - 1) == log - 2) {
          nums(log - 1) = nums(log - 1) + 1
        }
      }
    }
    var pos: Double = 0
    for (i <- 0 to 6) {
      pos = if (exist(i) == true) ((i + 1 - nums(i)) * (i + 1)) / 28 else 0
      nums(i) = pos
    }
    nums.indexWhere(_ == nums.max) + 1
  }
}
