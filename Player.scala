package ren.kujoka.Domemo

class Player(private val playerNum: Int) {
  private var hands_ = Seq.empty[Int]
  private var exist = Seq.fill(7)(true)
  
  def show() {
    print("\n" + playerNum + ":")
    for (hand <- hands)
      print(hand + " ")
  }

  def handsPlus(card: Int) {
    hands = hands :+ card
  }

  def notExist(num: Int) {
    exist = exist.updated(num - 1, false)
  }

  def think(others: Player*)(openCards: Int*): Int = {
    var nums: Seq[Double] = openCards.map(_.toDouble)
    for (i <- 0 until others.length - 1 if i != playerNum) {
      for (otherHand <- others(i).hands)
        nums = nums.updated(otherHand - 1, nums(otherHand - 1) + 1)
    }
    var pos: Double = 0
    for (i <- 0 to 6) {
      pos = if (exist(i) == true) ((i + 1 - nums(i)) * (i + 1)) / 28 else 0
      nums = nums.updated(i, pos)
    }
    nums.indexWhere(_ == nums.max) + 1
  }

  def hands: Seq[Int] = hands_

  def hands_=(newHands: Seq[Int]) = hands_ = newHands
}
