package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0) 0
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    balanceWithAcc(0, chars)
  }

  private def balanceWithAcc(acc: Int, chars: List[Char]): Boolean = {
    if (acc < 0) false
    else {
      if (chars.isEmpty)
        acc.equals(0)
      else {
        if (chars.head.equals('('))
          balanceWithAcc(acc + 1, chars.tail)
        else if (chars.head.equals(')')) {
          balanceWithAcc(acc - 1, chars.tail)
        } else balanceWithAcc(acc, chars.tail)
      }
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else {
      if (coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
