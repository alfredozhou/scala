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
  def pascal(c: Int, r: Int): Int =
    if (c > r || c < 0 || r < 0)
      throw new Error("Invalid column value and row value")
    else if (c == 0 || r == c)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def charValue(char: Char) = if (char == '(') 1 else if (char == ')') -1 else 0
    def balancedString(value: Int, chars: List[Char]): Boolean =
      if (value < 0)
        false
      else if (chars.isEmpty)
        value == 0
      else
        balancedString(value + charValue(chars.head), chars.tail)

    balancedString(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def partition(money: Int, coin: Int, coinage: List[Int]): Int = {
      if (coin > money) 0
      else if (coin == money) 1
      else {
        if (!coinage.isEmpty)
          partition(money - coin, coin, coinage) + partition(money, coinage.head, coinage.tail)
        else
          partition(money - coin, coin, coinage)
      }
    }

    if (coins.isEmpty)
      0
    else
      partition(money, coins.sorted.head, coins.sorted.tail)
  }
}
