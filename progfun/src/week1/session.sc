object session {

  def makeChange(money: Int, coins: List[Int]): Int = {
    def partition(money: Int, coin: Int, coinage: List[Int]): Int = {
      if (coin > money) 0
      else if (coin == money)	1
      else {
      		if (!coinage.isEmpty)
      			partition(money - coin, coin, coinage) + partition(money, coinage.head, coinage.tail)
      		else
      			partition(money - coin, coin, coinage)
      }
    }

		partition(money, coins.head, coins.tail)
  }                                               //> makeChange: (money: Int, coins: List[Int])Int

	List(4,3,2,1).sorted                      //> res0: List[Int] = List(1, 2, 3, 4)
	
}