
object rationals {

  val x = new Rational(1, 3)                      //> x  : Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : Rational = 3/2

  x.subtract(y).subtract(z)                       //> res0: Rational = -79/42
  y.add(y)                                        //> res1: Rational = 10/7
  x.less(y)                                       //> res2: Boolean = true
  x.max(y)                                        //> res3: Rational = 5/7
  y.numer                                         //> res4: Int = 5
  y.add(y).numer                                  //> res5: Int = 70
}

class Rational(x: Int, y: Int) {
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x
  def denom = y
  
  def less(other: Rational):Boolean = numer * other.denom < other.numer * denom

def max(other: Rational):Rational = if (this.less(other)) other else this

  def add(another: Rational): Rational = {
    val newX = x * another.denom + another.numer * y
    val newY = y * another.denom
    new Rational(newX, newY)
  }

  def subtract(another: Rational): Rational = add(another.neg)

  def neg =
    new Rational(-x, y)

  override def toString =
    numer / g + "/" + denom /g
}