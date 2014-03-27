object test {
  def squareListPattern(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => y * y :: squareListPattern(ys)
  }

  def squareList(xs: List[Int]): List[Int] =
    xs map (y => y * y)

  def encode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      (x, first.length) :: encode(rest)
  }

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(::)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)(???)
}