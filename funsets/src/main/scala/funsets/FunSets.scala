package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set =
    (x: Int) => x == elem

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set =
    (x: Int) => contains(s, x) || contains(t, x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: Set, t: Set): Set =
    (x: Int) => contains(s, x) && contains(t, x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set =
    (x: Int) => contains(s, x) && !contains(t, x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set =
    (x: Int) => contains(s, x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  def foreach(s: Set, predicateToEndEval: Int => Boolean, acc: Boolean): Boolean = {
    def innerIter(a: Int): Boolean = {
      if (contains(s, a) && predicateToEndEval(a)) !acc
      else if (a > bound) acc
      else innerIter(a + 1)
    }
    innerIter(-bound)
  }

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean =
    foreach(s, (x: Int) => !p(x), true)

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean =
    !forall(s, (x: Int) => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    def innerMap(a: Int, set: Set): Set = {
      if (contains(s, a)) innerMap(a + 1, union(set, singletonSet(f(a))))
      else if (a > bound) set
      else innerMap(a + 1, set)
    }
    innerMap(-bound, diff(singletonSet(1), singletonSet(1)))
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
