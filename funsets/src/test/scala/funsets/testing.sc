object testing {


  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: (Int => Boolean), elem: Int): Boolean = s(elem)
                                                  //> contains: (s: Int => Boolean, elem: Int)Boolean

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Int => Boolean =
    (x: Int) => x == elem                         //> singletonSet: (elem: Int)Int => Boolean

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Int => Boolean, t: Int => Boolean): Int => Boolean =
    (x: Int) => contains(s, x) || contains(t, x)  //> union: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
    
  
 
}