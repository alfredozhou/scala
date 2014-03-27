package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(singletonSet(100), 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s = union(s1, s2)
    val t = union(s2, s3)
    val all = union(s, t)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {

      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("diff") {
    new TestSets {

      assert(contains(diff(s, s1), 2), "diff of union(1, 2) and 1, should be 2")
      assert(contains(diff(s1, s2), 1), "diff of 1 and 2, should be 1")
      assert(!contains(diff(s1, s1), 1), "diff of 1 and 1 should be empty")
    }
  }

  test("intersect") {
    new TestSets {

      assert(contains(intersect(s, t), 2), "intersection of 1,2 and 2, 3, should be 2")
      assert(!contains(intersect(s1, s3), 3), "intersection of 1 and 3 should be nothing")
      assert(contains(intersect(s1, s1), 1), "intersection of self is self")
    }
  }

  test("filter") {
    new TestSets {

      assert(contains(filter(all, (x: Int) => x % 2 == 0), 2), "only 2 is even in 1, 2, 3")
    }
  }

  test("forall") {
    new TestSets {

      assert(forall(all, (x: Int) => x > 0), "1, 2, 3 are all positive")
      assert(!forall(all, (x: Int) => x > 1), "1 is not less than 1")
    }
  }

  test("exists") {
    new TestSets {

      assert(exists(all, (x: Int) => x > 0), "1, 2, 3 are all positive")
      assert(exists(all, (x: Int) => x > 1), "1 is not less than 1")
      assert(!exists(all, (x: Int) => x < 1), "none is smaller than 1")
    }
  }

  test("map") {
    new TestSets {
      val mapped = map(all, (x: Int) => x * 5)
      assert(contains(mapped, 5), "1, 2, 3 should be 5, 10, 15")
      val mapped2 = map(all, (x: Int) => x - 2)
      assert(contains(mapped2, -1), "1 - 2 is -1")
      assert(contains(mapped2, 0), "2 -2 is 0")
      assert(contains(mapped2, 1), "3 - 2 is 1")
    }
  }
}
