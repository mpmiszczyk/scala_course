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
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   * - test
   * - ignore
   * - pending
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
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
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
    val all = union(s1, union(s2, s3))
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
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains elements from both") {
    new TestSets {
      val s = intersect(s1, s1)
      assert(contains(s, 1), "Intersetion with itself")
      assert(!contains(s, 2), "but not with others")
      assert(!contains(s, 3), "or even others")
    }
  }

  test("intersection of excluding sets") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "does not contain first element")
      assert(!contains(s, 2), "or the second")
    }
  }

  test("filter of greater that 2") {
    new TestSets {
      val s = filter(
        all,
        (x: Int) => x > 2
      )
      assert(contains(all, 1), "one is in `all`")
      assert(!contains(s, 1), "but it does not pass filter")

      assert(contains(all, 3), "three is in `all`")
      assert(contains(s, 3), "and it passes filter")

      assert(!contains(all, 5), "five is not in all")
      assert(!contains(s, 5), "an it will not be in filtered set")
    }
  }

  test("forall on all elements") {
    new TestSets {
      assert(forall(all, (x: Int) => x < 5), "all elements are smaller then 5")
      assert(!forall(all, (x: Int) => x < 3), "but not than 3")
    }
  }

  test("exists on set `{1, 2, 3}`"){
    new TestSets {
      assert(!exists(all, (x: Int) => x > 5), "none of elements is greater than five")
      assert(exists(all, (x: Int) => x == 1), "but one of them is equal to two")
    }
  }

  test("map increase by one"){
    new TestSets {
      val s = map(s1, (x: Int) => x+1)
      assert(contains(s, 2), "new set contains increased value")
      assert(!contains(s, 1), "but not original one")
    }
  }

}
