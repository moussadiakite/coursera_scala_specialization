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
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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
    val s4 = singletonSet(4)
    val u1_2 = union(s1, s2)
    val u2_3 = union(s2, s3)
    val u1_3 = union(s1, s3)
    val u2_4 = union(s2, s4)
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

  test("singleton contains the right element and reject others") {
    new TestSets {
      assert(s1(1), "S1 contains 1")
      assert(s2(2), "S2 contains 2")
      assert(s3(3), "S3 contains 3")


      assert(!s1(3), "S1 doesn't contain 3")
      assert(!s2(1), "S2 doesn't contain 1")
      assert(!s3(2), "S3 doesn't contain 2")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect") {
    new TestSets {
      val i1 = intersect(u1_2, u2_3)
      val i2 = intersect(u1_2, u1_3)
      val i3 = intersect(u2_3, u1_3)
      val i4 = intersect(u1_2, u1_2)
      val i5 = intersect(s1, s2)
      assert(contains(i1, 2), "Intersect 1")
      assert(contains(i2, 1), "Intersect 2")
      assert(contains(i3, 3), "Intersect 3")
      assert(contains(i4, 2), "Intersect 4")
      assert(contains(s1, 1), "Intersect 5")
      assert(!contains(i5, 1), "Intersect 6")
      assert(!contains(i5, 2), "Intersect 7")
      assert(!contains(i5, 3), "Intersect 8")

    }
  }

  test("diff") {
    new TestSets {
      val d1 = diff(u1_2, u2_3)
      val d2 = diff(u1_2, s3)
      assert(contains(d1, 1), "Diff 1")
      assert(!contains(d1, 2), "Diff 1")
      assert(contains(d2, 1), "Diff 2")
      assert(contains(d2, 2), "Diff 3")
    }
  }

  test("filter") {
    new TestSets {
      val f1 = filter(u1_2, x => x % 2 == 0)
      assert(contains(f1, 2), "Filter 1")
      assert(!contains(f1, 1), "Filter 2")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(u2_4, x => x % 2 == 0), "Forall 1")
      assert(!forall(u1_2, x => x % 2 == 0), "Forall 2")
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(u1_2, x => x % 2 == 0), "Exists 1")
      assert(!exists(u2_4, x => x % 2 != 0), "Exists 2")
    }
  }

  test("map") {
    new TestSets {
      val m1 = map(u1_2, x => x * 5)
      assert(contains(m1, 5), "Map 1")
      assert(contains(m1, 10), "Map 2")
      assert(!contains(m1, 1), "Map 3")
      assert(!contains(m1, 2), "Map 4")
    }
  }
}
