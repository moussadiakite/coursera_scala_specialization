package stackoverflow

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("'scoredPosting' should work") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
    val postingsList: List[Question] = List(Posting(1,10000220,None,None,2,Some("Python")),
                        Posting(2,10000444,None,Some(10000220),1,None),

                        Posting(1,10000593,None,None,0,Some("C#")),
                        Posting(2,10419795,None,Some(10000593),0,None),
                        Posting(2,10004538,None,Some(10000593),1,None),
                        Posting(2,10211822,None,Some(10000593),0,None),

                        Posting(1,10000635,None,None,0,Some("C++")),
                        Posting(2,10000861,None,Some(10000635),1,None),

                        Posting(1,10000755,None,None,-1,Some("C++")),
                        Posting(2,10000838,None,Some(10000755),0,None),

                        Posting(1,10001079,None,None,3,Some("C#")),
                        Posting(2,10001396,None,Some(10001079),1,None),
                        Posting(2,10001817,None,Some(10001079),1,None),
                        Posting(2,10002046,None,Some(10001079),2,None),

                        Posting(1,10001424,None,None,0,Some("Objective-C")),
                        Posting(2,10002905,None,Some(10001424),0,None),

                        Posting(1,10001447,None,None,0,Some("JavaScript")),
                        Posting(2,10001474,None,Some(10001447),1,None),
                        Posting(2,10001477,None,Some(10001447),2,None),
                        Posting(2,10001485,None,Some(10001447),1,None),
                        Posting(2,10001519,None,Some(10001447),1,None),

                        Posting(1,10001681,None,None,1,Some("JavaScript")),
                        Posting(2,10022172,None,Some(10001681),1,None),

                        Posting(1,10001886,None,None,1,Some("PHP")),
                        Posting(2,10001989,None,Some(10001886),0,None),
                        Posting(2,10002024,None,Some(10001886),1,None),
                        Posting(2,10002028,None,Some(10001886),1,None),
                        Posting(2,10002051,None,Some(10001886),0,None)
    )
    val postings: RDD[Posting] = sc.parallelize(postingsList)
    val groupsRdd = groupedPostings(postings)
    val res = scoredPostings(groupsRdd).collect.toList.sortBy{case (q, _) => q.id}
    assert(res === List((Posting(1,10000220,None,None,2,Some("Python")),1), (Posting(1,10000593,None,None,0,Some("C#")),1), (Posting(1,10000635,None,None,0,Some("C++")),1), (Posting(1,10000755,None,None,-1,Some("C++")),0), (Posting(1,10001079,None,None,3,Some("C#")),2), (Posting(1,10001424,None,None,0,Some("Objective-C")),0), (Posting(1,10001447,None,None,0,Some("JavaScript")),2), (Posting(1,10001681,None,None,1,Some("JavaScript")),1), (Posting(1,10001886,None,None,1,Some("PHP")),1)))
  }

  test("kmeans"){
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
    import StackOverflow._
    val vectors = List((150000,1), (200000,1), (250000,1), (250000,0), (200000,2), (400000,0), (0,2), (0,1), (0,1), (100000,1), (350000,4), (350000,4), (0,1), (100000,0), (100000,4), (400000,8), (400000,8), (250000,1), (50000,5), (150000,1), (0,0), (400000,33), (50000,2), (100000,0), (200000,0), (200000,3), (200000,4), (250000,7), (500000,8), (250000,2), (50000,56), (0,2), (400000,0), (350000,4), (300000,4), (250000,2), (200000,2), (500000,18), (200000,4), (0,14), (100000,2), (50000,0), (200000,1), (400000,4), (100000,2), (0,1), (100000,2), (200000,7), (0,0), (150000,1), (50000,3), (200000,0), (100000,2), (0,0), (0,0), (0,0), (100000,2), (50000,4), (400000,3), (100000,0), (100000,0), (200000,8), (350000,1), (50000,1), (150000,0), (100000,1), (50000,12), (200000,0), (150000,7), (0,0), (50000,1), (150000,2), (50000,14), (400000,2), (50000,0), (200000,1), (350000,2), (0,0), (200000,2), (0,7), (50000,1), (200000,3), (600000,3), (400000,1), (200000,0), (250000,0), (150000,1), (200000,4), (200000,1))
    val means = Array((100000,1), (100000,14), (100000,1), (50000,0), (50000,37), (50000,3), (450000,2), (450000,16), (450000,0), (200000,37), (200000,0), (200000,3), (0,54), (0,0), (0,2), (500000,24), (500000,4), (500000,0), (350000,2), (350000,2), (350000,143), (400000,58), (400000,3), (400000,0), (150000,2), (150000,34), (150000,2), (600000,0), (600000,0), (600000,5), (650000,2), (650000,11), (650000,74), (550000,7), (550000,1), (550000,59), (300000,124), (300000,1), (300000,10), (250000,29), (250000,0), (250000,3), (700000,2), (700000,30), (700000,0))
    val vectorsRDD = sc.parallelize(vectors)
    kmeans(means, vectorsRDD, 1,true)
  }

}
