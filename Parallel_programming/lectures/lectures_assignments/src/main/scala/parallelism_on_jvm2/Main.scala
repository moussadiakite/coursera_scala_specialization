package parallelism_on_jvm2

object Main extends App {
  def startThread(a: Account, b: Account, n: Int): Thread = {
    val t = new Thread{
      override def run()= {
        for(i <- 0 until n){
          a.transfer(b, 1)
        }
      }
    }
    t.start()
    t
  }

  val a1 = new Account(500000)
  val a2 = new Account(700000)

  val t = startThread(a1, a2, 150000)
  val s = startThread(a2, a1, 150000)
  t.join()
  s.join()
  println(a1.getAmount)
  println(a2.getAmount)
}
