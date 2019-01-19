package parallelism_on_jvm_1

object Main extends App {
  def helloworld() = {
    val s = new HelloThread
    val t = new HelloThread

    s.start()
    t.start()
    s.join()
    t.join()
  }

  var uidCount = 0L

  val x = AnyRef

  def getUniqueId(): Long = x.synchronized {
    uidCount = uidCount + 1
    uidCount
  }

  def startThread(): Thread = {
    val t = new Thread{
      override def run() = {
        val uids = for(i <- 1 until 10) yield getUniqueId()
        println(uids)
      }
    }
    t.start()
    t
  }

  startThread()
  startThread()
}
