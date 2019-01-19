package parallelism_on_jvm_1

class HelloThread extends Thread {
  override def run() = {
    println("Hello")
    println("world")
  }
}