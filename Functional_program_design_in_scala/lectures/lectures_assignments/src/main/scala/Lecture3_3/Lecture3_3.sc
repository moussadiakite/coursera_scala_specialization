import javafx.scene.web.HTMLEditorSkin.Command

object exercise{
  def WHILE(condition: => Boolean)(command: => Unit): Unit = {
    if(condition) {
      command
      WHILE(condition)(command)
    }
  }

  var x = 0
  WHILE(x < 5){
    println(s"$x")
    x = x + 1
  }

  class REPEAT(command: => Unit){
    def UNTIL(condition: => Boolean): Unit = {
      command
      if(condition) ()
      else UNTIL(condition)
    }
  }

  object REPEAT{
    def apply(command: => Unit) = new REPEAT(command)
  }

  x = 0
  REPEAT{
    println(s"$x")
    x = x + 1
  } UNTIL(x > 5)
}