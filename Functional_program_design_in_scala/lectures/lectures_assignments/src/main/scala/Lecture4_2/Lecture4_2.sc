object exercise{
  class Signal[T](expr: => T){
    import Signal._
    private var myExpr: () => T = _
    private var myValue: T = _
    private var observers: Set[Signal[_]] = Set()
    update(expr)

    protected def update(expr: => T): Unit = {
      myExpr = () => expr
      computeValue()
    }

    protected def computeValue(): Unit = {
      val newValue = caller.withValue(this)(myExpr())
      if(myValue != newValue){
        myValue = newValue
        val obs = observers
        observers = Set()
        obs.foreach(_.computeValue())
      }
    }

    def apply(): T = {
      observers += caller.value
      assert(!caller.value.observers.contains(this), "cyclic signal definition")
      myValue
    }
  }

  object NoSignal extends Signal[Nothing](???){
    override protected def computeValue(): Unit = ()
  }

  object Signal {
    private val caller = new StackableVariable[Signal[_]](NoSignal)
    def apply[T](expr: => T): Signal[T] = new Signal(expr)
  }

  class Var[T](expr: => T) extends Signal[T](expr){
    override def update(expr: => T): Unit = super.update(expr)
  }

  object Var {
    def apply[T](expr: => T): Var[T] = new Var(expr)
  }

  class StackableVariable[T](init: T){
    private var values: List[T] = List(init)
    def value: T = values.head
    def withValue[R](newValue: T)(op: => R): R = {
      values = newValue::values
      try op finally values = values.tail
    }
  }

  class BankAccount {
    private var balance = Var[Int](0)
    def currentBalance = balance()

    def deposit(amount: Int): Unit =
      if(amount > 0) {
        val b = balance()
        balance() = b + amount
      }

    def withdraw(amount: Int): Unit =
      if(amount > 0 && amount <= balance()){
        val b = balance()
        balance() = b - amount
      } else throw new Error("Insufficient funds")
  }

  def consolidated(accts: List[BankAccount]): Signal[Int] =
    Signal(accts.map(_.currentBalance).sum)

  val a, b = new BankAccount
  val c = consolidated(List(a, b))
  c()
  a deposit 20
  c()
  b deposit 30
  c()

  val xchange = Signal(100.00)
  val inDollar = Signal(c() * xchange())
  inDollar()
  b withdraw 10
  inDollar()
}