object exercise {
  trait Subscriber {
    def handler(pub: Publisher)
  }

  trait Publisher {
    private var subscribers: Set[Subscriber] = Set()

    def subscribe(subscriber: Subscriber): Unit =
      subscribers += subscriber

    def unsubscribe(subscriber: Subscriber): Unit =
      subscribers -= subscriber

    def publish(): Unit =
      subscribers.foreach(_.handler(this))
  }

  class BankAccount extends Publisher {
    private var balance = 0
    def currentBalance: Int = balance

    def deposit(amount: Int): Unit =
      if(amount > 0) {
        balance += amount
        publish()
      }

    def withdraw(amount: Int): Unit =
      if(amount > 0 && amount <= balance){
        balance -= amount
        publish()
      } else throw new Error("Insufficient funds")
  }

  class Consolidator(observed: List[BankAccount]) extends Subscriber {
    observed.foreach(_.subscribe(this))

    private var total: Int = _ // The variable is initially uninitialized
    compute()

    private def compute(): Unit =
      total = observed.map(_.currentBalance).sum

    def handler(pub: Publisher): Unit = compute()

    def totalBalance: Int = total
  }

  val a, b = new BankAccount
  val c = new Consolidator(List(a, b))
  c.totalBalance
  a deposit 20
  c.totalBalance
  b deposit 30
  c.totalBalance

  try {
    println("Everything is ok")
  } finally {
    println("End")
  }
}