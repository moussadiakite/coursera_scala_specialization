package parallelism_on_jvm2

object UniqueIds{
  var uidCount = 0L

  val x = AnyRef

  def getUniqueId(): Long = x.synchronized {
    uidCount = uidCount + 1
    uidCount
  }
}

class Account(private var amount: Int = 0) {
  val uid = UniqueIds.getUniqueId()

  def getAmount: Int = amount

  def lockAndTransfer(target: Account, n: Int): Unit =
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }

  def transfer(target: Account, n: Int): Unit =
    if(this.uid < target.uid) this.lockAndTransfer(target, n)
    else target.lockAndTransfer(this, -n)
}
