import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

    private var queue = mutable.Queue() : mutable.Queue[Transaction]

    def pop: Transaction = this.queue.dequeue()

    def isEmpty: Boolean = this.queue.isEmpty

    def push(t: Transaction): Unit = this.queue.enqueue(t)

    def peek: Transaction = this.queue.front

    def iterator: Iterator[Transaction] = this.queue.iterator
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING
  var attempt = 0

  override def run: Unit = {

      def doTransaction() : Unit = {
        if (this.attempt == this.allowedAttemps) {
            this.status = TransactionStatus.FAILED
            return ()
        }
        this.from.withdraw(amount) match {
            case Left(amount) => to.deposit(amount) match {
                case Left(amount) => this.status = TransactionStatus.SUCCESS
                case _ => {
                    this.from.deposit(amount)
                }
            }
            case _ => ()
        }
        this.attempt = this.attempt + 1
      }

      if (status == TransactionStatus.PENDING) {

            // We need a total ordering to avoid deadlocks
            if (this.to.hashCode() > this.from.hashCode()){
                this.to.synchronized{this.from.synchronized{
                    doTransaction
                }
            }}
            else{
                this.from.synchronized{this.to.synchronized{
                    doTransaction
                }}
            }
          Thread.sleep(50) 
                           
      }
    }
}
