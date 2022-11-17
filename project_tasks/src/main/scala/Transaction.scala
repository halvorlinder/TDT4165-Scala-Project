import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

    // TODO
    // project task 1.1
    // Add datastructure to contain the transactions
    private var queue = mutable.Queue() : mutable.Queue[Transaction]

    // Remove and return the first element from the queue
    def pop: Transaction = this.queue.dequeue()

    // Return whether the queue is empty
    def isEmpty: Boolean = this.queue.isEmpty

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = this.queue.enqueue(t)

    // Return the first element from the this.queue.without removing it
    def peek: Transaction = this.queue.front

    // Return an iterator to allow you to iterate over the queue
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

  override def run: Unit = this.to.synchronized{this.from.synchronized{

      def doTransaction() : Unit = {
        if (this.attempt == this.allowedAttemps) {
            this.status = TransactionStatus.FAILED
            return ()
        }
        // TODO - project task 3
        // Extend this method to satisfy requirements.
        this.from.withdraw(amount) match {
            case Left(amount) => to.deposit(amount) match {
                case Left(amount) => this.status = TransactionStatus.SUCCESS
                case _ => {
                    this.from.deposit(amount)
                    this.attempt = this.attempt + 1
                }
            }
            case _ => this.attempt = this.attempt + 1
        }
      }

      // TODO - project task 3
      // make the code below thread safe
      if (status == TransactionStatus.PENDING) {
          doTransaction
          Thread.sleep(50) // you might want this to make more room for
                           // new transactions to be added to the queue
      }
    }}
}
