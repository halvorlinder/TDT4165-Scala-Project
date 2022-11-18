class Bank(val allowedAttempts: Integer = 3) {

    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        val transaction =  new Transaction(this.transactionsQueue, this.processedTransactions, from, to, amount, this.allowedAttempts) 
        val thread = Main.thread {
            this.processTransaction(transaction)
        }
    }

    private def processTransaction(trans: Transaction): Unit = {
        trans.run()            
        trans.status match{
            case TransactionStatus.PENDING =>  {
                this.processTransaction(trans)
            }
            case _ => 
                this.processedTransactions.synchronized{
                    this.processedTransactions.push(trans)
                }
        }
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        this.processedTransactions.iterator.toList
    }

    def main(args: Array[String]): Unit = {

    }

}