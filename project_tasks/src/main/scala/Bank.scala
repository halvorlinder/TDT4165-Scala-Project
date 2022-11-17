class Bank(val allowedAttempts: Integer = 3) {

    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        val transaction =  new Transaction(this.transactionsQueue, this.processedTransactions, from, to, amount, this.allowedAttempts) //creates new transaction
        this.transactionsQueue.synchronized{
            transactionsQueue.push(transaction) //puts transaction in queue
        }
        val thread = Main.thread {
            processTransactions 
        }
        thread.join
    }
                                                // TODO
                                                // project task 2
                                                // create a new transaction object and put it in the queue
                                                // spawn a thread that calls processTransactions

    private def processTransactions: Unit = {
        while (!transactionsQueue.isEmpty){
            val trans = transactionsQueue.pop
            val thread = Main.thread {
                trans.run()            
                trans.status match{
                    case TransactionStatus.PENDING =>  {
                        this.transactionsQueue.synchronized   {
                            this.transactionsQueue.push(trans)
                        }
                        processedTransactions
                    }
                    case _ => this.processedTransactions.synchronized(this.processedTransactions.push(trans))
                }
            }
            thread.join
        }
    }
                                                // TOO
                                                // project task 2
                                                // Function that pops a transaction from the queue
                                                // and spawns a thread to execute the transaction.
                                                // Finally do the appropriate thing, depending on whether
                                                // the transaction succeeded or not

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}