import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    def withdraw(amount: Double): Either[Double, Either[ NoSufficientFundsException , IllegalAmountException]] = this.balance.synchronized{
        if(this.balance.amount - amount < 0 ){
            return Right(Left(new NoSufficientFundsException()))
        }
        else if(amount < 0){
            return Right(Right(new IllegalAmountException()))
        }
        else{
            this.balance.amount = this.balance.amount - amount
            return Left(amount)
        }
    }
    def deposit (amount: Double): Either[Double, IllegalAmountException] = this.balance.synchronized{
        if (amount > 0){
            this.balance.amount = this.balance.amount + amount
            Left(amount)
        }
        else{
            Right(new IllegalAmountException())
        }
    }

    def getBalanceAmount: Double = this.balance.synchronized{this.balance.amount}

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}
