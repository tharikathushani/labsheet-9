object q3 {
  class Account(val accountNumber: String, var balance: Double) {
    require(balance >= 0, "Initial balance cannot be negative")

    def deposit(amount: Double): Unit = {
      require(amount > 0, "Deposit amount must be positive")
      balance += amount
    }

    def withdraw(amount: Double): Unit = {
      require(amount > 0, "Withdrawal amount must be positive")
      require(amount <= balance, "Insufficient balance")
      balance -= amount
    }

    def transfer(amount: Double, toAccount: Account): Unit = {
      require(amount > 0, "Transfer amount must be positive")
      require(amount <= balance, "Insufficient balance for transfer")

      balance -= amount
      toAccount.deposit(amount)
    }

    override def toString: String = s"Account $accountNumber - Balance: $balance"
  }

  object Main extends App {
    val account1 = new Account("A123", 1000.0)
    val account2 = new Account("B456", 500.0)

    println(account1)
    println(account2)

    account1.deposit(200.0)
    account2.withdraw(100.0)

    println(account1)
    println(account2)

    account1.transfer(300.0, account2)

    println(account1)
    println(account2)
  }

}
