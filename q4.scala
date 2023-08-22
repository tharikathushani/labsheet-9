object q4 {
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

  object Bank {
    def negativeBalanceAccounts(accounts: List[Account]): List[Account] =
      accounts.filter(_.balance < 0)

    def totalBalance(accounts: List[Account]): Double =
      accounts.map(_.balance).sum

    def applyInterest(accounts: List[Account]): List[Account] =
      accounts.map { account =>
        val interestRate = if (account.balance >= 0) 0.05 else 0.1
        val interest = account.balance * interestRate
        account.deposit(interest)
        account
      }
  }

  object Main extends App {
    val account1 = new Account("A123", 100.0)
    val account2 = new Account("B456", -200.0)
    val account3 = new Account("C789", 500.0)

    val bankAccounts = List(account1, account2, account3)

    val negativeBalances = Bank.negativeBalanceAccounts(bankAccounts)
    val totalBalances = Bank.totalBalance(bankAccounts)
    val accountsWithInterest = Bank.applyInterest(bankAccounts)

    println("Negative balance accounts:")
    negativeBalances.foreach(println)

    println(s"Total balance of all accounts: $totalBalances")

    println("Final balances after applying interest:")
    accountsWithInterest.foreach(println)
  }

}
