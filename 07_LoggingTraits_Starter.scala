// Code examples modified from 
// Scala for the Impatient by Horstmann


trait Logger {
  def log(msg: String) // abstract method
  
  // Traits can provide concrete implentations based on a few abstract methods
  def info(msg: String) = { log("INFO: " + msg) }
  def warn(msg: String) = { log("WARN: " + msg) }
  def severe(msg: String) = { log("SEVERE: " + msg) }
}


trait ConsoleLogger extends Logger {
  override def log(msg: String) = { println(msg) }
}

trait NullLogger extends Logger {
  override def log(msg: String) = { Unit }
}

class Account{
  var balance: Double = 0.0
}

// Can extend only one class, but multiple traits
class SavingsAccount extends Account with ConsoleLogger {
  def withdraw(amount: Double) = {
    if(amount > balance) log("Insufficient funds")
    else balance -= amount
  }
}

val acct = new SavingsAccount
acct.withdraw(10) // should get a message logged to console


class SavingsAccount extends Account with NullLogger {
  def withdraw(amount: Double) = {
    if(amount > balance) log("Insufficient funds")
    else balance -= amount
  }
}

val sal = new SavingsAccount
sal.withdraw(1000)


val sal2 = new SavingsAccount with ConsoleLogger
sal2.withdraw(1000)

trait TimeStampLogger extends Logger {
  abstract override def log(msg: String) = {
        super.log(new java.util.Date() + " " + msg)
  }
}

trait ShortLogger extends Logger {
  val maxLength = 15
  abstract override def log(msg: String) = {  
        super.log(
          if (msg.length <= maxLength) msg else msg.substring(0,maxLength - 3) + "..." )
  }
}

val lg = new ConsoleLogger with ShortLogger
lg.warn("I have a bad feeling about this...")

val a1 = new SavingsAccount with ConsoleLogger with TimeStampLogger with ShortLogger
val a2 = new SavingsAccount with ConsoleLogger with ShortLogger with TimeStampLogger

a1.withdraw(1000)
a2.withdraw(1000)