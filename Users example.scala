
class NotCase(val x: Int, val y: String)
case class YesCase(x: Int, y: String)    // val is automatic

object Users {
  def main(args: Array[String]): Unit = {
    val nc = new NotCase(7, "abc")
    val yc = YesCase(20, "xyz") // no "new" with automatic companion object

    // Case classes override toString (conversion to String)
    println(nc)    // NotCase@cabe9c1
    println(yc)    // YesCase(20,xyz)

    // Java/Scala default .equals uses reference equality
    //   (i.e., checks if objects are in the same memory location)
    val otherNC = new NotCase(7, "abc")
    otherNC == nc     // false

    // Case classes override .equals to check constructor arguments
    val otherYC = YesCase(20, "xyz")
    yc == otherYC     // true
  }
}