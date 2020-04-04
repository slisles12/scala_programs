object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Hello World!")
    val x = 7
    var y = 6
    //can't use because val x = 7;
    y = 9

    println("x is " + x + " y is " + y);

    println("abc"(1))

    println( "x = " + x)

    println( s"x = ${x}")
    
    println( f"x = $x")

    println( raw"x = ${x}")

    println(compare("hello", "hello"))

    println(compare("hello", "o"))

    val a = if( 3 > 5 )  "what?"  else "ok"
    val b = if( 3 > 5 ) -1 else true
    val c = if( 3 > 5 )  "what?"  else  true

    println(turns(1));
    println(turns(3));

    println(numTerms("3 + 2 + 5 + 6"));

    val e = List(); 
    val e1 = List(1,2,3,4);
    val e2 = Nil
    val myList = List("a", 7,  List("nested", "items"))
    val other = 7 :: 5 :: List(8, 3)
    val yetAnother = 7 :: 5 :: Nil

    println(sum(e1))
  }

    def compare (x: String, y: String): Boolean = {
        if (x.length() == y.length()){
            true
        }
        else{
            false
        }

    } 
    def turns(nTurns: Int): String = {
        if (nTurns > 1){
            val a = "You have " + nTurns + " turns remaining"
            a
        }
        else{
            val a = "You have " + nTurns + " turn remaining"
            a
        }
    }

    def numTerms(terms: String): Int = {
        terms.count(_ == '+' ) + 1
    }

    def sum(xs: List[Int]): Int = {
        xs match {
            case Nil => 0
            case _ => xs.head + sum(xs.tail)
        }
    }
}
