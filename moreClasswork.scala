import scala.collection.mutable 
object moreClasswork {
  def main(args: Array[String]): Unit = {

    // Map: Transforming collections
    val xs = List(1,5,10)
    println(xs)
    println(xs.map((x: Int) => 2 * x))


    val square = makePowerFn(2)
    val cube = makePowerFn(3)
    println(square(10))
    println(cube(10))

    val smallAndOdd= both( (x: Int) => x < 10, (x: Int) => x % 2 == 1)
    smallAndOdd(6) == false
    smallAndOdd(13) == false
    smallAndOdd(3) == true
    Range(-10, 30).toList.filter(smallAndOdd)

    println(count(List(1,2,3,4,1,1), 1))

    println(odds(List("hi", "middle", "bye", "end")))
    println(foo((x: Int) => 2 * x))

    println(contains(List("a", "b", "c"), "b"))
        
    val s = new Square(1.0)
    val l = new Line(1.0)
    val p = new Polygon(3) with Colorable

    val list = List(s,l,p)

    val f = (x: Int) => x * x
    val g = (y: Int) => y + 1

    val hOne = f andThen g
    val hTwo = f compose g

    hOne(1)
    hTwo(3)

    /*
    colorAll(new cmyk(0.5, 0.5, 0.0, 0.0), list)
    colorAll(new rgb(0.0, 1.0, 0.0), list)*/


    //transform(Traversable( { (x: Double) => x * x },  { (x: Double) => x - 1 } ), Vector(1,2,3)) 
  }

  // This function takes a function that takes an integer and returns a double
  // This function also takes a list of integers
  // The overall function returna  list of doubles
  def transform( fn: Int => Double, xs: List[Int] ): List[Double] = {
    for (number <- xs
    ) yield fn(number)
  }

  //bring in list, map each el of that list to the function and return it to b
  def myMap[A, B] (xs: List[A], f: A => B): List[B] = {
    xs match {
      case Nil => Nil
      case _ => f(xs.head)::myMap(xs.tail,f)
    }
  }

  //function that takes an int and returns a function that takes a double and returns a double
  def makePowerFn(power: Int ):  Double => Double= {
    (x: Double) => Math.pow(x, power)
  }

  //This function returns a function that returns true if both functions returns true for the same input
  def both[A](p1: A => Boolean, p2: A => Boolean): A => Boolean = {
    (a: A) => p1(a) && p2(a)
  }

  //This function takes two arguments
  //arg1 is a list of soem generic type
  //arg2 is an argument of the same type
  //returns the number of times that arg2 is in the list
  def count[A] (xs:List[A], y:A): Int = {
    xs match {
      case Nil => 0
      case _ => (if(xs.head == y) 1 else 0) + count(xs.tail, y)
    }
  }


  //Write a method countByFolding that takes a List[A] and a target
  // of type A and returns the number of occurrences of target in the List. 
  //Use foldLeft.
  def foo(f : Int => Double): List[Double] = {

    (for (i <- 0 to 100
    )yield f(i)).toList

  }


  //a function f as an argument. f should take an Int as its argument
  // and return a Double. The foo function should apply f to each integer
  // from 1 through 100 and return a List of those results.
  def cbf2[A] (lst: List[A], target: A): Int = {
      lst.foldLeft(0) { (sum: Int, nextVal: A) => if(nextVal == target) sum + 1 else sum}
  }

  //Write a method odds that takes a List of some generic
  //type and returns every other element in the List as a 
  //new List. For example, odds( List("hi", "middle", "bye",
  // "end") ) should return List("hi", "bye"). Your method
  // should use recursion (perhaps in a helper method) and
  // pattern matching.
  def odds[A](list: List[A]): List[A] = {

    val num = list.length

    list match {
      case Nil => Nil
      case list if (list.length % 2 == 0) => list.head :: odds(list.tail)
      case list if (list.length % 2 == 1) => odds(list.tail)
   }

  }

  //Write a function transform. It should take a Traversable[ A=>A ] 
  //and a Traversable[A] as its arguments, returning a Traversable[A]. (Traversable 
  //is a superclass of List, ListBuffer, Array, IndexedSeq, etc. You can use for-comprehensions 
  //to iterator through Traversables.) The first argument is a sequence of transformations to apply 
  //in order to each element in the latter argument. Each function is applied to each element.
  
  def transform[A] (fns: Iterable[A => A], elements: Iterable[A]): Iterable[A] = {
	  for(e <- elements) yield (fns.foldLeft(e) { (elem: A, fn: A => A) => fn(elem)})
  }
  

  //Write a method getAnswers[A,B](items: Traversable[A], f: A=>B):
  // Map[A, B]. It should return a map where the keys are each distinct
  // item in items and the values are the results from applying f to those keys.
  // That is, the value for each key k should be equal to f(k).
  
  def getAnswers[A,B](items: Traversable[A], f: A=>B): Map[A,B] = {
    val list = mutable.Map[A, B]();
    for(i <- items) {
      list(i) = f(i)
    }
    list.toMap
  }
  

  //Write a recursive function called "contains" that takes a List[A]
  //and an item of type A, returning true if the list contains x and
  //false if it does not. Use pattern matching. (Scala's List has 
  //a contains method, but for this problem you are writing your own.)
  def contains[A](list: List[A], a: A): Boolean = {
    list match {
      case Nil => false
      case list if (list.head == a) => true
      case list if (list.head != a) => contains(list.tail, a)
   }
  }

  //returns h(x) = f(g(x))
  def my_compose[A,B,C](f: B=>c, g: A=>B): A => C = {
    x: A => f(g(x))
  }

  //regular recursive boy
  def countRecursive[A] (items: List[A], p: A => Boolean): Int = { 
      items match {
          case head::rest => (if (p(head)) 1 else 0) + countRecursive(rest, p)
          case Nil => 0
      }
  }

import scala.annotation.tailrec

  //fancy recursion
  def countRecursive[A] (items: List[A], p: A => Boolean): Int = { 

      @tailrec
      def helper(items:List[A], p: A => Boolean, sumSoFar:Int): Int = {
          items match {
              case head::rest => helper(rest, p, sumSoFar + (if (p(head)) 1 else 0))
              case Nil => sumSoFar
          }
      }

      helper(items, p, 0)

  }

  def getInt(s: String): Option[Int] = { try {Some(s.toInt)} catch {case ex: java.lang.NumberFormatException => None}}

  // TODO: add a map method
  def map[B](test: A => B): Traversable[B] = {
    val result = scala.collection.mutable.ListBuffer[B]()
    foreach( (x: A) => result += test(x))
    result
  }

  object applyMethod {
    def main(args: Array[String]){
      // Example
      val example1 = Vector(1, -10, 4, 25).flatMap( (x: Int) => if(x > 0) Some(Math.sqrt(x.toDouble)) else None );
      println(example1);
      // .flatten
      val example2 = Vector(1, -10, 4, 25).map( (x: Int) => if(x > 0) Some(Math.sqrt(x.toDouble)) else None );
      println(example2.flatten);
    }
  }

}


object applyMethod {
  def main(args: Array[String]){
    val a = new myClass("Ethan");
    // Calls apply method
    println(a());
    // Calls apply method with a string
    println(a("class"));
    // Calls companion object
    val b = myClass();
    //
    print(b());
  }
}

// Apply Example
class myClass(val name:String) { 
  def apply() = println("Hello apply")
  def apply(msg:String) = println("Hello "  + msg)
}

object myClass {
  def apply() = new myClass("default")
}

//Write a class Counts with parameter type A (like "class Counts[A] { ... }"). 
//Counts will keep track of how many items of each value of type A have been "seen"
// so far. Counts should have a method add(x: A): Unit that increments the count for
// x by one, setting the count to one if this is the first time x has been added. Counts
// should also have a method getCount(x: A): Int that returns the number of times x has
// been added to this object.
class Counts[A] {

  private val m: mutable.Map[A,Int] = mutable.Map()

  def add(a:A) = {
    m(a) = 1 + m.getOrElse(a, 0)
  }
  
  def getCount(x: A): Int = {
    m.getOrElse(x,0)
  }
} 

//Write a class StringMod that takes one constructor argument: 
//a Traversable of some generic type. Write a method toString that 
//takes an optional argument note of type String, with default value 
//"NOTE: ". The toString method should return a String with each item
// in the Traversable on its own line, prefixed by the note. 
class StringMod[A](travers: Traversable[A]){

  def toString(note: String = "NOTE"): String ={
    (for (i <- travers) yield note + i).mkString
  }

}

//Write a sealed abstract class Color with three subclasses: Rgb, Cmyk, and Grayscale.
//Color should have three abstract methods, red, green, and blue. 
//Each of them should return the amount of that channel in the current color
// (i.e., a Double in [0.0, 1.0]).
//Color should have a concrete method luminosity: Double. Use the following
// formula to convert RGB to luminosity: 0.21 R + 0.72 G + 0.07 B.
sealed abstract class Color {

  def red(): Double 

  def blue(): Double

  def green(): Double

  def luminosity(): Double = {
    (0.21 * red() + 0.72 * green() + 0.07 * blue())
  }

}

//Write case classes Rgb, Cmyk, and Grayscale that inherit 
//from Color. Each should have constructor val's for the respective 
//channels (e.g., red, green, and blue for Rgb).
class rgb(red: Double, green: Double, blue: Double) extends Color{

  override def red(): Double = {red}
  override def green(): Double = {green}
  override def blue(): Double = {blue}

}

class cmyk(c: Double, m: Double, y: Double, k: Double) extends Color{

  override def red(): Double = {
    ((1-c) * (1-k))
  }
  override def green(): Double = {
    ((1-m) * (1-k))
  }
  override def blue(): Double = {
    ((1-y) * (1-k))
  }

}


//Grayscale is converted to RGB by using the single 
//grayscale value in all three channels
case class Grayscale(l: Double) extends Color {
	def red: Double = { l }
	def green: Double = { l }
	def blue: Double = { l }
	override def luminosity: Double = { l }
}

//Write a trait Colorable with a public var field color of type Color. 
//Assign color a default value in the Colorable trait (which makes it concrete
//instead of abstract). Override toString to prepend the color to the "normal" 
//string representation of the object that is Colorable.
trait Colorable{
  var c: Color = new cmyk(1,2,3,4)

  override def toString: String = {
    c.red() + c.blue() + c.green() + super.toString
  }
}

//Write a class Polygon that has a val numEdges.
class Polygon(val edges: Int) 

//Write a case class Line that has a val length of type
//Double. Line should implement Colorable.
case class Line(val len: Double) extends Colorable 

//Write a case class Square that inherits from Polygon and 
//also has a val edgeLength: Double. Square should implement Colorable.
case class Square(val edgelen: Double) extends Polygon(4) with Colorable 

/*
def colorAll(c: Color, objList: Iterable[Colorable]): Unit = {
	for(obj <- objList) obj.color = c
}*/










