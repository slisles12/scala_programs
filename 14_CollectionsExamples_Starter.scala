// Example: Extending Traversable
class Pair[A](val first: A, val second: A) extends Traversable[A] {
  def foreach[U](f: A => U) = {
    f(first)
    f(second)
  }
}


// Exercise: How it Works: Writing MyTraversable
trait MyTraversable[A] {
  def foreach[U](f: A => U)
  def filter(test: A => Boolean): Traversable[A] = {
    val result = scala.collection.mutable.ListBuffer[A]()
    
    // use an anonymous closure to modify result
    foreach( (x: A) => if(test(x)) result += x )
    result
  }
  // TODO: add a map method
}

class Foo[A](first: A, second: A) extends MyTraversable[A] {
  def foreach[U](f: A=>U) = { f(first); f(second) }
}


// Example: Extending Iterable
class IterablePair[A](val first: A, val second: A) extends Iterable[A] {
  
  // This class will let us create iterators for IterablePair
  private class MyPairIter extends Iterator[A] {
    var nextIndex = 0
    
    // To extend Iterator[A], we need to implement next and hasNext
    def next: A = {
        nextIndex += 1
        nextIndex match {
            case 1 => first
            case 2 => second
            case _ => throw new RuntimeException("bad iterator")
        }
    }
    
    def hasNext: Boolean = { nextIndex <= 1 }
  }
  
  // For IterablePair[A] to extend Iterable[A],
  //   we need to implement iterator method
  def iterator: Iterator[A] = { new MyPairIter }

}


// Exercise: How it Works: Writing MySeq
trait MySeq[A] {
  def iterator: Iterator[A]
  def length: Int
  def apply(index: Int): A
  
  // TODO: complete the reverse method
  def reverse: Traversable[A] = {
    val result = scala.collection.mutable.ListBuffer[A]()
	
	//foreach( (x: A) => x::result )
	//foreach( (x: A) => result.prepend(x) )
	for( i <- (0 until length) ) result.prepend( apply(i) )
    result
    
    // Hint: prepend to result using result.prepend(...) 

  }

    def zipWithIndex: Iterable[(A, Int)] = {
        val result = scala.collection.mutable.ListBuffer[(A, Int)]()
        for( i <- (0 until length) ) result.append( apply(i), i )
        result
  }

}
