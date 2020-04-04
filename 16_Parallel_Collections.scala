import scala.collection.parallel.CollectionConverters._ 

  

object Demo { 

  def main(args: Array[String]): Unit = { 

    val v = Vector.range(0, 10) 

    v.foreach(print) 

    println() 

    println("now with par") 

    v.par.foreach(print) 

    println() 

  } 

} 

 

 

Change 1: 

import scala.collection.parallel.immutable.ParVector  

//import scala.collection.parallel.mutable.ParArray  

val v = ParVector.range(0, 10) 

v.foreach(print) 