import scala.collection.mutable 
import scala.annotation.tailrec

object Exam2Prep {
  def main(args: Array[String]): Unit = {

    //problem one
    List(1, 2, 3) map { (_)/2.0 }

    //problem seven
    List(-5, 0, 3).map { x: Int => if(x <= 0) 0 else 1 }

    (for ((x) <- (List(-5, 0, 3)))
      yield if(x <= 0) 0 else 1).toList


    //problem nine
    val f = isAnagram("computer science");

    println(f("science computer"))

    //problem ten
    val xs = List("science computer", "lol", "scicomp enceuter")
    val anagrams = (for(x <- xs if (f(x))) yield x).toList
    println(anagrams)

  }

  //problem two
  trait Weighted[A] { 
    val items: Iterable[A] 
    val weightingFn: A => Double

    def weights: Iterable[Double] = { 
      (for(item <- items) yield weightingFn(item)).toIterable
    } 
    
    def totalWeight: Double = { 
      (for(item <- items) yield weightingFn(item)).sum 
    } 
    
    def sumIf(p: A => Boolean): Double = { 
        (for(item <- items if (p(item))) yield weightingFn(item)).sum
    } 
  }

  //problem three
  def findBest[A](xs: Iterable[A], f: A=>Double): (A, Double) = {

    implicit val order = Ordering.Double.TotalOrdering

    val map: Map[A, Double] = (for(item <- xs) yield (item -> f(item))).toMap
    
    return map.maxBy(_._2)

  }

  //problem four
  def pickLonger(xs: Iterable[String], xy: Iterable[String]): Iterable[String] = {
    for ((x, y) <- (xs zip xy))
      yield if(x.size > y.size) x else y
  }

  //problem five
  def pickLongest(xs: List[Iterable[String]]): Iterable[String] = {
    xs.foldLeft(xs.head) {
        (curr, next) => pickLonger(curr, next)
    }
  }

  //problem six
  def containsSecret(secret: String): String=>Boolean = {
    (word: String) => if (word.contains(secret)) true else false
  }

  //problem eight
  def isAnagram(s: String) = {
    (t: String) => t.toSeq.sorted.unwrap.toLowerCase() == s.toSeq.sorted.unwrap.toLowerCase()
  }

  defaddCurried(x: Double) = { (y: Int) => x + y }
  //problem eleven
  def countXOR[A](list: List[A], firstPred: A => Boolean, secondPred: A => Boolean): Int = {

    if (list.size == 0) {
      return 0
    }
    val current = list(0)

    if (firstPred(current) && secondPred(current)){
      return 0 + countXOR(list.slice(1,list.size), firstPred, secondPred)
    }

    if (firstPred(current) || secondPred(current)){
      return 1 + countXOR(list.slice(1,list.size), firstPred, secondPred)
    }
      
    return 0 + countXOR(list.slice(1,list.size), firstPred, secondPred)

  }

  //problen twelve
  def sumAbs(list: List[Double]): Double = {
    if (list.size == 0){
      return 0
    }
    return list(0).abs + sumAbs(list.slice(1,list.size))
  }


  //problen thirteen
  def prod(list: List[Double]): Double = {
    if (list.size == 0){
      return 1.0
    }
    return list(0) * prod(list.slice(1,list.size))
  }

import scala.annotation.tailrec
def sumAbs(items: List[Int]): Int = {
    @tailrec // tells compiler to try to optimize the helper
     def helper (items: List[Int], sumSoFar: Int): Int = {
        items match {
            case Nil => sumSoFar
            case h::tail => helper(tail, sumSoFar + sumAbs(tail))
        }
    } 
    helper(items, 0)
}


  //problem fourteen

  class Node(neighbors: Iterable[Node]){

  }

  def fullTree(depth: Int, numChildren: Int): Node = {

    

  }

}