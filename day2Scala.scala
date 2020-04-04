object ScalaTesting {
  def main(args: Array[String]): Unit = {

    val mylist1: List[String] = List("hey", "hey", "hey", "hey") 

    println(countR(mylist1, "hey") + " heys in list");


    for (x <- List(3, -5, 12, -7, 18))
            yield if(x <= 0) 0 else Math.sqrt(x)
        
    for (x <- List(3, -5, 12, -7, 18)
        if (x > 0)
    ) yield Math.sqrt(x)

    val s = "3 + 45 + 12";

    println(numTerms(s) + " number of terms in " + s);

    pairs(3);

    val ages = Map( "Peter" -> 42, "Jane" -> 38, "Rashad" -> 52 )
    println(ages.keys)
    println(ages.values)
    println(ages("Peter"))
    println(ages("Peter"))
    //println(ages("Peter") += 10) not possible
    //println(ages("nope"))
    println(ages.contains("Peter"))
    println(ages.contains("nope"))
    println(ages.get("Peter"))
    println(ages.get("nope")) 
    println("IMPORTANT LAST TWO")
    println(ages.getOrElse("Peter", 0))
    println(ages.getOrElse("nope", 0))
    for( x <- ages 
    ) println(x)


    val hadBirthday= List("Peter", "Rashad")
    println((for( x <- ages;
                name = x._1;
                age = x._2
                ) yield (name, if(hadBirthday.contains(name)) age + 1 else age )).toMap)
  }

        
    def countR(a: List[String], b: String): Integer = {
        a match {
            case Nil => 0
            case _ => (if(a.head == b) 1 else 0) + countR(a.tail, b)
        }
    }


    def pairs(n: Int) {
        for(i <- 0 to n; 
            j <- i to n; 
            if (i < j)
        )println("(" + i + "," + j + ")")
    }

    def findMatching(a: String, b: List[String]): List[String] = {
        for(word <- b
        if (word.contains(a))
        ) yield word
    }

    def numTerms(s: String): Int = {

        (for(i <- 0 until s.length; 
            if(s(i) == '+')
            ) yield 1).length + 1

    }

    def countA(word: String) : Int = {
        (for (character <- word
            if (character == 'a')
            ) yield character).length 
    }

    def frequencyTable(words: List[String]): Map[String, Int] {
        val wordCount = mutable.Map[String, Int]()
        for (word <- words) {
            wordCount(word) = 1 + wordCount.getOrElse(word,0)
        } 
        wordCount.toMap
    }
}


