import scala.collection.mutable 
object DNATools {
  def main(args: Array[String]): Unit = {

    /* own personal testing
    println(transcribe("GATGGAACTTGACTACGTAAATT"))

    println(reverseComplement("AAAACCCGGT"))

    println(countNucleotides("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"))

    println(pointMutations("GAGCCTACTAACGGGAT", "CATCGTAATGACGGCCT"))    

    println(findMotif("AACAACAACAA", "AACAA")) */
  }
    
    def countNucleotides (x: String): Map[Char, Int] = {
        
        //map
        val nucleotides = mutable.Map[Char, Int]('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0)

        //increment each nucleotide
        for (character <- x)
            nucleotides(character) = 1 + nucleotides.getOrElse(character, 0)

        //make immutable
        nucleotides.toMap
    }

    def pointMutations (x: String, y: String): Int = {

        //yield if chars are different
        (for (i <- 0 to x.length;
        if (x.charAt(i) != y.charAt(i)) 
        )yield "notSame").length

    }

    def findMotif (x: String, y: String): List[Int] = {

        //compare against all possible substrings
        if (y.length != 0){
            (for (i <- 0 to x.length; j <- i to x.length;
            if (x.substring(i, j).compareTo(y) == 0))
            yield i+1).toList
        }
        else{
            //if string is empty return nil
            return Nil
        }

    }

    def transcribe (x: String): String = {

        //transcribe
        (for (character <- x)
            yield if (character == 'T') 'U' else character).mkString

    }

    def reverseComplement (x: String): String = {

        //reverse
        (for (character <- x)
            yield if (character == 'A') 'T' else if (character =='T') 'A' else if (character =='G') 'C' else if (character =='C') 'G').mkString.reverse 

    }

    

}
