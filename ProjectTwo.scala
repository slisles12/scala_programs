import scala.annotation.tailrec

class Fasta(val name:String, val dna:String){

    object Fasta {
        def parseFastas(s:String): List[String] = {
            val list = s.split("<").toList
            list
            //for (string <- list) yield(string + "\n\n").toList
        }
    }

    def gcContent(): Double = {
        val difference = ((for {i <- 0 to dna.length} yield if ((dna.charAt(i) == 'G') || (dna.charAt(i) == 'C')) 1 else 0).sum)
        difference/dna.length
    }
}

//val test = ">Rosalind_6404\nCCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC\nTCCCACTAATAATTCTGAGG\n>other_id\nCCGATTAGCT\n>SEQUENCE_48\nCCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT\nATATCCATTTGTCAGCAGACACGCCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGC\nTCCGCCGAAGGTCTATAGGCTATTCCA"


object ProjectTwo {

    // DONE
    // Problem 0 and 2
    def translateRna(s: String): String = {
        @tailrec
        def helper(translated: String, untranslated: String): String = {
            untranslated match {
                case untranslated if (untranslated.length() > 3) => helper(translated + CodonTable.getAminoAcid(untranslated.take(3)), untranslated.drop(3))
                case _ => translated
            }
        }
        helper("", s)
    }

    // DONE
    // Problem 3
    def countSources(s: String): Int = {
        (for (character <- s
        ) yield ProtienTable.getProtienCount(character)).foldLeft(1)(_*_%1000000) * 3 % 1000000
    }

    // TODO: 
    // Problem 4.1
    def profile(ss: Seq[String]): Seq[Map[Char, Int]] = {
        val positionGroups = for (i <- 0 until ss(0).length) yield (for (j <- 0 until ss.length) yield ss(j).charAt(i))

        for( x <- positionGroups
        ) yield x.groupBy( (c: Char) => c ).map( kv => (kv._1, kv._2.length) )
    }

    // TODO:
    // Problem 4.2
    def consensus(ss: Seq[String]): String = {
        for (i <- 0 until ss.length) yield profile(ss(i))
    }

}


object OverlapFunction {
  def apply(num: Int): (String, String) => Boolean = {

      (one: String, two: String) => {

          if ((one.length < num) || (two.length < num)){
              false
          }

          val pre = one.substring(0, num);
          val pos = two.substring(num, two.length)
          if (pre.equals(pos) && !(one.equals(two))){
              true
          }
          false
      }

  }
}


object NoisyOverlapFunction {
  def apply(num: Int, pointMutationsAllowed: Int): (String, String) => Boolean = {

      (one: String, two: String) => {

          if ((one.length < num) || (two.length < num)){
              false
          }

          val pre = one.substring(0, num);
          val pos = two.substring(num, two.length)
          val difference = (for {i <- 0 to pointMutationsAllowed} yield if (pre.charAt(i) == pos.charAt(i)) 1 else 0).sum
         
          if (pointMutationsAllowed < difference){
              true
          }
          false;
      }
  }
}



