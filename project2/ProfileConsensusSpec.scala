import org.scalatest._

class ProfileConsensusSpec extends FlatSpec with Matchers {

 
  // for testing consensus and profile
  val dnaTestGroups = Seq(
    Seq( "T", "T", "T"),
    Seq( "T", "A", "G", "G" ),
    List("AGC", "AAT", "CAT"),
    List("AGGAA", "CTAAA", "CCCCA", "GGGGA"), // consensus should be CGGAA
    Seq("AATGCGTAC",
        "TATATCCGA",
        "AAAGAGGGA",
        "TTATAGAGG",
        "ACTCACGTA")
  )
  
  "profile" should "work for one sequence" in {
    ProjectTwo.profile(Seq("A")) should equal (Seq(Map('A' -> 1)))
    ProjectTwo.profile(Seq("ACG")) should equal (Seq(Map('A' -> 1), Map('C' -> 1), Map('G' -> 1)))
  }
  
  it should "work for multiple length-one sequences" in {
    ProjectTwo.profile(dnaTestGroups(0)) should equal (Seq(Map('T'->3)))
    ProjectTwo.profile(dnaTestGroups(1)) should equal (Seq(Map('T'->1, 'A'->1, 'G'->2)))
  }
  
  it should "work for case 2" in {
    ProjectTwo.profile(dnaTestGroups(2)) should equal (Seq(
      Map('A'->2, 'C'->1),
      Map('G'->1, 'A'->2),
      Map('T'->2, 'C'->1)
    ))
  }
  
  it should "work for case 3" in {
    ProjectTwo.profile(dnaTestGroups(3)) should equal (Seq(
      Map('A'->1, 'C'->2, 'G'->1),
      Map('G'->2, 'T'->1, 'C'->1),
      Map('G'->2, 'A'->1, 'C'->1),
      Map('A'->2, 'C'->1, 'G'->1),
      Map('A'->4)
    ))
  }
  
  it should "work for case 4" in {
    ProjectTwo.profile(dnaTestGroups(4)) should equal (Seq(
      Map('A'->3, 'T'->2),
      Map('A'->3, 'T'->1, 'C'->1),
      Map('T'->3, 'A'->2),
      Map('G'->2, 'A'->1, 'C'->1, 'T'->1),
      Map('A'->3, 'T'->1, 'C'->1),
      Map('G'->3, 'C'->2),
      Map('G'->2, 'A'->1, 'C'->1, 'T'->1),
      Map('G'->3, 'A'->1, 'T'->1),
      Map('A'->3, 'C'->1, 'G'->1)
    ))
  }
  
  
  
  "consensus" should "work for one sequence" in {
    ProjectTwo.consensus(Seq("A")) should equal ("A")
    ProjectTwo.consensus(Seq("ACG")) should equal ("ACG")
  }
  
  it should "work for multiple length-one sequences" in {
    ProjectTwo.consensus(dnaTestGroups(0)) should equal ("T")
    ProjectTwo.consensus(dnaTestGroups(1)) should equal ("G")
  }
  
  it should "work for case 2" in {
    ProjectTwo.consensus(dnaTestGroups(2)) should equal ("AAT")
  }
  
  it should "work for case 3" in {
    ProjectTwo.consensus(dnaTestGroups(3)) should equal ("CGGAA")
  }
  
  it should "work for case 4" in {
    ProjectTwo.consensus(dnaTestGroups(4)) should equal ("AATGAGGGA")
  }
 
}