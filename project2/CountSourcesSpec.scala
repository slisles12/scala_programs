import org.scalatest._

class CountSourcesSpec extends FlatSpec with Matchers {
  
  "countSources" should "return 6 for F" in {
    ProjectTwo.countSources("F") should equal (6)
  }
  it should "count for Y" in {
    ProjectTwo.countSources("Y") should equal (6)
  }
  it should "count for S" in {
    ProjectTwo.countSources("S") should equal (18)
  }
  it should "count for L" in {
    ProjectTwo.countSources("L") should equal (18)
  }
  it should "count for PHQ" in {
    ProjectTwo.countSources("PHQ") should equal (48)
  }
  it should "count for KIM" in {
    ProjectTwo.countSources("KIM") should equal (18)
  }
  it should "count for VADER" in {
    ProjectTwo.countSources("VADER") should equal (1152)
  }
  it should "count for ACDEFGHIKLMNPQRSTVWY" in {
    ProjectTwo.countSources("ACDEFGHIKLMNPQRSTVWY") should equal (215872)
  }
  it should "return the number of possible RNA sources, modulo 1000000 for a long amino acid sequence" in {
    val input = 
"ACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWY"
    ProjectTwo.countSources(input) should equal (799872)
  }
 
}