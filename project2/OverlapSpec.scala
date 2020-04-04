import org.scalatest._

class OverlapSpec extends FlatSpec with Matchers {
  
  "OverlapFunction" should "have an apply method that returns a (String, String) => Boolean" in {
    val fn: (String, String) => Boolean = OverlapFunction(4)
  }
  
  it should "detect length-two overlaps" in {
      val overlap2 = OverlapFunction(2)
      
      overlap2("AAAATTTT", "TTTTGGGG") should equal (true)
      overlap2("ACGGACTC", "TCTCGAAA") should equal (true)
      overlap2("CCCCGGAA", "ACGAACCC") should equal (false)
      overlap2("TTTTGGGG", "AAAATTTT") should equal (false)
      overlap2("AAATTGGG", "GGGGACAC") should equal (true)
      overlap2("AAATGTGG", "GGGGAC") should equal (true)
      overlap2("AAATGGTG", "GGGGA") should equal (false)
      overlap2("AAATGGGT", "GGGG") should equal (false)
      overlap2("AAAAAA", "AAAAAA") should equal (false)
      overlap2("AT", "AT") should equal (false)
  }
  
  it should "detect length-four overlaps" in {
      val overlap4 = OverlapFunction(4)
      
      overlap4("AAAATTTT", "TTTTGGGG") should equal (true)
      overlap4("ACGGACTC", "ACTCGAAA") should equal (true)
      overlap4("CCCCGGAA", "CGGAACCC") should equal (false)
      overlap4("TTTTGGGG", "AAAATTTT") should equal (false)
      overlap4("AAATTGGG", "GGGGACAC") should equal (false)
      overlap4("AAATGTGG", "GGGGAC") should equal (false)
      overlap4("AAATGGTG", "GGGGA") should equal (false)
      overlap4("AAATGGGT", "GGGG") should equal (false)
      overlap4("AAAAAA", "AAAAAA") should equal (false)
  }
  
}