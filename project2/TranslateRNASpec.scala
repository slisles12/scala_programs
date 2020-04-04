import org.scalatest._

class TranslateRNASpec extends FlatSpec with Matchers {

  "translateRna" should "translate RNA sequences to amino acid sequences" in {
    ProjectTwo.translateRna("UAG") should equal ("")
    ProjectTwo.translateRna("GCCUAA") should equal ("A")
    ProjectTwo.translateRna("CGAUAG") should equal ("R")
    ProjectTwo.translateRna("UAUUUGAAGGGAUAUUGA") should equal ("YLKGY")
    ProjectTwo.translateRna("UAUCGACGAUAA") should equal ("YRR")
  }
 
}