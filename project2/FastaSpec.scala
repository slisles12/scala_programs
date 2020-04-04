import org.scalatest._

class FastaSpec extends FlatSpec with Matchers {

  "Fasta class" should "have name and dna members in the proper order" in {
    val fOne = new Fasta("the_name", "CCTAGA");
    fOne.name should equal ("the_name")
    fOne.dna should equal ("CCTAGA")
  }
  
  
  "parseFastas" should "parse one single-line sequence" in {
    val fsList: List[Fasta] = Fasta.parseFastas(">Rosalind_11\nCCTG")
    
    fsList.size should equal (1)
    fsList.head.name should equal ("Rosalind_11")
    fsList.head.dna should equal ("CCTG")
  }
  
  it should "parse one multi-line sequence" in {
    val fsList: List[Fasta] = Fasta.parseFastas(">Rosalind_2222\nA\nT\nG\nT\nA")
    
    fsList.size should equal (1)
    fsList.head.name should equal ("Rosalind_2222")
    fsList.head.dna should equal ("ATGTA")
  }
  
  it should "parse multiple single-line sequences" in {
    val fsList: List[Fasta] = Fasta.parseFastas(">Rosalind_3\nAAATG\n>Rosalind_4\nGGT\n>Rosalind_5\nCCTG")
    
    fsList.size should equal (3)
    
    fsList(0).name should equal ("Rosalind_3")
    fsList(0).dna should equal ("AAATG")
    
    fsList(1).name should equal ("Rosalind_4")
    fsList(1).dna should equal ("GGT")
    
    fsList(2).name should equal ("Rosalind_5")
    fsList(2).dna should equal ("CCTG")
  }
  
  it should "parse multiple multi-line sequences" in {
    val fsList: List[Fasta] = Fasta.parseFastas(">Rosalind_4853\nA\nAT\nTT\n>Rosalind_378590\nGCT\n>Rosalind_7485\nACT\nG")
    
    fsList.size should equal (3)
    
    fsList(0).name should equal ("Rosalind_4853")
    fsList(0).dna should equal ("AATTT")
    
    fsList(1).name should equal ("Rosalind_378590")
    fsList(1).dna should equal ("GCT")
    
    fsList(2).name should equal ("Rosalind_7485")
    fsList(2).dna should equal ("ACTG")
  }
  
  "gcContent" should "return the fraction of nucleotides that are G and C" in {
    new Fasta("x", "CCTAGA").gcContent should equal (0.5)
    
    new Fasta("x", "CAAG").gcContent should equal (0.5)
    
    new Fasta("x", "CCAGCCAGCCAGCCAGCCAGCCAGCCAGCCAG").gcContent should equal (0.75)
  
    new Fasta("x", "CTTTGTTT").gcContent should equal (0.25)
  }
  
  it should "return 1.0 when all nucleotides are G or C" in {
    new Fasta("x", "G").gcContent should equal (1.0)
    new Fasta("x", "C").gcContent should equal (1.0)
    new Fasta("x", "GGCGGCCCGCGGGC").gcContent should equal (1.0)
  }
  
  it should "return 0.0 when no nucleotides are G or C" in {
    new Fasta("x", "A").gcContent should equal (0.0)
    new Fasta("x", "T").gcContent should equal (0.0)
    new Fasta("x", "ATTTATTTTATAAATT").gcContent should equal (0.0)
  }
}