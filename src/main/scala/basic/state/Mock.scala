package basic.state

trait Mock {
  def nextInt:(Int,Mock)
}

object Mock {
  def nonNegativeIntExplict(rng: Mock): (Int,Mock) = {
    val (a,r) = rng.nextInt
    if(a < 0) (-(a + 1),r) else (a,r)
  }
  
  def intExplict(rng: Mock): (Int,Mock) = {
    val (a,r) = rng.nextInt
    (a * a,r)
  }
  
  def doubleExplict(rng: Mock): (Double,Mock) = {
    val (d,r) = nonNegativeIntExplict(rng)
    (d / (Int.MaxValue.toDouble + 1), r)    
  }
}
case class SimpleMock(seed:Long) extends Mock {
  def nextInt:(Int,Mock) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newMock = SimpleMock(newSeed)
    val n = (newSeed >>> 16).toInt
    (n,newMock)
  }
}