package basic.preview

object Currying extends App {
  
  def test(s1:String,s2:String) = s1 + s2
  val partFn = (test _).curried 
  //partFn: String => (String => String) = scala.Function2$$Lambda$1026/178917238@340a8894
  val unrruiedFn = Function.uncurried(partFn);
  //res1: (String, String) => String = scala.Function$$$Lambda$1054/376668615@608b1fd2
  
  //test02 와 test03은 같
  //partFn과 test04의 function signature가 일치한다.
  //test04: String => (String => String)
  def test02(s1:String)(s2:String) = s1 + s2
  def test03(s1:String) = (s2:String) => s1 + s2
  def test04 = (s1:String) => (s2:String) => s1 + s2
  
}