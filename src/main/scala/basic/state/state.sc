package basic.state

object state {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def test01(s:String) = "hi " + s                //> test01: (s: String)String
  
  def test02(s:String) = "hellow " + s            //> test02: (s: String)String
  
  val test03 = test01 _                           //> test03  : String => String = basic.state.state$$$Lambda$8/1349414238@566776a
                                                  //| d
  val test04 = test02 _                           //> test04  : String => String = basic.state.state$$$Lambda$9/1627960023@1554909
                                                  //| b
  
  val test05 = test01 _ compose test02            //> test05  : String => String = scala.Function1$$Lambda$12/586617651@13969fbe
  
  println(test05("scala"))                        //> hi hellow scala
  
  def test = (s:String) => (i:Int) => i + 2       //> test: => String => (Int => Int)
  def test07(f:Int => Int):Unit = { () }          //> test07: (f: Int => Int)Unit
  val tt = test07 _ compose test                  //> tt  : String => Unit = scala.Function1$$Lambda$12/586617651@1a407d53
  
  
  import basic.state.State._
  import basic.state.CandySlotMachine._
  
  val test06 = modify[Machine] _ compose update   //> test06  : basic.state.Input => basic.state.State[basic.state.Machine,Unit] =
                                                  //|  scala.Function1$$Lambda$12/586617651@dfd3711
  
  val inputs = List(Coin,Turn)                    //> inputs  : List[Product with Serializable with basic.state.Input] = List(Coin
                                                  //| , Turn)
  val rs02 = inputs map (modify[Machine] _ compose update)
                                                  //> rs02  : List[basic.state.State[basic.state.Machine,Unit]] = List(State(basic
                                                  //| .state.State$$Lambda$24/1347870667@2344fc66), State(basic.state.State$$Lambd
                                                  //| a$24/1347870667@458ad742))
  val rs04 = sequence(rs02)                       //> rs04  : basic.state.State[basic.state.Machine,List[Unit]] = State(basic.stat
                                                  //| e.State$$Lambda$24/1347870667@7c0e2abd)
  val rs03 = rs02.head                            //> rs03  : basic.state.State[basic.state.Machine,Unit] = State(basic.state.Stat
                                                  //| e$$Lambda$24/1347870667@2344fc66)
}