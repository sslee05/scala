package basic.state

import basic.state.RNG._

/**
 * 상태변경에 따른 참조투명을 지원하는 방법은 
 * 상태를 명시적으로 드러낸다(호출된 것의 상태 변경이 아닌
 * 상태가 변경된 것을 반환 한다.)
 */
trait RNG {
  def nextInt: (Int,RNG)
}

object RNG {
  //############ 명시적 상태전의 ###################
  
  //0이상 Int.MaxValue 이하의 난수 정수를 생성하라.
  def nonNegativeIntExplict(rng: RNG): (Int,RNG) = {
    val (a,r) = rng.nextInt
    if(a < 0) (-(a + 1),r) else (a,r)
  }
  
  //0 이상 1미만의 난수를 발생하는 함수를 작성하라.
  def doubleExplict(rng: RNG): (Double,RNG) = {
    val (d,r) = nonNegativeIntExplict(rng)
    (d / (Int.MaxValue.toDouble + 1), r)    
  }
  
  //난수쌍 (Int,Double)을 발생하는 함수를 작성하라.
  def intDoubleExplict(rng: RNG): ((Int,Double),RNG) = {
    val (i,r) = nonNegativeIntExplict(rng)
    val (d,r2) = doubleExplict(r)
    ((i,d),r2)
  }
  
  //난수쌍 (Double,Int)을 발생하는 함수를 작성하라.
  def doubleIntExplict(rng: RNG): ((Double,Int),RNG) = {
    val (d,r) = doubleExplict(rng)
    val (i,r2) = nonNegativeIntExplict(r)
    ((d,i),r2)
  }
  
  //3tuple (Double,Double,Double) 의 난수를 발생하는 함수를 작성하라.
  def double3Explict(rng: RNG): ((Double,Double,Double),RNG) = {
    val (d1,r1) = doubleExplict(rng)
    val (d2,r2) = doubleExplict(r1)
    val (d3,r3) = doubleExplict(r2)
    
    ((d1,d2,d3),r3)
  }
  
  //정수들의 난수들의 목록을 생성하는 함수를 작성하라.
  def intsExplict(n: Int)(rng: RNG): (List[Int],RNG) = {
    
    @annotation.tailrec
    def go(xs: List[Int],n:Int)(rng: RNG): (List[Int],RNG) = {
      if(n > 0) {
        val (i,r) = rng.nextInt
        go(i::xs,n-1)(r)
      }
      else (xs,rng)
    }
    
    go(List(),n)(rng)
  }
  
  //############ HOF 상태전의 ###################
  
  //상태전의의 일반화를 선언
  type Rand[+A] = RNG => (A,RNG)

  //정수의 난수를 발생함수를 작성하라.
  //val int:Rand[Int] = rng => rng.nextInt
  val int:Rand[Int] = _.nextInt
  
  //상태전의를 하지 않고 그대로 전달하는 함수를 작성하라.
  def unit[A](a:A):Rand[A] = rng => (a,rng)
  
  //상태전의 map 함수를 작성하라.
  def map[A,B](s:Rand[A])(f: A => B):Rand[B] = {
    rng => {
      val (a,r) = s(rng)
      (f(a),r)
    }
  }
  
  //map 와 nonNegativIntExplict를 재사용하여 0보다 크고
  //2로 나누어 지는 Int를 발생하는 함수ㅠ nonNegativeEven를 
  //구현하라.
  def nonNegativeIntEven:Rand[Int] = 
    map(nonNegativeIntExplict)(a => a - (a % 2))
    
  //double 을 map을 이용하여 재구성하라.
  def double:Rand[Double] = 
    map(nonNegativeIntExplict)(a => a / (Int.MaxValue.toDouble +1))
    
  //상태동작들의 조합을 할 수 있는 함수를 구현하라.
  def map2[A,B,C](rndA:Rand[A],rndB:Rand[B])(f:(A,B) => C):Rand[C] = rng => {
    val (a,r) = rndA(rng)
    val (b,r2) = rndB(r)
    (f(a,b),r2)
  }
  
  //map2를 이용하 intDoubleExplict 와 doubleIntExplict를 재구성 하라.
  def intDouble:Rand[(Int,Double)] = map2(int,double)((a,b) => (a,b))
  def doubleInt:Rand[(Double,Int)] = map2(double,int)((b,a) => (b,a))
  
  //상태전이의 list를 하나의 상태전의로 조합하는 sequence 를 구현하라.
  def sequence[A](xs:List[Rand[A]]):Rand[List[A]] = 
    xs.foldRight(unit[List[A]](List()))((a,g) => map2(a,g)((x,y) => x::y))
    
  //flatMap을 구현하라.
  //flatMap을 이용하면 Rand[A] 의 값에 기초하여 Rand[B]를 선택할 수 있다.
  //아래 test,test02,test03 을 참고 
  def flatMap[A,B](ra:Rand[A])(f:A => Rand[B]):Rand[B] = rng => {
    val (a,r) = ra(rng)
    f(a)(r)
  }
  
  /*
  def test(n:Int):Rand[Int] = 
    map(nonNegativeIntEven)(a => if(a > n) a else ??? 
  */
  
  def test02(n:Int):Rand[Int] = rng => {
    val (a,r) = nonNegativeIntEven(rng)
    if(a > n) (a,r)
    else test02(n)(r)
  }
  
  def test03(n:Int):Rand[Int] = 
    flatMap(nonNegativeIntEven)(a => if(a > n) unit(n) else test03(n))
    
  //map을 flatMap으로 재구현 하라.
  def mapViaFlatMap[A,B](ra:Rand[A])(f: A => B):Rand[B] = 
    flatMap(ra)(a => unit(f(a)))
    
  //map2를 flatMap으로 재구현 하라.
  def map2ViaFlatMap[A,B,C](ra:Rand[A],rb:Rand[B])(f:(A,B) => C):Rand[C] = 
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}

object RNGDriver extends App {
  
  //test nextInt
  val rng = SimpleRNG(42)
  println(rng.nextInt)
  println(rng.nextInt)
  
  //test nonNegativeIntExplict
  val (a,r) = nonNegativeIntExplict(rng)
  println(a)
  val (a2,r2) = nonNegativeIntExplict(r)
  val (a3,r3) = nonNegativeIntExplict(rng)
  println("new value:"+a2 + " same value:" + a3)
  
  //test 3tuple
  val ((d1,d2,d3),r4) = double3Explict(rng)
  println(d1+":"+d2+":"+d3+":"+r4)
  
  //test intsExplict
  val (xs,r5) = intsExplict(5)(rng)
  println(xs + ":" + r5)
  
  //test nonNegativeIntEvent
  val rngx = nonNegativeIntEven
  println(rngx(rng))
  
  //test double
  val rnd01 = double
  println(rnd01(rng))
  
  //test map2
  val rngx02 = map2(int,double)((a,b) => (b % a))
  println(rngx02(rng))
  
  //test sequence
  val xs07 = List(nonNegativeIntEven,double,int)
  val rs07 = sequence(xs07)
  println(rs07(rng))
  
  //test test03
  val xs08 = test03(10)
  println(xs08(rng))
  
  //test mapViaFlatMap
  val rs09 = mapViaFlatMap(nonNegativeIntEven)(a => a + 2)
  println(rs09(rng))
  
  
}

case class SimpleRNG(seed:Long) extends RNG {
  def nextInt:(Int,RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n,newRNG)
  }
}

