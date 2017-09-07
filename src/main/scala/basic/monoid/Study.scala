package basic.monoid

import java.util.concurrent.Executors

trait Study {
  
  //monoid 는 하나의 형식(Monoid[A] 의 A)이되, 그 형식(A)에 대해 결합법칙을 만족하며 항등(zero)을
  //가진 이항연산(op)이 존재하는 형식이다.
  //Monoid[A] 의 instance는 이 monoid 를 증명(증거)일 뿐이다.
}

//monoid law = 항등원의 법칙, 결합법칙
//monoid 구성
// 어떤 형식 A
// 그 형식 2개를 하나의 값을 산출하는 항등법칙과 결합법칙이 성립하는 이항연산
// 항등원
trait Monoid[A] {

  //op(op(a1,a2),a3) == op(a1,op(a2,a3))
  def op(a1: A, a2: A): A = ???

  //op(a1,zero) == op(zero,a1) == a1
  def zero: A = ???
}

object StudyDriver extends App {
  
  val stringMonoid = new Monoid[String] {
    override def op(s1: String, s2: String): String = s1 + s2
    override def zero: String = ""
  }
  
  val listMonoid = new Monoid[List[Int]] {
    override def op(xs: List[Int], ys: List[Int]): List[Int] = xs ++ ys
    override def zero: List[Int] = Nil
  }
  
  //ex-01)정수 덧셈과 곱셈에 대한 Monoid instance들과 해당 Boolean 연산자들을 제시하라.
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }
  
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }
  
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }
  
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }
  
  //ex-02)Option[String] 에 대한 Monoid를 작성하라.
  val optionMonoid = new Monoid[Option[String]] {
    override def op(o1: Option[String], o2: Option[String]): Option[String] = o1 orElse o2
    override def zero: Option[String] = None
  }
  
  val o1 = Some("a")
  val o2 = Some("b")
  val o3 = Some("c")
  //Option[String] 에 대한 결합법칙 
  val rs01 = optionMonoid.op(o1,optionMonoid.op(o2,o3))
  val rs02 = optionMonoid.op(optionMonoid.op(o1,o2),o3)
  println("rs01 :"+ rs01+ " rs02:"+rs02)
  
  //Option[String]에 대한 항등원법칙 
  val rs03 = optionMonoid.op(o1,optionMonoid.zero)
  val rs04 = optionMonoid.op(optionMonoid.zero,o1)
  println("rs03 :"+ rs03+ " rs03:"+rs04)
  
  //ex-03)인수의 형식과 반환값의 형식이 같은 함수를 자기함수(endofunction)이라 한다. 자기함수들을 위한 monoid를 작성하라.
  def endoM[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f1: A => A, f2: A => A): A => A = f1 compose f2
    override def zero: A => A = a => a
  }
  //endoMonoid Test
  //1 결합법칙 
  val endoMonoid = endoM[Int]
  val rs05 = endoMonoid.op(x => x + x, endoMonoid.op(y => y * y, z => z - 1))(3)
  val rs06 = endoMonoid.op(endoMonoid.op(x => x + x, y => y * y), z => z - 1)(3)
  println("rs05 :"+ rs05+ " rs06:"+rs06)
  
  //2.항등원법칙
  val rs07 = endoMonoid.op(x => x + x, endoMonoid.zero)(3)
  val rs08 = endoMonoid.op(endoMonoid.zero, x => x + x)(3)
  println("rs07 :"+ rs07+ " rs07:"+rs07)
  
  //List의 foldRight 와 foldLeft에 대한 고찰
  val xs = List("ss","lee"," study"," scala")
  val rs09 = xs.foldLeft(stringMonoid.zero)(stringMonoid.op)
  println(rs09)
  
  // List의 목록 형식이 Monoid instance의 형식과 부합하지 않는다면 map를 이용하면 된다.
  //ex-04) foldMap를 구현하라.
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = 
    as.foldLeft(m.zero)((b,a) => {
      println(b+":"+a)
      m.op(b,f(a))
    }) // b(f(a))
  
  val xs02 = List(1,2,3,4,5)
  val rs10 = foldMap(xs02,stringMonoid)(x => x.toString)
  println(rs10)
  
  //ex-05 foldRight 을 foldMap으로 구현하라.
  //f:(A,B) => B 는 f.curried : A => (B => B) 여기서 B => B type은 호출하고 있는 foldMap의 또 다른 B type 이다.
  // 즉 type B[B] = B => B 
  // foldMap의 B type을 B => B 로 생각하면 됨.
  def foldRightViaFoldMap[A,B](xs:List[A])(z:B)(f:(A,B) => B): B = {
		  val t = foldMap(xs,endoM[B])(f.curried)
      t(z)
  }
  
  // foldMap 은 다음과 같이 진행 (foldMap 안에서 endoMonoid에서 g(f(x))를 하고 있고 
  // foldLeft를 사용하고 있다. 따라서 head 쪽에서 tail쪽으로 f(x)가 g(x) 로 induction 되므로 head 쪽에서 tail쪽을 받는 형태가 된다. 
  // f(1) => (x => 1.toString + x)  result: 1.toString + x
  // f(2) => (x => 2.toString + x)  result: 1.toString + (x => 2.toString + x)
  // f(3) => (x => 3.toString + x)  result: 1.toString + 2.toString + (x => 3.toString + x)
  // f(4) => (x => 4.toString + x)  result: 1.toString + 2.toString + 3.toString + (x => 4.toString + x)
  // f(5) => (x => 5.toString + x)  result: 1.toString + 2.toString + 3.toString + 4.toString + (x => 5.toString + x)
  // 이후 endoMonoid op 에서  x => x 
  val rs11 = foldRightViaFoldMap(xs02)(stringMonoid.zero)((a,b) => {
    println("##"+a+":"+ b)
    a.toString + b
  })
  println("#########")
  println(rs11)
  
  //ex-06 foldLeft을 foldMap으로 구현하라.
  def dualMonoid[A](m: Monoid[A]) = new Monoid[A] {
    override def op(f1: A, f2: A) = m.op(f2,f1)
    override def zero: A = m.zero
  }
  
  def foldLeftViaFoldMap[A,B](xs: List[A])(z:B)(f: (B,A) => B): B = 
    foldMap(xs,dualMonoid(endoM[B]))(a => b => f(b,a))(z)
  
  // foldMap 은 다음과 같이 진행 foldLeft를 사용하고 있다. 
  // 따라서 head 쪽에서 tail쪽으로 f(x)가 g(x) 로 induction 되어야 할 것 같지만 dualMonoid에서 
  // f(g(x))를 하고 있어 (정의역과 치역이 뒤바뀐다.) tail 쪽에서 header쪽으로 g(x)가 f(x)를 받고 있다.
  //   공역(X)                           정의역(Y)
  // f(1) => (x => 1.toString + x)  result: 1.toString + x
  // f(2) => (x => 2.toString + x)  result: 2.toString + (x => 1.toString + x) 
  // f(3) => (x => 3.toString + x)  result: 3.toString + (x => 2.toString + 1.toString + x)
  // f(4) => (x => 4.toString + x)  result: 4.toString + (x => 3.toString + 2.toString + 1.toString + x)
  // f(5) => (x => 5.toString + x)  result: 5.toString + (x => 4.toString + 3.toString + 2.toString + 1.toString + x)
  val rs12 = foldLeftViaFoldMap(xs02)(stringMonoid.zero)((b,a) => {
    println("##"+a+":"+ b)
    a.toString + b
  })
  println(rs12)
  
  //IndexedSeq에 대한 foldMap을 구현하라. 구현은 반드시 수차열을 둘로 분할해서  
  //재귀적으로 각 절방을 처리하고 그결과들을 monoid를 이용해서 결합해야 한다.
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty) m.zero
    else if(v.size == 1) f(v(0))
    else {
      println(v.size)
      val (l,r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l,m)(f), foldMapV(r,m)(f))
      
    }
  }
  
  val xs13 = IndexedSeq(1,2,3,4,5) 
  val rs13 = foldMapV(xs13,stringMonoid)(x => x.toString)
  println(rs13)
  
  import basic.parallel.Par._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(p1: Par[A], p2: Par[A]): Par[A] = p1.map2(p2)(m.op)
    override def zero: Par[A] = unit(m.zero)
  }
  
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    foldMapV(v,par(m))(asyncF(f))
    
//    val r: Par[IndexedSeq[B]] = parMap(v)(f)
//    r.flatMap(seqB => foldMapV(seqB,par(m))(b => lazyUnit(b)) )
  }
  
  val rs14 = parFoldMap(xs13,stringMonoid)(x => x.toString())
  val es = Executors.newFixedThreadPool(5)
  val time1 = System.nanoTime()
  println(rs14(es))
  val time2 = System.nanoTime()
  println(time2 - time1)
  println("###")
  
  def ordered(xs: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int,Int,Boolean)]] {
      override def zero = None
      override def op(op1: Option[(Int,Int,Boolean)], op2: Option[(Int,Int,Boolean)]) : Option[(Int,Int,Boolean)] = {
        (op1,op2) match {
          case (None,x) => x
          case (x,None) => x
          case (Some((x,y,z)),Some((x1,y1,z1))) => 
            Some((x min x1, y max y1 , z && z1 && y <= x1))
        }
      }
    }
    
    foldMapV(xs,m)(x => Some(x,x,true)).map(x => x._3).getOrElse(true)
  }
  
  val xs14 = IndexedSeq(12,3,5,4)
  println(ordered(xs14))
  
}