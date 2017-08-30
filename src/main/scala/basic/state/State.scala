package basic.state

import basic.state.State._

/**
 * 상태전이를 좀더 일반화 한다.
 * S => (A,S) 이것을 State class로 표현한다.
 */
case class State[S,+A](run:S => (A,S)) {
  
  def flatMap[B](f: A => State[S,B]):State[S,B] = State(s => {
    val (a,s1) = run(s)
    f(a).run(s1)
  })
  
  def map[B](f:A => B):State[S,B] = 
    flatMap(a => unit(f(a)))
  def map2[B,C](s:State[S,B])(f:(A,B) => C):State[S,C] = 
    flatMap(a => s.map(b => f(a,b)))
    
}

object State {
  
  def unit[S,A](a:A):State[S,A] = State(s => (a,s))
  
  def sequenceExplict[S,A](xs:List[State[S,A]]):State[S,List[A]] = {
    
    @annotation.tailrec
    def go(s:S,actions:List[State[S,A]],acc:List[A]):(List[A],S) = {
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2,t, a :: acc) }
      }
    }
    
    State( s => go(s,xs,List()) )
  }
  
  /**
   * recursive tail 하지 못하다.
   */
  def sequenceViaFoldRight[S,A](xs:List[State[S,A]]):State[S,List[A]] = 
    xs.foldRight(unit[S,List[A]](List()))((a,g) => a.map2(g)((x,y) => x::y))
  
    /**
     * 아래의 foldLeft는 reverse를 하므로 foldRight보다 속도가 느릴것 같지만
     * 실제로 foldRight보다 빠르다. 이유는 foldRight는 Nil까지 갔다가 stack frame
     * 이 다시 원상태로 와야 하기 때문에 foldRight도 2 번 종단을 하기 때문이다.
     */
  def sequenceViaFoldLeft[S,A](xs:List[State[S,A]]):State[S,List[A]] = 
    xs.reverse.foldLeft(unit[S,List[A]](List()))((g,a) => a.map2(g)((x,y) => x::y))
  
  /**
   * 아래의 sequenceViaFoldLeft02 는 foldLeft를 이용하되 reverse를 하지 않고 
   * tail recursive하게 처리 된다. 
   */
  def sequenceViaFoldLeft02[S,A](xs:List[State[S,A]]):State[S,List[A]] =
    xs.foldLeft[State[S,List[A]] => State[S,List[A]]]( (y:State[S,List[A]]) => y ) {
     (g,a) => x => g(a.map2(x)((a1,x1) => a1::x1)) 
    } (unit(Nil:List[A]))
 
  /**
   * type을 두어 좀더 간단하게 표현 
   */
  def sequence[S,A](xs: List[State[S,A]]): State[S,List[A]] = {
    type StateFn = State[S,List[A]] => State[S,List[A]]
    xs.foldLeft[StateFn](s => s)((g,a) => s => g(a.map2(s)((a1,b1) => a1::b1) ))(unit(List():List[A]))
  }
    
  //######### machine candy example #########
  def get[S]:State[S,S] = State(s => (s,s))
  def set[S](s:S):State[S,Unit] = State( _ => ((),s))
  
  def modify[S](f: S => S): State[S,Unit] = 
    get.flatMap(s => set(f(s)))
    
  def modifyFor[S](f: S => S):State[S,Unit] = for {
    s <- get  
    s2 <- set(f(s))
  }yield()
}

/**
 * Input 에 따른 Machine 의 상태. Machine의 상태에 따른 candies,coin
 * type Rand = Machine => (Machine,(Int,Int))
 * map,flatMap(f:function) 이 실행시 f function은 State의 run실행시 반영됨.
 * 
 */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked:Boolean,candies:Int,coins:Int)

object CandySlotMachine {
  def update = (i:Input) => (s:Machine) => 
    (i,s) match {
      case (_, Machine(_,0,_)) => s //candy가 없으면 아무일도 안함.
      case (Coin, Machine(false, _, _)) => s //풀린 자판기에 동전을 넣으면 아무일도 안함.
      case (Turn, Machine(true, _, _)) => s // 잠겨있으면 아무일도 안함.
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false,candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => 
        Machine(true,candy - 1, coin)
    }
  
  def simulatedMachine(input: List[Input]): State[Machine,(Int,Int)] = 
    sequence(input map(modify[Machine] _ compose update)).flatMap(n => 
      get.map(s => (s.candies,s.coins)))
  
  def simulatedMachineFor(inputs:List[Input]): State[Machine,(Int,Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield(s.coins,s.candies)
  
}

/**
 * Machine = S (RNG)
 * 
 */
object StateDriver extends App {
  
  import basic.state.CandySlotMachine._
  
  val xs01 = List(Coin,Turn)
  val rs01 = simulatedMachine(xs01) 
  println(rs01.run(Machine(true,10,1)))
  
  
  /*
  import basic.state.Mock._
  import basic.state.Mock
  
  val state01 = State[Mock,Int](intExplict)
  val state02 = State[Mock,Int](nonNegativeIntExplict)
  
  val xs = List(state01,state02)
  println(xs)
  
  val simpleMock = SimpleMock(Int.MaxValue)
  val sts = sequence(xs)
  println(sts.run(simpleMock))
 */ 
  
  
}