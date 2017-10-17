package exercise.state.repeat01

import exercise.state.repeat01.State._

//ex-01) 상태를 가지는 State case class 를 작성하라.
case class State[S,+A](run: S => (A,S)) {
  
  //ex-04)flatMap를 구현하라.
  def flatMap[B](f: A => State[S,B]): State[S,B] = State(s => run(s) match {
    case (a,s1) => f(a) run s1
  })
  
  //ex-04) map를 구현하라.
  def map[B](f: A => B): State[S,B] = flatMap(a => unit(f(a)))
    
  //ex-05)map2 함수를 구현하라.
  def map2[B,C](mb: State[S,B])(f: (A,B) => C): State[S,C] = 
    flatMap(a => mb map(b => f(a,b)))
  
}

//ex-02)State object 를 만들어라.
object State {
  
  //ex-03)항등원인 unit 함수를 구현하라.
  def unit[S,A](a: A): State[S,A] = State(s => (a,s))
  
  //ex-04)명시적으로 loop를 실행해서 sequence를 구현하라.
  def sequenceExplicit[S,A](xs: List[State[S,A]]): State[S,List[A]] = {
    
    def go(s: S, actions: List[State[S,A]],rs: List[A]): (List[A],S) = actions match {
      case Nil => (rs.reverse,s)
      case h::t => h.run(s) match { case (a,s1) => go(s1,t,a::rs) }
    }
    
    State(s => go(s,xs,List[A]()))
  }
  
  //ex-06)sequence를 foldRight로 구현하라.
  def sequenceViaFoldRight[S,A](xs: List[State[S,A]]): State[S,List[A]] = 
    xs.foldRight[State[S,List[A]]](unit(List[A]()))((a,b) => a.map2(b)((a1,b1) => a1::b1))
    
    
  //ex-07)sequence를 foldLeft로 구현하라.
  //아래의 foldLeft는 reverse를 하므로 foldRight보다 속도가 느릴것 같지만
  //실제로 foldRight보다 빠르다. 이유는 foldRight는 Nil까지 갔다가 stack frame
  //이 다시 원상태로 와야 하기 때문에 foldRight도 2 번 종단을 하기 때문이다.
  def sequenceViaFoldLeft[S,A](xs: List[State[S,A]]): State[S,List[A]] = 
    xs.reverse.foldLeft[State[S,List[A]]](unit(List[A]()))((b,a) => a.map2(b)((a1,b1) => a1::b1))
    
  //ex-08)sequence를 reverse 없이 foldLeft를 이용해서 구현하라.
  def sequence[S,A](xs: List[State[S,A]]): State[S,List[A]] = {
    type StateFn = State[S,List[A]] => State[S,List[A]]
    xs.foldLeft[StateFn](s => s)((g,a) => b => g(a.map2(b)((a1,b1) => a1::b1)))(unit(Nil))
  }
  
  //ex-09) get 를 구현하라.
  def get[S]: State[S,S] = State(s => (s,s))
  
  //ex-10) set 를 구현하라.
  def set[S](s: S): State[S,Unit] = State(_ => ((),s))
  
  //ex-11) 다음을 구현하라.
  def modify[S](f: S => S): State[S,Unit] = 
    get flatMap(a => set(f(a)))
  
  //ex-12) modify를 for 표현식으로 구현하라.
  def modifyByFor[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  }yield()
    
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked:Boolean,candies:Int,coins:Int)

object CandySlotMachine {
  def update = (i: Input) => (m: Machine) => (i,m) match {
    case (_, Machine(_,0,_)) => m //candy가 없으면 아무일도 안함.
    case (Coin, Machine(false, _, _)) => m //풀린 자판기에 동전을 넣으면 아무일도 안함.
    case (Turn, Machine(true, _, _)) => m // 잠겨있으면 아무일도 안함.
    case (Coin, Machine(true, candy, coin)) =>
      Machine(false,candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) => 
      Machine(true,candy - 1, coin)
  }
  
  
  //def test: Input => State[Machine,Unit] = modify[Machine] _ compose update
  //val rs:List[State[Machine,Unit]] = inputs map(modify[Machine] _ compose update)
  //val rs:State[Machine,List[Unit]] = sequence(inputs map(modify[Machine] _ compose update))
  
  //ex-13) 다음을 구현하라.
  def simulatedMachine(inputs: List[Input]): State[Machine,(Int,Int)] = 
    sequence(inputs map(modify[Machine] _ compose update)).flatMap(ma => get.map(m => (m.candies,m.coins)))
    
  //ex-14) simulatedMachine을 for 표현식으로 구현하라.
  def simulatedMachineByFor(inputs: List[Input]): State[Machine,(Int,Int)] = for {
    _ <- sequence(inputs map(modify[Machine] _ compose update))
    s <- get
  } yield(s.candies,s.coins)
  
}

object StateDriver01 extends App {
  
  import exercise.state.repeat01.CandySlotMachine._
//  val xs01 = List(Coin,Turn)
//  val rs01 = simulatedMachine(xs01) 
//  println(rs01.run(Machine(true,10,1)))
  
  val xs02 = List(State((s:Int) => (s+1,s+1)),State((s:Int) => (s * 2,s +1 )))
  println(sequenceExplicit(xs02).run(0))
  println(sequence(xs02).run(0))
  
}
