package basic.parallel


object genlib02 {

    //foldLeft는 병렬적이 않다 => 예제를 생각 해보자.
    val xs = List(1,2,3,4,5)                      //> xs  : List[Int] = List(1, 2, 3, 4, 5)
    val rs = xs.foldLeft(0)((x,y) => x + y)       //> rs  : Int = 15
    println(rs)                                   //> 15
    
    //1.간단한 예제를 만들어 본다.
    def sum(xs: IndexedSeq[Int]): Int = {
      if(xs.size  <= 1) xs.headOption getOrElse 0
      else {
        val (l,r) = xs.splitAt(xs.length / 2)
        sum(l) + sum(r)
      }
    }                                             //> sum: (xs: IndexedSeq[Int])Int
    
    //분할 정복으로 병렬적으로 처리 할 수 있을 듯 하다.
    //위의 sum(l) + sum(r) 를 보면 병렬처리결과를 담을
    //자료구조와 그 자료 구조에서 값을 뽑을 수 있는 함수가 필요하다.
    
    
    trait Par[+A]
    
    object Par {
      //병렬처리할 대상을 지연으로 받아 평가할 수 있는 계산을 돌려 준다.
      def unit[A](a: => A): Par[A] = ???
      
      //병렬 계산에서의 결과 값을 추출 한다.
      def get[A](par: Par[A]): A = ???
      
      //해결방안01
      def map2[A,B,C](parA: Par[A], parB: Par[B])(f: (A,B) => C) : Par[C] = ???
      
      def fork[A](a: => Par[A]): Par[A] = ???
    }
    
    def sum02(xs:IndexedSeq[Int]): Int = {
      if(xs.size <= 1) xs.headOption getOrElse 0
      else {
        val (l,r) = xs.splitAt(xs.length / 2)
        val sumL: Par[Int] = Par.unit(sum(l))
        val sumR: Par[Int] = Par.unit(sum(r))
        
        Par.get(sumL) + Par.get(sumR)
      }
    }                                             //> sum02: (xs: IndexedSeq[Int])Int
    
    //문제점
    //unit 을 즉시 지연 평가후 get에서 호출시 평가 된다면
    //left get이 끝날때 가지 기다렸다가 right의 get(unit) 처리가 되니 unit은 즉시 평가 되어야 한다.
    //unit이 즉시 처리한 한다 고 해도 값을 얻는 get이 즉시 처리 한다고 하면 어차피 right 쪽이 block된다.
    
    //해결방안01
    //get 을 없에고 sum function return type을 Par로 수정해보기
    def sum03(xs: IndexedSeq[Int]): Par[Int] = {
      if(xs.size <= 1) Par.unit(xs.headOption getOrElse 0)
      else {
        val (l,r) = xs.splitAt(xs.length / 2)
        Par.map2(sum03(l),sum03(r))(_ + _)
      }
    }                                             //> sum03: (xs: IndexedSeq[Int])basic.parallel.genlib02.Par[Int]
    
    //문제점
    //map2 역시 left 인자가 분할이 끝나고 right 인자의 분할 처리가 시작된다. 즉 병렬 처리 않됨.
    
    //해결방안02
    //map2를 strictiness 하게 하된 평가를 지연하려면 서술을 기술하는 방법(foldRightViaLeft처럼)
    //또 한가지는 client에서 판단하게 명시적 분기를 하는 것이다.
    //첫째 엄격한 서술을 저장하는 것은 오히려 저장공간이 더 커질 수 있다.
    //따라서 둘째 방법으로 또한 2째 방법시 상황에 따라 client에게 선택적으로 지연처리 혹은 엄격처리 할 수 있게 선택을 줄 수 있다.
    def sum04(xs: IndexedSeq[Int]): Par[Int] = {
      if(xs.size <= 1) Par.unit(xs.headOption getOrElse 0)
      else {
        val (l,r) = xs.splitAt(xs.length /2 )
        Par.map2(Par.fork(sum04(l)), Par.fork(sum04(r)))(_ + _)
      }
    }                                             //> sum04: (xs: IndexedSeq[Int])basic.parallel.genlib02.Par[Int]
   
   //fork의 명시적 분기를 두어 client에게 판단하게 함으로써 병렬처리의 결과를 조합하는 부분과(map2)
   //동기로 처리할지 비동기로 할지의 관심사를 분리할 수 있게 되었다.
   
   //판단
   //fork는 개별 논리적 thead 처리단위 이다. fork에서 논리적 thread를 처리하게 할지
   //아니면 get 같은 곳서의 평가를 강제하고 fork 에서는 평가를 미룰지
   
   //해결방안03
   //fork 에서 한다면 병렬처리의 자원 및 api 에 dependency가 생긴다.
   // get 같은 것에서 평가하게 함으로써 fork는 Par 인수를 받고 그 인수에동시적 평가가 필요하다는 점만 명시하는 역할 만 한다고 생각.
   
   
   //Par는 get같은 것에 의해 평가 될때 병렬될 병렬 계산에 관한 서술
   //fork는 fork는 병렬될 계산 서술(Par) 인수를 받고 그 인수에동시적 평가가 필요하다는 점만 명시
    
    val rs01 = IndexedSeq(1,2,3,4,5)              //> rs01  : IndexedSeq[Int] = Vector(1, 2, 3, 4, 5)
    println(sum(rs01))                            //> 15
}