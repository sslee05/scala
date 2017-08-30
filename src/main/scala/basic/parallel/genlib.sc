package basic.parallel

/*
1.lib를 설계시 무었을 하고자 한는지 => 자료형식 도출
2.시작은 항상 간단한 예제로 시작하라.
3.결과를 담는 자료형식(container)와 이를 추출하는 함수가 필요함을 알 수 있다.
*/
object genlib {
  def sum(ints: IndexedSeq[Int]): Int =
    if(ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r) // left와 right가 병렬처리 된다면 이 결과를 담는 자료형식 container가 필요하다.
    }                                             //> sum: (ints: IndexedSeq[Int])Int
  
  trait Par[+A] {
    
  }
  
  object Par {
    //1.결과를 담는 자료형식을 반환 하는 함수 도출
    def unit[A](a: A): Par[A] = ???
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  
    //2.자료에서 결과값을 추출하는 함수 도출
    def get[A](p: Par[A]): A = ???
    
    //get 을 사용하지 않으려면
    def map2[A,B,C](pa: Par[A],pb: Par[B])(f: (A,B) => C): Par[C] = ???
  }
  
  def sum02(ints: IndexedSeq[Int]): Int = {
    if(ints.size <= 1) ints.headOption getOrElse 0
    else {
      val (l,r) = ints splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum02(l))
      val sumR: Par[Int] = Par.unit(sum02(r))
      
      Par.get(sumL) + Par.get(sumR)
      //치환 모델을 적용하면 unit이 비동기로 실행되더라도 get의 완료까지 기다려야 하므로 비동기로 되지 안는 부수효과가 발생된다.
      Par.get(Par.unit(sum02(l))) + Par.get(Par.unit(sum02(r)))
      // 따라서 get은 비동기 계산이 끝나지 않고도 조합 가능해야 한다.
    }
  }                                               //> sum02: (ints: IndexedSeq[Int])Int
  
  def sum03(ints: IndexedSeq[Int]): Par[Int] =
    if(ints.size <= 1) Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length / 2)
      Par.map2(sum03(l),sum03(r))(_ + _) // unit를 호출  하지 않는다.
    }                                             //> sum03: (ints: IndexedSeq[Int])basic.parallel.genlib.Par[Int]
  //Par.map2(sum03(l),sum03(r))(_ + _) 의 scala의 parameter는 엄격하기 때문에
  //map2 의 sum03(l) 이 분할 정복이 끝난 이후에 sum03(r) 의 분할 정복이 시작된다.
  
  //그럼 map2를 call-by-name으로 해야 하나?
  //항상 병렬로 하지 않는 것이 더 좋아 보인다.
  //따라서 map2는 call-by-value 로 두고 병렬여부의 선택을 client에게 주자.
  //fork를 선언하자.
  
  def fork[A](a: => Par[A]): Par[A] = ???         //> fork: [A](a: => basic.parallel.genlib.Par[A])basic.parallel.genlib.Par[A]
  
  def sum04[A](ints: IndexedSeq[Int]): Par[Int] =
    if(ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length / 2)
      Par.map2(fork(sum04(l)),fork(sum04(r)))(_ + _)
    }                                             //> sum04: [A](ints: IndexedSeq[Int])basic.parallel.genlib.Par[Int]
  
  //fork 와
  val xs = IndexedSeq(1,2,3,4,5,6,7,8,9,10)       //> xs  : IndexedSeq[Int] = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(sum(xs))                                //> 55
  
  
}