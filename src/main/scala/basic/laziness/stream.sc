package basic.laziness

import basic.laziness.Stream._

object stream {
  //takeWhileOrigin test
  val xs = Stream(1,4,5,6,3,2)                    //> xs  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$Lambda$9/932
                                                  //| 607259@7a5d012c,basic.laziness.Stream$$$Lambda$10/1740000325@6b2fad11)
  println(xs.takeWhileOrigin(x => x > 3).toList)  //> List(4, 5, 6)
  
  //exist test
  println(xs.exist(x => x > 3))                   //> true
  
  //forAll test
  println(xs.exist(x => x < 1))                   //> false
  
  //takeWhile test
  println(xs.takeWhile(x => x > 3).toList)        //> List(4, 5, 6)
  
  //head Option test
  println(xs.headOption)                          //> Some(1)
  
  //test map
  println(xs.map(x => x * 2).toList)              //> List(2, 8, 10, 12, 6, 4)
  
  //append test
  val ys = Stream(8,9,10)                         //> ys  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$Lambda$9/932
                                                  //| 607259@34b7bfc0,basic.laziness.Stream$$$Lambda$10/1740000325@366e2eef)
  println(xs append ys toList)                    //> List(1, 4, 5, 6, 3, 2, 8, 9, 10)
  
  //test flatMap
  val xs01 = Stream(Stream(1,2),Stream(3,4),Stream(5,6))
                                                  //> xs01  : basic.laziness.Stream[basic.laziness.Stream[Int]] = Cons(basic.lazin
                                                  //| ess.Stream$$$Lambda$9/932607259@7225790e,basic.laziness.Stream$$$Lambda$10/1
                                                  //| 740000325@54a097cc)
  val rs01 = xs01 flatMap(x => x map(x1 => x1 + 2))
                                                  //> rs01  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$Lambda$9/9
                                                  //| 32607259@52af6cff,basic.laziness.Stream$$$Lambda$10/1740000325@735b478)
  println(rs01.toList)                            //> List(3, 4, 5, 6, 7, 8)
  
  //test filter
  println(xs.filter(x => x > 2).toList)           //> List(4, 5, 6, 3)
  
  //test constant
  val conVal = constant(1)                        //> conVal  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$Lambda$9
                                                  //| /932607259@13c78c0b,basic.laziness.Stream$$$Lambda$10/1740000325@12843fce)
  println(conVal.headOption)                      //> Some(1)
  
  //test from
  val fromVal = from(1)                           //> fromVal  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$Lambda$
                                                  //| 9/932607259@6a2bcfcb,basic.laziness.Stream$$$Lambda$10/1740000325@4de8b406)
  println(fromVal.headOption)                     //> Some(1)
  
  //test fibo
  val fiboVal = fibs                              //> fiboVal  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$Lambda$
                                                  //| 9/932607259@5ae9a829,basic.laziness.Stream$$$Lambda$10/1740000325@6d8a00e3)
  println(fiboVal.takeOrigin(10).toList)          //> List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  
  //test unfold
  val unfoldVal = unfold(Stream(1,2,3,4,5)) {
    case Cons(h,t) => Some((h(),t()))
    case Empty => None
  }                                               //> unfoldVal  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$Lamb
                                                  //| da$9/932607259@5e5792a0,basic.laziness.Stream$$$Lambda$10/1740000325@266532
                                                  //| 22)
  println(unfoldVal.toList)                       //> List(1, 2, 3, 4, 5)
  
  //test fibs
  val fibsViaFoldVal = fibsViaUnfold              //> fibsViaFoldVal  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$
                                                  //| $Lambda$9/932607259@ae45eb6,basic.laziness.Stream$$$Lambda$10/1740000325@59
                                                  //| f99ea)
  println(fibsViaFoldVal takeOrigin 10 toList)    //> List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  
  //test fromViaUnfold
  val fromViaUnfoldVal = fromViaUnfold(1)         //> fromViaUnfoldVal  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream
                                                  //| $$$Lambda$9/932607259@6f7fd0e6,basic.laziness.Stream$$$Lambda$10/1740000325
                                                  //| @47c62251)
  println(fromViaUnfoldVal takeOrigin 10 toList)  //> List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  
  //test constantViaUnfold
  val constantViaUnfoldVal = constantViaUnfold(3) //> constantViaUnfoldVal  : basic.laziness.Stream[Int] = Cons(basic.laziness.St
                                                  //| ream$$$Lambda$9/932607259@66a3ffec,basic.laziness.Stream$$$Lambda$10/174000
                                                  //| 0325@77caeb3e)
  println(constantViaUnfoldVal takeOrigin 10 toList)
                                                  //> List(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
  
  //test mapViaUnfold
  val mapViaUnfoldVal = rs01.mapViaUnfold(x => x + 1)
                                                  //> mapViaUnfoldVal  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$
                                                  //| $$Lambda$9/932607259@3bfdc050,basic.laziness.Stream$$$Lambda$10/1740000325@
                                                  //| 1bce4f0a)
  println(mapViaUnfoldVal takeOrigin 10 toList)   //> List(4, 5, 6, 7, 8, 9)
  
  //test takeViaUnfold
  println(mapViaUnfoldVal take 10 toList)         //> List(4, 5, 6, 7, 8, 9)
  
  //test zipWithViaFold
  val zipXs = Stream(1,2,3,4,5)                   //> zipXs  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$Lambda$9
                                                  //| /932607259@5c3bd550,basic.laziness.Stream$$$Lambda$10/1740000325@91161c7)
  val zipYs = Stream(6,7,8)                       //> zipYs  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$Lambda$9
                                                  //| /932607259@604ed9f0,basic.laziness.Stream$$$Lambda$10/1740000325@6a4f787b)
  println(zipXs.zipWith(zipYs){(x,y) => x + y} toList)
                                                  //> List(7, 9, 11)
  
  //test zipAll
  println(zipXs zipAll zipYs toList)              //> List((Some(1),Some(6)), (Some(2),Some(7)), (Some(3),Some(8)), (Some(4),None
                                                  //| ), (Some(5),None))
  
  //test startWith
  val startWithXs = Stream(1,2,3,4,5)             //> startWithXs  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$La
                                                  //| mbda$9/932607259@7d9d1a19,basic.laziness.Stream$$$Lambda$10/1740000325@39c0
                                                  //| f4a)
  val startWithYx = Stream(1,2,3)                 //> startWithYx  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$La
                                                  //| mbda$9/932607259@1794d431,basic.laziness.Stream$$$Lambda$10/1740000325@42e2
                                                  //| 6948)
  println(startWithXs startsWith startWithYx)     //> true
  
  //test tail
  println(tails(startWithXs) flatMap(x => x map(x1 => x1)) toList)
                                                  //> List(1, 2, 3, 4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5)
  
  //test subSequence
  println(startWithXs subSequence startWithYx)    //> true
  
  //test scanRight
  // a + b에서 b 부분에서 t().foldRight(z)(f) 부분이 평가 됨.
  val scanRightXs = Stream(1,2,3)                 //> scanRightXs  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$La
                                                  //| mbda$9/932607259@146ba0ac,basic.laziness.Stream$$$Lambda$10/1740000325@4dfa
                                                  //| 3a9d)
  val rs = scanRightXs.scanRight(0)((a,b) => a + b)
                                                  //> rs  : basic.laziness.Stream[Int] = Cons(basic.laziness.Stream$$$Lambda$9/93
                                                  //| 2607259@598067a5,basic.laziness.Stream$$$Lambda$10/1740000325@3c0ecd4b)
  println(rs.toList)                              //> List(6, 5, 3, 0)
}