package basic.parser

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self => // Parsers trait를 칭하게 됨.
  
  // 단순예제인 char 'a' => Parser[a]
  //string 함수 와  map 함수를 이용하여 구현하라.
  def char(c: Char): Parser[Char] = 
    string(c.toString).map(x => x.charAt(0))
  
  //실행자 
  def run[A](p: Parser[A])(input: String): Either[ParseError,A] = ???
  
  //문자열을 인식해야 한다면 ?
  implicit def string(s: String): Parser[String] = ???
  
  //"abc" or "def" 중 하나를 인식해야 한다면 ?
  def orString(s1: String, s2:String): Parser[String] = ???
  
  //orString 을 좀더 일반화 
  //2번째 인수를 laziness 하게 두었다 1번째 인자가 성공이면 2번째 인자는 평가 할 필요가 없어진다.
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = ???
  
  //implicit 를 이용해 ParserOps에게 위임하기 위함. "abc" | "eff" 형식으로 사용 가능하게 함.
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):ParserOps[String] = ParserOps(f(a))
  
  implicit def regex(r: Regex): Parser[String] = ???
  
  
  //0 개이상의 문자 'a'를 인식해서 출현 갯수를 돌려주는 parser
  //many를 map2 와 or 와 succeed 로 구현 하라.
  //재귀 호출이므로 map2 의 두번째 인수 그리고 many의 인수 는 strict하면 안된다.
  def many[A](p: => Parser[A]): Parser[List[A]] = 
    map2(p, many(p))(_::_) or succeed(List())
  
  //map2 와 succeed를 이용하여 구현하라.
  def listOfN[A](n:Int, p:Parser[A]): Parser[List[A]] =
    if(n <= 0) succeed(List())
    else map2(p,listOfN(n-1,p))(_ :: _)
  
  //manay는 return type이 잘못 Parser[Int] 이어야 한다.
  //Parser[Int]는 일반적이지 못하다 => map를 생각해내자  
  def map[A,B](par: Parser[A])(f: A => B): Parser[B] = 
    flatMap(par)(x => succeed(f(x)))
  
  //항등원 
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  
  //입력문자에서 자신이 조사한 부분만 넘겨주는 조합
  def slice[A](p: Parser[A]): Parser[String] = ???
  
  //한 parser를 실행하고 그 것이 성공하면 다음 parser를 실해하는 조합기를 만들어라.
  def product[A,B](p1: Parser[A],p2: Parser[B]): Parser[(A,B)] = 
    map2(p1,p2)((a,b) => (a,b))
  
  //product 와 map2를 이용해서 flatMap를 구현하라.
  def flatMap[A,B](par: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  
  //many를 보면 map2의 2번째 인수가 laziness 해야 한다.
  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = for {
    a <- p1
    b <- p2
  }yield(f(a,b))
  
  //many1를 map2와 many를 이용하여 구현하라.
  def many1[A](par: Parser[A]): Parser[List[A]] = 
    map2(par,many(par))(_::_)
  
  
  //##########################################################################################
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = ???
    def or[B >: A](p2: Parser[B]): Parser[B] = ???
    
    
    //0 개이상의 문자 'a'를 인식해서 출현 갯수를 돌려주는 parser
    def many: Parser[List[A]] = self.many(p)
    
    
    //manay는 return type이 잘못 Parser[Int] 이어야 한다.
    //Parser[Int]는 일반적이지 못하다 => map를 생각해내자 
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    
    //입력문자에서 자신이 조사한 부분만 넘겨주는 조합
    def slice:Parser[String] = self.slice(p)
    
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    
    // ** trait prouct로 위임 
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    
    // product trait product로 위임.
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p,p2)
    
    
    
  }
  
}


object ParsersDriver extends App {
  
  import basic.parser._
  val c = 'a'
  /*
  //char
  run(char(c))(c.toString) == Right(c)
  
  //string
  run(string("abc"))("abc") == Right("abc")
  
  //string orString
  run(orString(string("abc"),string("def")))("abcreqee") == Right("abc")
  
  //or
  run(or(string("abc"),string("def")))("abcfynhg") == Right("abc")
  
  //listOfN
  run(listOfN(3,"ab" | "cad"))("ababcad") == Right(List("ab","ab","cad"))
  run(listOfN(3,"ab" | "cad"))("cadabab") == Right(List("cad","ab","ab"))
  run(listOfN(3,"ab" | "cad"))("ababab") == Right(List("ab","ab","ab"))
  
  //map
  run(map(many(char('a')))(_.size))("abccde") == Right(2)
  */
}

