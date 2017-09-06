package basic.parser

object ing {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  trait Parser[A]
  trait PaserError
  
  trait Parsers[ParserError,Parser[+_]] {
    //간단한 예제 'a' => Parser[a]
    def char(c: Char): Parser[Char] = ???
    
    def string(s:String): Parser[String] = ???
    
    //"abc" or "def" 를 인식해야 한다면?
    def orString(s1:String,s2:String): Parser[String] = ???
    
    //orString 를 좀더 일반화 한다면?
    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = ???
  
    //실행자
    def run[A](p: Parser[A])(input: String): Either[PaserError,A] = ???
  }
  
  
  val c = 'a'                                     //> c  : Char = a
  //run(char(c))(c.toString) == Right(c)
  
  //run(or(string("abc"),string("def"))("abctwtfgfds") == Right("abc")
  
  
}