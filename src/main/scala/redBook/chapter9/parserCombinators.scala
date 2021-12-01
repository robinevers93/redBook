package redBook.chapter9

trait Parsers[ParseError, Parser[+_]] { self =>

  def char(c: Char): Parser[Char]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def orString(s1: String, s2: String): Parser[String]
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](parser: Parser[A]): ParserOps[A] = ParserOps[A](parser)
  implicit def asStringParser[A](a:A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  val c: Char = 'a'
  val s: String = "test"

  case class ParserOps[A](p: Parser[A]) {
    val c = "b"
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }

  //law 1
  run(char(c))(c.toString) == Right(c) //succeeds when input is 'a' and return 'a' as result
  run(string(s))(s) == Right(s)

  //law2
  run(or(string("abra"), string("cadabra")))("abra") == Right ("abra") //succeeds when input is either 'abra' or 'cadabra'
  run(or(string("abra"), string("cadabra")))("cadabra") == Right ("cadabra")
}

object test {
//  case class tempParser[A](a: A)
//
//  val x: Parsers[String, tempParser] = ???
//  val y: String = "y"
//  val z: String = "z"
//
//  import x._
//
//  y.or(z)

}
