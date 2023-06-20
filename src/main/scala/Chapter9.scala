import scala.util.matching.Regex

object Chapter9 extends App:
	trait Parsers[ParseError, Parser[+_]]:
		def string(s: String): Parser[String]

		def succeed[A](a: A): Parser[A] =
			string("").map(_ => a)

		def char(c: Char): Parser[Char] =
			string(c.toString).map(_.charAt(0))

		def regex(r: Regex): Parser[String]

		def wrap[A](a: => Parser[A]): Parser[A]

		extension [A](self: Parser[A])
			def run(input: String): Either[ParseError, A]

			def flatMap[B](f: A => Parser[B]): Parser[B]

			def map[B](f: A => B): Parser[B]

			def mapViaFlatMap[B](f: A => B): Parser[B] =
				self.flatMap(a => succeed(f(a)))

			def mapTwo[B, C](that: => Parser[B])(f: (A, B) => C): Parser[C] =
				(self ** that).map((a, b) => f(a, b))

			def mapTwoViaFlatMap[B, C](that: => Parser[B])(f: (A, B) => C): Parser[C] =
				// self.flatMap(a => that.map(b => f(a, b)))
				for
					a <- self
					b <- that
				yield
					f(a, b)

			@annotation.targetName("or")
			def |(that: => Parser[A]): Parser[A]

			@annotation.targetName("product")
			def **[B](that: => Parser[B]): Parser[(A, B)] =
				// self.flatMap(a => that.map(b => (a, b)))
				for
					a <- self
					b <- that
				yield
					(a, b)

			def listOfN(n: Int): Parser[List[A]] =
				if n <= 0 then
					succeed(Nil)
				else
					self.mapTwo(self.listOfN(n - 1))(_ :: _)

			def zeroOrMany: Parser[List[A]] =
				self.oneOrMany | succeed(Nil)

			def oneOrMany: Parser[List[A]] =
				self.mapTwo(self.zeroOrMany)(_ :: _)

			def slice(that: Parser[A]): Parser[String]
