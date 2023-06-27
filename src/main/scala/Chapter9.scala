import Chapter9.Result.Failure

object Chapter9 extends App:
	opaque type Parser[A] = State => Result[A]

	case class State(input: String, pos: Int):
		lazy val line: Int =
			input.slice(0, pos + 1).count(_ == '\n') + 1

		lazy val col: Int =
			input.slice(0, pos + 1).lastIndexOf('\n') match
				case -1        => pos + 1
				case lineStart => pos - lineStart

		def toParse: String =
			input.drop(pos)

		def consumed(n: Int): State =
			copy(pos = pos + n)

		def toError: Errors =
			val msg = s"Could not parse '${toParse.headOption.getOrElse("eof")}' at line $line, col $col"

			Errors(List((msg, this)))

	case class Errors(stack: List[(String, State)]):
		def push(msg: String, state: State): Errors =
			copy(stack = (msg, state) :: stack)

	enum Result[+A]:
		case Success(get: A, state: State)
		case Failure(get: Errors)

		def extract: Either[Errors, A] =
			this match
				case Success(a, _) => Right(a)
				case Failure(e) => Left(e)

		override def toString: String =
			this match
				case Success(_, s) =>
					s"${Console.GREEN}✔ Parsing successful: consumed ${s.pos} characters${Console.RESET}"
				case Failure(e) =>
					s"${Console.RED}✘ Parsing failed:\n  - ${e.stack.map(_._1).mkString("\n  - ")}${Console.RESET}"

	object Parser:
		def succeed[A](a: A): Parser[A] =
			(s: State) => Result.Success(a, s)

		def satisfy(s: State)(f: String => Option[String]): Result[String] =
			f(s.toParse) match
				case Some(a) =>
					Result.Success(a, s.consumed(a.length))
				case None =>
					Result.Failure(s.toError)

		def string(str: String): Parser[String] =
			(s: State) => satisfy(s):
				i => Some(str).filter(i.startsWith)

		def char(c: Char): Parser[Char] =
			string(c.toString).map(_.charAt(0))

		import scala.util.matching.Regex

		def regex(r: Regex): Parser[String] =
			(s: State) => satisfy(s):
				i => r.findPrefixOf(i)

		extension [A](self: Parser[A])
			def run(input: String): Result[A] =
				self(State(input, 0))

			def flatMap[B](f: A => Parser[B]): Parser[B] =
				(s: State) => self(s) match
					case Result.Success(a, s) => f(a)(s)
					case Result.Failure(e)    => Result.Failure(e)

			def map[B](f: A => B): Parser[B] =
				self.flatMap(a => succeed(f(a)))

			def mapTwo[B, C](that: => Parser[B])(f: (A, B) => C): Parser[C] =
				for
					a <- self
					b <- that
				yield
					f(a, b)

			def zeroOrMore: Parser[List[A]] =
				self.oneOrMore | succeed(Nil)

			def oneOrMore: Parser[List[A]] =
				self.mapTwo(self.zeroOrMore)(_ :: _)

			def times(n: Int): Parser[List[A]] =
				if n <= 0 then
					succeed(Nil)
				else
					self.mapTwo(self.times(n - 1))(_ :: _)

			def separator[B](sep: Parser[B]): Parser[List[A]] =
				self.mapTwo((sep ~ self).zeroOrMore)(_ :: _)

			def slice: Parser[String] =
				(s: State) => self(s) match
					case Result.Success(_, ns) => Result.Success(ns.input.substring(s.pos, ns.pos), ns)
					case Result.Failure(e) => Result.Failure(e)

			def label(msg: String): Parser[A] =
				(s: State) => self(s) match
					case Result.Failure(e) => Result.Failure(e.push(msg, s))
					case success => success

			@annotation.targetName("or")
			def |[B >: A](that: => Parser[B]): Parser[B] =
				(s: State) => self(s) match
					case _: Result.Failure[A] => that(s)
					case r: Result.Success[A] => r

			@annotation.targetName("product")
			def **[B](that: => Parser[B]): Parser[(A, B)] =
				self.mapTwo(that)((_, _))

			@annotation.targetName("ignore")
			def ~[B](that: => Parser[B]): Parser[B] =
				self.mapTwo(that)((_, b) => b)

		given fromStringToParser: Conversion[String, Parser[String]] with
			def apply(s: String): Parser[String] = string(s)

	enum JSON:
		case JNull
		case JNumber(get: Double)
		case JString(get: String)
		case JBool(get: Boolean)
		case JArray(get: IndexedSeq[JSON])
		case JObject(get: Map[String, JSON])

	object JSON:
		import Parser._

		val whitespace: Parser[String] =
			regex("[\\u0020\\u000A\\u000D\\u0009]*".r).slice

		val jnull: Parser[JSON] =
			string("null").map(_ => JNull)

		val jbool: Parser[JBool] =
			(string("true") | string("false")).map(b => JBool(b.toBoolean))

		val jnumber: Parser[JNumber] =
			regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).map(n => JNumber(n.toDouble))

		val jstring: Parser[JString] =
			for
				_ <- whitespace ~ char('"')
				s <- regex("[^\"]*".r).slice
				_ <- char('"')
			yield
				JString(s)

		val jarray: Parser[JArray] =
			for
				_ <- whitespace ~ char('[')
				a <- whitespace ~ json.separator(char(','))
				_ <- whitespace ~ char(']')
			yield
				JArray(a.toIndexedSeq)

		val keyval: Parser[(String, JSON)] =
			for
				k <- whitespace ~ jstring
				_ <- whitespace ~ char(':')
				v <- whitespace ~ json
			yield
				(k.get, v)

		val jobject: Parser[JObject] =
			for
				_ <- whitespace ~ char('{')
				o <- whitespace ~ keyval.separator(char(','))
				_ <- whitespace ~ char('}')
			yield
				JObject(o.toMap)

		def json: Parser[JSON] =
			jnull | jnumber | jstring | jbool | jarray | jobject
