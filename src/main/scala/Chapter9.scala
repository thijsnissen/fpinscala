object Chapter9 extends App:
	opaque type Parser[A] = State => Result[A]

	case class State(input: String, pos: Int):
		def consumed(n: Int): State =
			copy(pos = pos + n)

	case class Errors(stack: List[(String, State)]):
		def push(msg: String, state: State): Errors =
			copy(stack = (msg, state) :: stack)

	enum Result[+A]:
		case Success(get: A, state: State)
		case Failure(get: Errors)

		def extract: Either[Errors, A] =
			this match
				case Success(a, _) => Right(a)
				case Failure(e)    => Left(e)

	object Parser:
		def succeed[A](a: A): Parser[A] =
			(s: State) => Result.Success(a, s)

		def char(c: Char): Parser[Char] =
			string(c.toString).map(_.charAt(0))

		def string(str: String): Parser[String] =
			(s: State) =>
				val toParse = s.input.drop(s.pos)

				if toParse.startsWith(str) then
					Result.Success(str, s.consumed(str.length))
				else
					Result.Failure(Errors(List((s"Could not parse '$str', found '${toParse.headOption.getOrElse("eof")}'", s))))

		import scala.util.matching.Regex

		def regex(r: Regex): Parser[String] =
			(s: State) => r.findPrefixOf(s.input.drop(s.pos)) match
				case Some(a) =>
					Result.Success(a, s.consumed(a.length))
				case None =>
					Result.Failure(Errors(List((s"Could not parse regex '$r'", s))))

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

			def slice: Parser[String] =
				(s: State) => self(s) match
					case Result.Success(_, s) =>
						Result.Success(s.input.substring(0, s.pos), s)
					case Result.Failure(e) => Result.Failure(e)

			@annotation.targetName("or")
			def |[B >: A](that: => Parser[B]): Parser[B] =
				(s: State) => self(s) match
					case _: Result.Failure[A] => that(s)
					case r: Result.Success[A] => r

			@annotation.targetName("product")
			def **[B](that: => Parser[B]): Parser[(A, B)] =
				self.mapTwo(that)((_, _))

		given fromStringToParser: Conversion[String, Parser[String]] with
			def apply(s: String): Parser[String] = Parser.string(s)

	enum JSON:
		case JNull
		case JNumber(get: Double)
		case JString(get: String)
		case JBool(get: Boolean)
		case JArray(get: IndexedSeq[JSON])
		case JObject(get: Map[String, JSON])

	object JSON:
		import Parser._

		val whitespace: Parser[Char] =
			char(' ') | char('\t') | char('\n') | char('\r')

		val jnull: Parser[JSON] =
			Parser.string("null").map(_ => JNull)

		val jnumber: Parser[JNumber] =
			regex("[0-9]".r).oneOrMore.slice.map(n => JNumber(n.toDouble))

		val jstring: Parser[JString] =
			for
				_ <- char('"')
				s <- (regex("[a-zA-Z]".r) | whitespace.slice).zeroOrMore.slice
				_ <- char('"')
			yield
				JString(s)

		val jbool: Parser[JBool] =
			(string("true") | string("false")).map(b => JBool(b.toBoolean))

		val jliteral: Parser[JSON] =
			jnull | jnumber | jstring | jbool

		val jkeyval: Parser[(String, JSON)] =
			for
				k <- jstring
				_ <- whitespace
				_ <- char(':')
				_ <- whitespace
				v <- jliteral | jarray | jobject
				_ <- char(',')
				_ <- whitespace
			yield
				(k.toString, v)

		val jval: Parser[JSON] =
			for
				v <- jliteral | jarray | jobject
				_ <- char(',')
				_ <- whitespace
			yield
				v

		val jarray: Parser[JArray] =
			for
				_ <- Parser.char('[')
				_ <- whitespace
				v <- jval.oneOrMore
				_ <- whitespace
				_ <- Parser.char(']')
			yield
				JArray(v.toIndexedSeq)

		val jobject: Parser[JObject] =
			for
				_  <- whitespace
				_  <- Parser.char('{')
				_  <- whitespace
				kv <- jkeyval.oneOrMore
				_  <- whitespace
				_  <- Parser.char('}')
			yield
				JObject(kv.toMap)

		def JSONParser: Parser[JSON] =
			jliteral | jarray | jobject
