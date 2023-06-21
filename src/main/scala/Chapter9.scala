object Chapter9 extends App:
	import scala.util.matching.Regex

	trait Parsers[Parser[+_]]:
		def string(s: String): Parser[String]

		def regex(r: Regex): Parser[String]

		def char(c: Char): Parser[Char] =
			string(c.toString).map(_.charAt(0))

		def succeed[A](a: A): Parser[A] =
			string("").map(_ => a)

		extension [A](self: Parser[A])
			def run(input: String): Either[ParseErrors, A]

			def flatMap[B](f: A => Parser[B]): Parser[B]

			def map[B](f: A => B): Parser[B] =
				self.flatMap(a => succeed(f(a)))

			def mapTwo[B, C](that: => Parser[B])(f: (A, B) => C): Parser[C] =
				// (self ** that).map((a, b) => f(a, b))
				// self.flatMap(a => that.map(b => f(a, b)))
				for
					a <- self
					b <- that
				yield
					f(a, b)

			def slice(that: Parser[A]): Parser[String]

			def label(msg: String): Parser[A]

			def scope(msg: String): Parser[A]

			def attempt: Parser[A]

			def listOfN(n: Int): Parser[List[A]] =
				if n <= 0 then
					succeed(Nil)
				else
					self.mapTwo(self.listOfN(n - 1))(_ :: _)

			def zeroOrMany: Parser[List[A]] =
				self.oneOrMany | succeed(Nil)

			def oneOrMany: Parser[List[A]] =
				self.mapTwo(self.zeroOrMany)(_ :: _)

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

	case class Location(input: String, offset: Int = 0):
		lazy val line: Int =
			input.slice(0, offset + 1).count(_ == '\n') + 1

		lazy val col: Int  =
			input.slice(0, offset + 1).lastIndexOf('\n') match
				case -1 => offset + 1
				case lineStart => offset - lineStart

		def toError(msg: String): ParseErrors =
			ParseErrors(List((this, msg)))

		def advanceBy(n: Int): Location =
			copy(offset = offset + n)

	case class ParseErrors(stack: List[(Location, String)]):
		def push(l: Location, msg: String): ParseErrors =
			copy(stack = (l, msg) :: stack)

		def label(s: String): ParseErrors =
			copy(stack = latestLoc.map((_, s)).toList)

		def latestLoc: Option[Location] =
			latest.map((l, _) => l)

		def latest: Option[(Location, String)] =
			stack.lastOption

	object Parser extends Parsers[Parser.Parser]:
		opaque type Parser[+A] = Location => Result[A]

		enum Result[+A]:
			case Success(get: A, charsConsumed: Int)
			case Failure(get: ParseErrors, isCommitted: Boolean)

			def mapError(f: ParseErrors => ParseErrors): Result[A] =
				this match
					case Failure(e, c) => Failure(f(e), c)
					case _ => this

			def uncommit: Result[A] =
				this match
					case Failure(e, true) => Failure(e, false)
					case _ => this

			def addCommit(isCommitted: Boolean): Result[A] =
				this match
					case Failure(e, c) => Failure(e, c || isCommitted)
					case _ => this

			def advanceSuccess(n: Int): Result[A] =
				this match
					case Success(a, m) => Success(a, m + n)
					case _ => this

		def string(s: String): Parser[String] =
			(l: Location) =>
				if l.input.startsWith(s) then
					Result.Success(s, s.length)
				else
					Result.Failure(l.toError(s"Expected $s"), true)

		def regex(r: Regex): Parser[String] =
			(l: Location) =>
				r.findPrefixOf(l.input) match
					case Some(a) => Result.Success(a, a.length)
					case None    => Result.Failure(l.toError(s"Expected $r"), true)

		extension [A](self: Parser[A])
			def run(input: String): Either[ParseErrors, A] =
				self(Location(input)) match
					case Result.Success(a, _) => Right(a)
					case Result.Failure(e, _) => Left(e)

			def flatMap[B](f: A => Parser[B]): Parser[B] =
				(l: Location) => self(l) match
					case Result.Success(a, n) =>
						f(a)(l.advanceBy(n))
							.addCommit(n != 0)
							.advanceSuccess(n)
					case Result.Failure(e, c) => Result.Failure(e, c)

			def slice(that: Parser[A]): Parser[String] =
				???

			def scope(msg: String): Parser[A] =
				(l: Location) => self(l).mapError(_.push(l, msg))

			def label(msg: String): Parser[A] =
				(l: Location) => self(l).mapError(_.label(msg))

			def attempt: Parser[A] =
				(l: Location) => self(l).uncommit

			@annotation.targetName("or")
			def |(that: => Parser[A]): Parser[A] =
				(l: Location) => self(l) match
					case Result.Failure(_, false) => that(l)
					case r => r

	enum JSON:
		case JNull
		case JNumber(get: Double)
		case JString(get: String)
		case JBool(get: Boolean)
		case JArray(get: IndexedSeq[JSON])
		case JObject(get: Map[String, JSON])

	object JSON:
		???

	// Why succeed? Is it truely unit ??
	// Flatmap stack overflow.
	pprint.log(Parser.succeed("He").run("Helloo"))
