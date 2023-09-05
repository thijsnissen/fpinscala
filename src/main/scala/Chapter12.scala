object Chapter12 extends App:
	import Chapter10.Monoid
	import Chapter11.Functor

	trait Applicative[F[_]] extends Functor[F]:
		def mapTwo[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

		def unit[A](a: => A): F[A]

		def map[A, B](fa: F[A])(f: A => B): F[B] =
			mapTwo(fa, unit(()))((a, _) => f(a))

		def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
			la.foldRight(unit(List.empty[B])):
				(a, acc) => mapTwo(f(a), acc)(_ :: _)

		// Exercise 12.1
		def sequence[A](lma: List[F[A]]): F[List[A]] =
			traverse(lma)(identity)

		def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
			sequence(List.fill(n)(ma))

		def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
			mapTwo(ma, mb)((_, _))

		// Exercise 12.2
		def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
			mapTwo(fab, fa)((a, b) => a(b))

		def mapTwoViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
			apply(apply(unit(f.curried))(fa))(fb)

		def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
			apply(unit(f))(fa)

		// Exercise 12.3
		def mapThree[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
			apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

		def mapFour[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
			apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

		def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
			val (a, (b, c)) = p

			((a, b), c)

		def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
			(i, i2) => (f(i), g(i2))

		// Exercise 12.12
		def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
			ofa.foldRight(unit(Map.empty[K, V])):
				case ((k, fv), acc) => mapTwo(acc, fv)((a, v) => a + (k -> v))

	val listApplicative: Applicative[List] =
		new Applicative[List]:
			def mapTwo[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
				@annotation.tailrec
				def loop1(a: A, b: List[B], acc: List[C]): List[C] =
					b match
						case h :: t => loop1(a, t, f(a, h) :: acc)
						case Nil    => acc

				@annotation.tailrec
				def loop2(a: List[A], b: List[B], acc: List[C]): List[C] =
					a match
						case h :: t => loop2(t, b, loop1(h, b, List.empty[C] ++ acc))
						case Nil    => acc.reverse

				loop2(fa, fb, List.empty[C])

			def unit[A](a: => A): List[A] =
				List(a)

	// Example on p. 210
	object Parser:
		import Chapter9.Parser
		import Chapter9.Parser.*
		import Chapter11.Monad

		val parserApplicative: Applicative[Parser] =
			new Applicative[Parser]:
				def mapTwo[A, B, C](fa: Parser[A], fb: Parser[B])(f: (A, B) => C): Parser[C] =
					fa.mapTwo(fb)(f)

				def unit[A](a: => A): Parser[A] =
					Parser.succeed(a)

		val parserMonad: Monad[Parser] =
			new Monad[Parser]:
				def unit[A](a: => A): Parser[A] =
					Parser.succeed(a)

				def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
					fa.flatMap(f)

		type Date = String

		case class Row(date: Date, temperature: Double)

		val date: Parser[Date] =
			whitespace ~ regex("\\d+/\\d+/\\d+".r)

		val dateSep: Parser[Date] =
			for
				d <- whitespace ~ regex("\\d+/\\d+/\\d+".r)
				_ <- char(',')
			yield
				d

		val temp: Parser[Double]  =
			for
				i <- whitespace ~ regex("\\d+".r)
			yield
				i.toDouble

		val header: Parser[Parser[Row]] =
			for
				_ <- whitespace ~ string("#")
				a <- whitespace ~ regex("\\w+".r)
				_ <- char(',')
				b <- whitespace ~ regex("\\w+".r)
			yield
				(a, b) match
					case ("Date", "Temperature") =>
						for
							d <- date
							_ <- char(',') ** whitespace
							t <- temp
						yield
							Row(d, t)
					case ("Temperature", "Date") =>
						for
							t <- temp
							_ <- char(',') ** whitespace
							d <- date
						yield
							Row(d, t)

		// Structure of computation is fixed before it starts. This parser has more
		// information up front than rowsMon and can therefore make assumptions that
		// could possibly lead to a more efficient implementation.
		val rowsApl: Parser[List[Row]] =
			parserApplicative
				.mapTwo(dateSep, temp)(Row.apply)
				.separator("\n")

		// Structure of computation is dynamic and depends on result of header
		val rowsMon: Parser[List[Row]] =
			parserMonad.flatMap(header):
				(r: Parser[Row]) => whitespace ~ r.separator("\n")
	end Parser

	// Exercise 12.4
	// This transposes the list! That is, we start with a list of rows, each of
	// which is possibly infinite in length. We get back a single row, where each
	// element is the column of values at that position.

	val lazyListApplicative: Applicative[LazyList] =
		new Applicative[LazyList]:
			def mapTwo[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (A, B) => C): LazyList[C] =
				fa zip fb map f.tupled

			def unit[A](a: => A): LazyList[A] = LazyList(a)

	// Exercise 12.5
	given EitherMonad[E]: Monad[[x] =>> Either[E, x]] with
		def unit[A](a: => A): Either[E, A] =
			Right(a)

		def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
			fa.flatMap(f)

	enum Validation[+E, +A]:
		case Valid(get: A)
		case Invalid(error: E)

	type Errors = List[String]

	import Validation.*
	import java.time.LocalDate
	import Chapter10.Monoid.listMonoid

	given ValidationApplicative[E](using m: Monoid[E]): Applicative[[x] =>> Validation[E, x]] with
		def unit[A](a: => A): Validation[E, A] = Valid(a)

		def mapTwo[A, B, C](
			fa: Validation[E, A],
			fb: Validation[E, B]
		)(f: (A, B) => C): Validation[E, C] =
			(fa, fb) match
				case (Valid(a), Valid(b))     => unit(f(a, b))
				case (Invalid(a), Invalid(b)) => Invalid(m.combine(a, b))
				case (Invalid(a), _)          => Invalid(a)
				case (_, Invalid(b))          => Invalid(b)

	case class WebForm(name: String, birthdate: LocalDate, phoneNumber: String)

	object WebForm:
		def validName(name: String): Validation[Errors, String] =
			if name.nonEmpty then Valid(name)
			else Invalid(List("Name cannot be empty"))

		def validBirthdate(birthdate: String): Validation[Errors, LocalDate] =
			try Valid(LocalDate.parse(birthdate))
			catch
				case _: java.time.format.DateTimeParseException =>
					Invalid(List("Birthdate must be in the form yyyy-MM-dd"))

		def validPhone(phoneNumber: String): Validation[Errors, String] =
			if phoneNumber.matches("[0-9]{10}") then Valid(phoneNumber)
			else Invalid(List("Phone number must be 10 digits"))

		def validWebForm(name: String, birthdate: String, phoneNumber: String): Validation[Errors, WebForm] =
			ValidationApplicative(using listMonoid)
				.mapThree(
					validName(name),
					validBirthdate(birthdate),
					validPhone(phoneNumber)
				)(WebForm.apply)

	end WebForm

	// TODO: Exercise 12.7
	// TODO: Exercise 12.8
	// TODO: Exercise 12.9
	// TODO: Exercise 12.10
	// TODO: Exercise 12.11

	trait Traverse[F[_]] extends Functor[F]:
		def traverse[G[_], A, B](using Applicative[G])(fa: F[A])(f: A => G[B]): G[F[B]] =
			sequence(map(fa)(f))

		def sequence[G[_], A](using Applicative[G])(fga: F[G[A]]): G[F[A]] =
			traverse(fga)(ga => ga)

	// Exercise 12.13
	case class Tree[+A](head: A, tail: List[Tree[A]])

	object Traverse:
		given listTraverse: Traverse[List] with
			def traverse[G[_], A, B](using Applicative[G])(fa: List[A])(f: A => G[B]): G[List[B]] =
				???

		given optionTraverse: Traverse[Option] with
			def traverse[G[_], A, B](using Applicative[G])(fa: Option[A])(f: A => G[B]): G[Option[B]] =
				???

		given treeTraverse: Traverse[Tree] with
			def traverse[G[_], A, B](using Applicative[G])(fa: Tree[A])(f: A => G[B]): G[Tree[B]] =
				???

