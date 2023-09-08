object Chapter12 extends App:
	import Chapter10.Monoid
	import Chapter11.Functor
	import Chapter10.Monoid.Foldable.Foldable
	import Chapter11.Monad

	trait Applicative[F[_]] extends Functor[F]:
		self =>
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

			// Exercise 12.8
			def product[G[_]](using ag: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] =
				new Applicative[[x] =>> (F[x], G[x])]:
					def mapTwo[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
						(self.mapTwo(fa(0), fb(0))(f), ag.mapTwo(fa(1), fb(1))(f))

					def unit[A](a: => A): (F[A], G[A]) =
						(self.unit(a), ag.unit(a))

			// Exercise 12.9
			def compose[G[_]](using ag: Applicative[G]): Applicative[[x] =>> F[G[x]]] =
				new Applicative[[x] =>> F[G[x]]]:
					def mapTwo[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
						self.mapTwo(fa, fb)((ga, gb) => ag.mapTwo(ga, gb)(f))

					def unit[A](a: => A): F[G[A]] =
						self.unit(ag.unit(a))

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
					Chapter9.Parser.succeed(a)

		val parserMonad: Monad[Parser] =
			new Monad[Parser]:
				def unit[A](a: => A): Parser[A] =
					Chapter9.Parser.succeed(a)

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

	// Exercise 12.7: See tests.

	// Exercise 12.10: See tests.

	// Exercise 12.11
	//def composeMonad[F[_], G[_]](using mf: Monad[F], mg: Monad[G], t: Traverse[G]): Monad[[x] =>> F[G[x]]] =
	//	new Monad[[x] =>> F[G[x]]]:
	//		def unit[A](a: => A): F[G[A]] =
	//			mf.unit(mg.unit(a))
	//
	//		def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
	//			???

	trait Traverse[F[_]] extends Functor[F] with Foldable[F]:
		self =>
			def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using Applicative[G]): G[F[B]] =
				sequence(map(fa)(f))

			def sequence[G[_], A](fga: F[G[A]])(using Applicative[G]): G[F[A]] =
				traverse(fga)(identity)

			type Const[M, _] = M

			object Const:
				given monoidApplicative[M](using m: Monoid[M]): Applicative[[x] =>> Const[M, x]] with
					def mapTwo[A, B, C](fa: M, fb: M)(f: (A, B) => C): M =
						m.combine(fa, fb)

					def unit[A](a: => A): M = m.zero

			def foldMap[A, M](as: F[A])(f: A => M)(using Monoid[M]): M =
				traverse[[x] =>> Const[M, x], A, Nothing](as)(f)(using Const.monoidApplicative)

			type Id[+A] = A

			object Id:
				given idMonad: Monad[Id] with
					def unit[A](a: => A) = a
					def flatMap[A, B](a: A)(f: A => B): B = f(a)

			def map[A, B](fa: F[A])(f: A => B): F[B] =
				traverse[Id, A, B](fa)(f)(using Id.idMonad)

			import Chapter6.State
			import Chapter6.State.*
			import Chapter11.stateMonad

			def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
				traverse(fa)(f)(using stateMonad)

			def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
				val res: State[S, F[B]] =
					traverseS(fa): (a: A) =>
						for
							s1 <- get[S]
							(b, s2) = f(a, s1)
							_ <- set(s2)
						yield b

				res.run(s)

			override def toList[A](fa: F[A]): List[A] =
				mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2.reverse

			def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
				mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

			// Exercise 12.16
			def reverse[A](fa: F[A]): F[A] =
				mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

			// Exercise 12.17
			@annotation.targetName("foldLeftViaMapAccum")
			def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
				mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

			// Exercise 12.18
			def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
				(using ag: Applicative[G], ah: Applicative[H])
			: (G[F[B]], H[F[B]]) =
				traverse[[x] =>> (G[x], H[x]), A, B](fa)((a: A) => (f(a), g(a)))(using ag.product(using ah))

			// Exercise 12.19 TODO: Get this compiling
			//def compose[G[_]](using tg: Traverse[G]): Traverse[[x] =>> F[G[x]]] =
			//	new Traverse[[x] =>> F[G[x]]]:
			//		override def traverse[H[_], A, B](fa: F[G[A]])(f: A => H[B])(using Applicative[H]): G[F[G[B]]] =
			//			self.traverse(fa)((ga: G[A]) => tg.traverse(ga)(f))

	// Exercise 12.13
	case class Tree[+A](head: A, tail: List[Tree[A]])

	object Traverse:
		given listTraverse: Traverse[List] with
			override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(using ag: Applicative[G]): G[List[B]] =
				fa.foldRight(ag.unit(List.empty[B])):
					(a, acc) => ag.mapTwo(f(a), acc)(_ :: _)

		given optionTraverse: Traverse[Option] with
			override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(using ag: Applicative[G]): G[Option[B]] =
				fa match
					case Some(a) => ag.map(f(a))(Some(_))
					case None    => ag.unit(None)

		given mapTraverse[K]: Traverse[[v] =>> Map[K, v]] with
			override def traverse[G[_], A, B](fa: Map[K, A])(f: A => G[B])(using ag: Applicative[G]): G[Map[K, B]] =
				fa.foldRight(ag.unit(Map.empty[K, B])):
					case ((k, a), acc) => ag.mapTwo(f(a), acc)((b, map) => map.updated(k, b))

		given treeTraverse: Traverse[Tree] with
			override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(using ag: Applicative[G]): G[Tree[B]] =
				val h: G[B] = f(fa.head)

				val t: G[List[Tree[B]]] =
					listTraverse.traverse(fa.tail)((a: Tree[A]) => traverse(a)(f))

				ag.mapTwo(h, t)(Tree(_, _))

	end Traverse

	// Exercise 12.15
	// In order to extend Functor you have to be able to implement the map function.
	// Traverse is tructure preserving so it can implement Functor. Some Foldable
	// instances are not structure preserving and that is why in general Foldable
	// cannot implement Functor.

	object Monad:
		// Exercise 12.20
		def composeM[G[_], H[_]](using mg: Monad[G], mh: Monad[H], th: Traverse[H]): Monad[[x] =>> G[H[x]]] =
			new Monad[[x] =>> G[H[x]]]:
				def unit[A](a: => A): G[H[A]] = mg.unit(mh.unit(a))

				override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
					mg.flatMap(mna)(na => mg.map(th.traverse(na)(f))(mh.join))

