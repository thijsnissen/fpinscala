object Chapter11 extends App:
	trait Functor[F[_]]:
		def map[A, B](fa: F[A])(f: A => B): F[B]

		def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
			(map(fab)(_._1), map(fab)(_._2))

		def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
			e match
				case Left(fa)  => map(fa)(Left(_))
				case Right(fb) => map(fb)(Right(_))

	val listFunctor: Functor[List] =
		new Functor[List]:
			def map[A, B](fa: List[A])(f: A => B): List[B] =
				fa.map(f)

	import Chapter12.Applicative

	trait Monad[F[_]] extends Applicative[F]:
		def unit[A](a: => A): F[A]

		def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

		override def map[A, B](fa: F[A])(f: A => B): F[B] =
			flatMap(fa)(a => unit(f(a)))

		def mapTwo[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
			flatMap(fa)(a => map(fb)(b => f(a, b)))

		// Exercise 11.3
		override def sequence[A](lma: List[F[A]]): F[List[A]] =
			traverse(lma)(identity)

		override def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
			la.foldRight(unit(List.empty[B])):
				(a, acc) => mapTwo(f(a), acc)(_ :: _)

		// Exercise 11.4
		override def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
			sequence(List.fill(n)(ma))

		def replicateMRec[A](n: Int, ma: F[A]): F[List[A]] =
			@annotation.tailrec
			def loop(i: Int, a: A, acc: List[A]): List[A] =
				if i <= 0 then acc
				else loop(i - 1, a, a :: acc)

			map(ma)((a: A) => loop(n, a, List.empty[A]))

		def replicateMRec2[A](n: Int, ma: F[A]): F[List[A]] =
			if n <= 0 then unit(Nil)
			else mapTwo(ma, replicateMRec2(n - 1, ma))(_ :: _)

		override def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
			mapTwo(ma, mb)((_, _))

		// Exercise 11.6
		def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
			ms.foldRight(unit(List.empty[A])):
				(a: A, acc: F[List[A]]) =>
					flatMap(f(a))((b: Boolean) => if b then map(acc)(a :: _) else acc)

		// Exercise 11.7
		def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
			a => flatMap(f(a))(b => g(b))

		// Exercise 11.8
		def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
			compose(_ => fa, f)(())

		// Exercise 11.12
		def join[A](mma: F[F[A]]): F[A] =
			flatMap(mma)(identity)

		// Exercise 11.13
		def flatMapViaJoin[A, B](fa: F[A])(f: A => F[B]): F[B] =
			join(map(fa)(f))

		def composeViaJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
			(a: A) => join(map(f(a))(g))

	val listMonad: Monad[List] =
		new Monad[List]:
			def unit[A](a: => A): List[A] = List(a)

			def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
				fa.flatMap(f)

	val optionMonad: Monad[Option] =
		new Monad[Option]:
			def unit[A](a: => A): Option[A] = Some(a)

			def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
				fa.flatMap(f)

	// Exercise 11.5: How will 'repplicateM' behave for different choises of F
	// (i.e. List or Option)? Describe in your own words the general meaning of
	// 'replicateM': It repeats the value of the monad F n times in a single value.
	// The monad itself decides how these values are are actually combined. I.e. List
	// always produces Lists, whereas Option can either be Some or None, depending
	// on the original input.

	// For List, the replicateM function will generate a list of lists. It will
	// contain all the lists of length n with elements selected from the input list.

  // For Option, it will generate either Some or None based on whether the input
	// is Some or None. The Some case will contain a list of length n that repeats
	// the element in the input Option.

	// The general meaning of replicateM is described well by the implementation
	// sequence(List.fill(n)(ma)). It repeats the ma monadic value n times and
	// gathers the results in a single value, where the monad F determines how
	// values are actually combined.

	// Exercise 11.9, see test.
	// x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
	// compose(compose(f, g), h) == compose(f, compose(g, h))

	// Exercise 11.10: See test.
	// compose(f, unit)(v) == f(v)
	// a => f(a).flatMap(unit)(v) = f(v)
	// f(v).flatMap(unit) = f(v)
	// x.flatMap(unit) == x

	// Exercise 11.11: See test.

	// Exercise 11.14
	// Associativity: join(join(f(v)) == f(v).flatMap(join)
	// Identity: join(map(f(v))(unit)) == f(v)

	// Exercise 11.15
	// In general the associativity law states that the order in which
	// operations are executed does not matter for the end result. In case of Par, join
	// first runs the inner threads and then the outer. In case of Parser join first
	// runs the outer parser and then the inner parser.

	// Exercise 11.16
	// The left identity law states that wrapping a monad in it's unit and then
	// flattening it has no effect. The right identity law states that wrapping
	// each value of a monad in it's unit and then flattening it has no effect.

	// Example Product Order System p. 195
	import Chapter8.Gen
	import Chapter8.Gen.*

	case class Order(item: Item, quantity: Int)
	case class Item(name: String, price: Double)

	val genItem: Gen[Item] =
		for
			name <- Gen.string(3)
			price <- Gen.choose(1.0, 100.0)
		yield
			Item(name, price)

	val genOrder: Gen[Order] =
		for
			item     <- genItem
			quantity <- Gen.choose(1, 100)
		yield
			Order(item, quantity)

	val genOrderTest =
		genOrder
			.listOfNViaFlatMap(Gen.choose(1, 10))
			.run(Chapter6.SimpleRNG(123))

	//pprint.log(genOrderTest)

	// The Identity Monad
	case class Id[A](value: A):
		def flatMap[B](f: A => Id[B]): Id[B] =
			f(value)

		def map[B](f: A => B): Id[B] =
			Id(f(value))

	val idMonad: Monad[Id] =
		new Monad[Id]:
			def unit[A](a: => A): Id[A] = Id(a)

			def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)

	// The State Monad
	import Chapter6.State
	import Chapter6.State.*

	type IntState[A] = State[Int, A]

	val intStateMonad: Monad[IntState] =
		new Monad[IntState]:
			def unit[A](a: => A): IntState[A] = State.unit(a)

			def flatMap[A, B](fa: IntState[A])(f: A => IntState[B]): IntState[B] =
				fa.flatMap(f)

	// Exercise 11.2 with type lambda =>>
	def stateMonad[S]: Monad[[x] =>> State[S, x]] =
		new Monad[[x] =>> State[S, x]]:
			def unit[A](a: => A): State[S, A] =
				State.unit(a)

			def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
				fa.flatMap(f)

	// Exercise 11.18
	// ReplicateM gives a list of n following results from the state and the next state.
	// mapTwo combines the results of two states of the same type and returns te next state.
	// Sequence returns a list with the results of all the states in the input list and the next state.

	// Exercise 11.19
	// getting and setting the state does nothing
	// getState.flatMap(setState) == unit(())
	//
	// setting the state to 's' and getting it back out yields 's'
	// setState(s).flatMap(_ => getState) == unit(s)

	// Example of the State monad on p. 202
	val F = stateMonad[Int]

	def zipWithIndex[A](as: List[A]): List[(Int, A)] =
		as
			.foldLeft(F.unit(List.empty[(Int, A)])): (acc, a) =>
				for
					xs <- acc
					n  <- get
					_  <- set(n + 1)
				yield
					(n, a) :: xs
			.run(0)
			._1
			.reverse

	// Exercise 11.20
	// Reader transforms an R into an A. It functions as a sort of read only state.
	// The Reader Monad is used for representing computations that depend on some
	// shared immutable environment or configuration.
	// FlatMap passes r along to the outer and the inner reader.
	opaque type Reader[R, A] =
		R => A

	object Reader:
		def apply[R, A](f: R => A): Reader[R, A] =
			f

		extension [R, A](self: Reader[R, A])
			def run(r: R): A =
				self(r)

		given readerMonad[R]: Monad[[x] =>> Reader[R, x]] with
			def unit[A](a: => A): Reader[R, A] =
				(_: R) => a

			def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
				(r: R) => f(fa.run(r)).run(r)

