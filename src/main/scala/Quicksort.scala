object Quicksort extends App:
	trait Monad[F[_]]:
		def unit[A](a: => A): F[A]

		def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

	object Monad:
		extension[F[_], A] (self: F[A])(using M: Monad[F])
			def flatMap[B](f: A => F[B]): F[B] =
				M.flatMap(self)(f)

			def map[B](f: A => B): F[B] =
				M.flatMap(self)((a: A) => M.unit(f(a)))

	enum Free[F[_], A]:
		case Return(a: A)
		case Suspend(s: F[A])
		case FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

	object Free:
		import Monad.*
		export Monad.*

		@annotation.tailrec
		def step[F[_], A](free: Free[F, A]): Free[F, A] =
			free match
				case FlatMap(FlatMap(fx, f), g) => step(fx.flatMap(x => f(x).flatMap(g)))
				case FlatMap(Return(x), f)      => step(f(x))
				case _                          => free

		extension[F[_], A] (self: Free[F, A])
			def run(using F: Monad[F]): F[A] =
				self match
					case Return(a)               => F.unit(a)
					case Suspend(fa)             => fa
					case FlatMap(Suspend(fa), f) => fa.flatMap(a => f(a).run)
					case FlatMap(_, _)           => sys.error("Impossible, `step` eliminates these cases")

		given freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
			def unit[A](a: => A): Free[F, A] =
				Free.Return(a)

			def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
				Free.FlatMap(fa, f)

		given function0Monad: Monad[Function0] with
			def unit[A](a: => A): () => A =
				() => a

			def flatMap[A, B](fa: () => A)(f: A => () => B): () => B =
				f(fa())

		given ParMonad: Monad[NonBlockingPar.Par] with
			def unit[A](a: => A): NonBlockingPar.Par[A] =
				NonBlockingPar.Par.parMonad.unit(a)

			def flatMap[A, B](fa: NonBlockingPar.Par[A])(f: A => NonBlockingPar.Par[B]): NonBlockingPar.Par[B] =
				NonBlockingPar.Par.parMonad.flatMap(fa)(f)

	import Free.*
	import Free.given
	import math.Ordering.Implicits.infixOrderingOps

	type TailRec[A] = Free[Function0, A]
	type Async[A]   = Free[NonBlockingPar.Par, A]

	def quicksortTailRec[A](s: Seq[A])(using Ordering[A]): TailRec[Seq[A]] =
		if s.isEmpty then Return(s) else Suspend: () =>
			val (lessThan, equalTo, greaterThan) =
				val pivot = s.head

				s.foldLeft((Seq.empty[A], Seq.empty[A], Seq.empty[A])):
					case ((l, e, g), x) =>
						if      x > pivot  then (l, e, x +: g)
						else if x == pivot then (l, x +: e, g)
						else                    (x +: l, e, g) // x < pivot

			//for
			//	lt <- quicksortTailRec(lessThan)
			//	gt <- quicksortTailRec(greaterThan)
			//yield
			//	lt ++ equalTo ++ gt

			???

	def quicksortAsync[A](s: Seq[A])(using Ordering[A]): Async[Seq[A]] =
		???

	val randomSeq: List[Int] =
		(for _ <- 0 until 1000000 yield scala.util.Random.nextInt(Int.MaxValue)).toList

	println:
		quicksortTailRec(randomSeq).run
