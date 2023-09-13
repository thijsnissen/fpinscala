object Monads:
	import Part3Summary.*
	import Part3Summary.Monad.*

	// The Identity Monad
	opaque type Id[+A] = A

	object Id:
		extension [A](self: Id[A])
			def get: A = self

		given idMonad: Monad[Id] with
			def unit[A](a: => A): Id[A] = a

			def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
				f(fa.get)

	// The State Monad
	opaque type State[S, +A] =
		S => (A, S)

	object State:
		def get[S]: State[S, S] =
			(s: S) => (s, s)

		def set[S](s: => S): State[S, Unit] =
			_ => ((), s)

		extension[S, A] (self: State[S, A])
			def run(s: => S): (A, S) =
				self(s)

		given stateMonad[S]: Monad[[x] =>> State[S, x]] with
			def unit[A](a: => A): State[S, A] =
				s => (a, s)

			def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
				s =>
					val (aa, as) = fa(s)
					val (ba, bs) = f(aa)(as)

					(ba, bs)

	// The Reader Monad
	opaque type Reader[R, A] =
		R => A

	object Reader:
		def apply[R, A](f: R => A): Reader[R, A] =
			f

		def ask[R]: Reader[R, R] = r => r

		extension[R, A] (self: Reader[R, A])
			def run(r: R): A =
				self(r)

		given readerMonad[R]: Monad[[x] =>> Reader[R, x]] with
			def unit[A](a: => A): Reader[R, A] =
				(_: R) => a

			def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
				(r: R) => f(fa.run(r)).run(r)

	// The IO Monad
	opaque type IO[A] =
		() => A

	object IO:
		def PrintLine(msg: String): IO[Unit] =
			() => println(msg)

		def ReadLine: IO[String] =
			() => scala.io.StdIn.readLine

		extension[A] (self: IO[A])
			def unsafeRun: A =
				self()

		given ioMonad: Monad[IO] with
			def unit[A](a: => A): IO[A] =
				() => a

			def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
				f(fa.unsafeRun)

	// The IO Monad via Free
	import NonBlockingPar.Par

	opaque type IOFree[A] = Free[Par, A]

	// The Free Monad
	enum Free[F[_], A]:
		case Return(a: A)
		case Suspend(s: F[A])
		case Chain[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

		def translate[G[_]](fToG: [x] => F[x] => G[x]): Free[G, A] =
			this.runFree([x] => (fx: F[x]) => Suspend(fToG(fx)))

	object Free:
		type TailRec[A] = Free[Function0, A]

		extension [A](self: TailRec[A])
			def runTailRec: A =
				self match
					case Return(a)   => a
					case Suspend(s)  => s()
					case Chain(x, f) => x match
						case Return(a)   => f(a).runTailRec
						case Suspend(s)  => f(s()).runTailRec
						case Chain(y, g) => y.flatMap(a => g(a).flatMap(f)).runTailRec

		@annotation.tailrec
		def step[F[_], A](a: Free[F, A]): Free[F, A] =
			a match
				case Chain(Chain(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
				case Chain(Return(x), f) => step(f(x))
				case _ => a

		extension [F[_], A](self: Free[F, A])
			def run(using F: Monad[F]): F[A] =
				step(self) match
					case Return(a)   => F.unit(a)
					case Suspend(s)  => s
					case Chain(x, f) => x match
						case Suspend(s) => F.flatMap(s)(a => f(a).run)
						case _          => sys.error("Unreacheable case since step takes care of the other cases.")

			def runFree[G[_]](t: [x] => F[x] => G[x])(using G: Monad[G]): G[A] =
				step(self) match
					case Return(a)   => G.unit(a)
					case Suspend(s)  => t(s)
					case Chain(x, f) => x match
						case Suspend(s) => G.flatMap(t(s))(a => f(a).runFree(t))
						case _          => sys.error("Unreacheable case since step takes care of the other cases.")

		given freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
			def unit[A](a: => A): Free[F, A] =
				Free.Return(a)

			def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
				Free.Chain(fa, f)
