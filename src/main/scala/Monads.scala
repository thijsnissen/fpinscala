object Monads:
	import Part3Summary.Monad

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
		extension[S, A](self: State[S, A])
			def get: State[S, S] =
				(s: S) => (s, s)

			def set(s: => S): State[S, Unit] =
				_ => ((), s)

		given stateMonad[S]: Monad[[x] =>> State[S, x]] with
			def unit[A](a: => A): State[S, A] =
				s => (a, s)

			def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
				s =>
					val (aa, as) = fa(s)
					val (ba, bs) = f(aa)(as)

					(ba, bs)
