object Patterns extends App:
	trait Monoid[A]:
		def empty: A

		def append(l: A)(r: A): A

		extension (self: A)
			@annotation.targetName("append")
			def |<>|(that: A): A =
				append(self)(that)

	trait Functor[F[_]]:
		def map[A, B](f: A => B)(fa: F[A]): F[B]

		extension [A](self: F[A])
			@annotation.targetName("map")
			def |@|[B](f: A => B): F[B] =
				map(f)(self)

	trait Applicative[F[_]](using Functor[F]):
		def pure[A](a: A): F[A]

		def apply[A, B](ff: F[A => B])(fa: F[A]): F[B]

		extension [A](self: F[A])
			@annotation.targetName("apply")
			def |*|[B](that: F[A => B]): F[B] =
				apply(that)(self)

			def mapTwo[B, C](that: F[B])(f: (A, B) => C): F[C] =
				that |*| (self |@| f.curried)

	trait Monad[F[_]](using af: Applicative[F]):
		def unit[A](a: A): F[A] =
			af.pure(a)

		def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

		extension[A](self: F[A])
			@annotation.targetName("flatMap")
			def >>=[B](f: A => F[B]): F[B] =
				flatMap(self)(f)

	// Folable is an interface for folding operations on a Monoid.
	trait Foldable[F[_]]:
		extension [A](self: F[A])
			def foldMap[B](f: A => B)(using Monoid[B]): B

			def foldRight[B](z: B)(f: A => B)(using Monoid[B]): B

	trait Traversable[T[_]](using Functor[T], Foldable[T]):
		def sequence[A, F[_]](t: T[F[A]])(using Applicative[F]): F[T[A]] =
			t.traverse(identity)

		extension [A](self: T[A])
			def traverse[B, F[_]](f: A => F[B])(using Applicative[F]): F[T[B]]

	enum List[+A]:
		case Nil
		case Cons(head: A, tail: List[A])

	object List:
		given Functor[List] with
			extension[A] (self: List[A])
				def map[B](f: A => B): List[B] =
					self match
						case Cons(h, t) => Cons(f(h), t |@| f)
						case Nil        => Nil

		given Applicative[List] with
			def pure[A](a: A): List[A] =
				Cons(a, Nil)

			extension[A] (self: List[A])
				def apply[B](that: List[A => B]): List[B] =
					(self, that) match
						case (Cons(sh, st), Cons(th, tt)) => Cons(th(sh), st |*| tt)
						case (_, _)                       => Nil

		given Monad[List] with
			extension[A] (self: List[A])
				def flatMap[B](f: A => List[B]): List[B] =
					self match
						case Cons(h, t) => f(h) |<>| (t >>= f)
						case Nil        => Nil

		given Foldable[List] with
			extension [A](self: List[A])
				def foldMap[B: Monoid](f: A => B): B =
					self match
						case Cons(h, t) => f(h) |<>| t.foldMap(f)
						case Nil        => Nil

				@annotation.tailrec
				def foldRight[B: Monoid](z: B)(f: A => B): B =
					self match
						case Cons(h, t) => t.foldRight(z |<>| f(h))(f)
						case Nil        => z

		given listMonoid[A]: Monoid[List[A]] =
			new Monoid[List[A]]:
				def empty: List[A] =
					Nil

				extension (self: List[A])
					def append(that: List[A]): List[A] =
						self match
							case Cons(h, t) => Cons(h, t |<>| that)
							case Nil => that
