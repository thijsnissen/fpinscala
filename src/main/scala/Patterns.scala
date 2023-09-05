object Patterns extends App:
	trait Semigroup[A]:
		def append(l: A)(r: A): A

		extension (self: A)
			@annotation.targetName("appendInfix")
			def |<>|(that: A): A =
				append(self)(that)

	trait Monoid[A] extends Semigroup[A]:
		def empty: A

	trait Functor[F[_]]:
		def map[A, B](f: A => B)(fa: F[A]): F[B]

		extension [A](self: F[A])
			@annotation.targetName("mapInfix")
			def |@|[B](f: A => B): F[B] =
				map(f)(self)

	trait Applicative[F[_]](using Functor[F]):
		def pure[A](a: A): F[A]

		def apply[A, B](ff: F[A => B])(fa: F[A]): F[B]

		extension [A](self: F[A])
			@annotation.targetName("applyInfix")
			def |*|[B](that: F[A => B]): F[B] =
				apply(that)(self)

			def mapTwo[B, C](that: F[B])(f: (A, B) => C): F[C] =
				that |*| (self |@| f.curried)

	trait Monad[F[_]](using af: Applicative[F]):
		def unit[A](a: A): F[A] =
			af.pure(a)

		def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

		extension[A](self: F[A])
			@annotation.targetName("flatMapInfix")
			def >>=[B](f: A => F[B]): F[B] =
				flatMap(self)(f)

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
		def apply[A](as: A*): List[A] =
			if as.isEmpty then
				Nil
			else
				Cons(as.head, apply(as.tail*))

		given Functor[List] with
			def map[A, B](f: A => B)(fa: List[A]): List[B] =
				fa match
					case Cons(h, t) => Cons(f(h), t |@| f)
					case Nil        => Nil

		given Applicative[List] with
			def pure[A](a: A): List[A] =
				Cons(a, Nil)

			def apply[A, B](ff: List[A => B])(fa: List[A]): List[B] =
				(fa, ff) match
					case (Cons(fah, fat), Cons(ffh, fft)) => Cons(ffh(fah), fat |*| fft)
					case (_, _)                       => Nil

		given Monad[List] with
			def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
				fa match
					case Cons(h, t) => f(h) |<>| (t >>= f)
					case Nil        => Nil

		given Foldable[List] with
			extension [A](self: List[A])
				def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
					self match
						case Cons(h, t) => f(h) |<>| t.foldMap(f)
						case Nil        => mb.empty

				@annotation.tailrec
				def foldRight[B: Monoid](z: B)(f: A => B): B =
					self match
						case Cons(h, t) => t.foldRight(z |<>| f(h))(f)
						case Nil        => z

		given traversable: Traversable[List] with
			extension[A] (self: List[A])
				def traverse[B, F[_]](f: A => F[B])(using af: Applicative[F]): F[List[B]] =
					self match
						case Cons(h, t) => f(h).mapTwo(t.traverse(f))(Cons(_, _))
						case Nil        => af.pure(Nil)

		given listMonoid[A]: Monoid[List[A]] =
			new Monoid[List[A]]:
				def empty: List[A] =
					Nil

				def append(l: List[A])(r: List[A]): List[A] =
					l match
						case Cons(h, t) => Cons(h, t |<>| r)
						case Nil => r
