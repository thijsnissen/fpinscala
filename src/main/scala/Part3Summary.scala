object Part3Summary extends App:
	// Monoid
	trait Monoid[A]:
		def combine(a1: A, a2: A): A

		def zero: A

	// Foldable
	trait Foldable[F[_]]:
		def foldRight[A, B](as: F[A], z: B)(f: (A, B) => B): B

		def foldLeft[A, B](as: F[A], z: B)(f: (B, A) => B): B

		def foldMap[A, B](as: F[A], m: Monoid[B])(f: A => B): B =
			foldLeft(as, m.zero)((b, a) => m.combine(f(a), b))

		def concat[A](as: F[A], m: Monoid[A]): A =
			foldLeft(as, m.zero)(m.combine)

	// Functor
	trait Functor[F[_]]:
		def map[A, B](fa: F[A])(f: A => B): F[B]

	// Applicative
	trait Applicative[F[_]] extends Functor[F]:
		def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
			mapTwo(fab, fa)((a, b) => a(b))

		def unit[A](a: => A): F[A]

		def mapTwo[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

		def map[A, B](fa: F[A])(f: A => B): F[B] =
			mapTwo(fa, unit(()))((a, _) => f(a))

		def sequence[A](lma: List[F[A]]): F[List[A]] =
			traverse(lma)(identity)

		def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
			la.foldRight(unit(List.empty[B])):
				(a, acc) => mapTwo(f(a), acc)(_ :: _)

		def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
			sequence(List.fill(n)(ma))

		def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
			mapTwo(ma, mb)((_, _))

	// Monad
	trait Monad[F[_]] extends Applicative[F]:
		def unit[A](a: => A): F[A]

		def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

		def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
			a => flatMap(f(a))(b => g(b))

		def join[A](mma: F[F[A]]): F[A] =
			flatMap(mma)(identity)

		def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
			ms.foldRight(unit(List.empty[A])):
				(a: A, acc: F[List[A]]) =>
					flatMap(f(a))((b: Boolean) => if b then map(acc)(a :: _) else acc)

		def mapTwo[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
			flatMap(fa)(a => map(fb)(b => f(a, b)))

		override def map[A, B](fa: F[A])(f: A => B): F[B] =
			flatMap(fa)(a => unit(f(a)))

  // Traversable
