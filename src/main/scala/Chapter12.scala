object Chapter12 extends App:
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
