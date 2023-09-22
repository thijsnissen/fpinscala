object Part3Summary:
	// Monoid
	trait Semigroup[A]:
		def combine(a1: A, a2: A): A

	trait Monoid[A] extends Semigroup[A]:
		def zero: A

	// Functor
	trait Functor[F[_]]:
		def map[A, B](fa: F[A])(f: A => B): F[B]

	// Foldable
	trait Foldable[F[_]]:
		def foldLeft[A, B](as: F[A], z: B)(f: (B, A) => B): B

		def foldRight[A, B](as: F[A], z: B)(f: (A, B) => B): B =
			foldLeft(as, z)((b, a) => f(a, b))

		def foldMap[A, B](as: F[A], m: Monoid[B])(f: A => B): B =
			foldLeft(as, m.zero)((b, a) => m.combine(f(a), b))

		def concat[A](as: F[A], m: Monoid[A]): A =
			foldLeft(as, m.zero)(m.combine)

		def toList[A](fa: F[A]): List[A] =
			foldRight(fa, List.empty[A])(_ :: _)

	// Applicative
	trait Applicative[F[_]] extends Functor[F]:
		self =>
			def mapTwo[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

			def unit[A](a: => A): F[A]

			def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
				mapTwo(fab, fa)((a, b) => a(b))

			def map[A, B](fa: F[A])(f: A => B): F[B] =
				mapTwo(fa, unit(()))((a, _) => f(a))

			def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
				la.foldRight(unit(List.empty[B])):
					(a, acc) => mapTwo(f(a), acc)(_ :: _)

			def sequence[A](lma: List[F[A]]): F[List[A]] =
				traverse(lma)(identity)

			def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
				ofa.foldRight(unit(Map.empty[K, V])):
					case ((k, fv), acc) => mapTwo(acc, fv)((a, v) => a + (k -> v))

			def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
				sequence(List.fill(n)(ma))

			def mapThree[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
				apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

			def mapFour[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
				apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

			def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
				val (a, (b, c)) = p

				((a, b), c)

			def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
				(i, i2) => (f(i), g(i2))

			def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
				mapTwo(ma, mb)((_, _))

			def product[G[_]](using ag: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] =
				new Applicative[[x] =>> (F[x], G[x])]:
					def mapTwo[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
						(self.mapTwo(fa(0), fb(0))(f), ag.mapTwo(fa(1), fb(1))(f))

					def unit[A](a: => A): (F[A], G[A]) =
						(self.unit(a), ag.unit(a))

			def compose[G[_]](using ag: Applicative[G]): Applicative[[x] =>> F[G[x]]] =
				new Applicative[[x] =>> F[G[x]]]:
					def mapTwo[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
						self.mapTwo(fa, fb)((ga, gb) => ag.mapTwo(ga, gb)(f))

					def unit[A](a: => A): F[G[A]] =
						self.unit(ag.unit(a))

	// Traversable
	trait Traverse[F[_]] extends Functor[F] with Foldable[F]:
		def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using Applicative[G]): G[F[B]] =
			sequence(map(fa)(f))

		def sequence[G[_], A](fga: F[G[A]])(using Applicative[G]): G[F[A]] =
			traverse(fga)(identity)

	// Monad
	trait Monad[F[_]] extends Applicative[F]:
		def unit[A](a: => A): F[A]

		def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

		def mapTwo[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
			flatMap(fa)(a => map(fb)(b => f(a, b)))

		override def map[A, B](fa: F[A])(f: A => B): F[B] =
			flatMap(fa)(a => unit(f(a)))

		override def sequence[A](lma: List[F[A]]): F[List[A]] =
			traverse(lma)(identity)

		override def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
			la.foldRight(unit(List.empty[B])):
				(a, acc) => mapTwo(f(a), acc)(_ :: _)

		override def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
			sequence(List.fill(n)(ma))

		override def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
			mapTwo(ma, mb)((_, _))

		def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
			ms.foldRight(unit(List.empty[A])):
				(a: A, acc: F[List[A]]) =>
					flatMap(f(a))((b: Boolean) => if b then map(acc)(a :: _) else acc)

		def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
			a => flatMap(f(a))(b => g(b))

		def join[A](mma: F[F[A]]): F[A] =
			flatMap(mma)(identity)

	object Monad:
		extension [F[_], A](self: F[A])(using M: Monad[F])
			def flatMap[B](f: A => F[B]): F[B] =
				M.flatMap(self)(f)

			def map[B](f: A => B): F[B] =
				M.map(self)(f)

	trait MonadThrow[F[_]] extends Monad[F]:
		import scala.util.{Try, Failure, Success}

		def raiseError[A](t: Throwable): F[A]

		extension[A] (self: F[A])
			def attempt: F[Try[A]]

			def handleErrorWith(h: Throwable => F[A]): F[A] =
				flatMap(attempt):
					case Failure(t) => h(t)
					case Success(a) => unit(a)
