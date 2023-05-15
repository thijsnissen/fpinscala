object Chapter6 extends App:
	trait RNG:
		def nextInt: (Int, RNG)

	case class SimpleRNG(seed: Long) extends RNG:
		def nextInt: (Int, RNG) =
			val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
			val nextRNG = SimpleRNG(newSeed)
			val n = (newSeed >>> 16).toInt

			(n, nextRNG)

	type Rand[+A] = RNG => (A, RNG)

	def unit[A](a: A): Rand[A] =
		rng => (a, rng)

	def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
		rng =>
			val (a, rng2) = s(rng)

			(f(a), rng2)

	def nonNegativeEven: Rand[Int] =
		map(nonNegativeInt)(i => i - i % 2)

	// Exercise 6.1
	@annotation.tailrec
	def nonNegativeIntThijs(rng: RNG): (Int, RNG) =
		rng.nextInt match
			case (n, r) if n < 0 => nonNegativeIntThijs(r)
			case (n, r)          => (n, r)

	def nonNegativeInt(rng: RNG): (Int, RNG) =
		rng.nextInt match
			case (n, r) if n < 0 => (-1 * (n + 1), r)
			case (n, r) => (n, r)

	// Exercise 6.2
	def double(rng: RNG): (Double, RNG) =
		val (n, r) = nonNegativeInt(rng)

		(n.toDouble / (Int.MaxValue.toDouble + 1), r)

	// Exercise 6.3
	def intDouble(rng: RNG): ((Int, Double), RNG) =
		val (n1, r1) = rng.nextInt
		val (n2, r2) = double(r1)

		((n1, n2), r2)

	def doubleInt(rng: RNG): ((Double, Int), RNG) =
		val ((i, d), r) = intDouble(rng)

		((d, i), r)

	def doubleThree(rng: RNG): ((Double, Double, Double), RNG) =
		val (n1, r1) = double(rng)
		val (n2, r2) = double(r1)
		val (n3, r3) = double(r2)

		((n1, n2, n3), r3)

	// Exercise 6.4
	def ints(count: Int)(rng: RNG): (List[Int], RNG) =
		@annotation.tailrec
		def go(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) =
			if c <= 0 then
				(acc, r)
			else
				val (ns, rs) = r.nextInt

				go(c - 1, rs, ns :: acc)

		go(count, rng, List.empty[Int])

	// Exercise 6.5
	def double2: Rand[Double] =
		map(nonNegativeInt)(_.toDouble / (Int.MaxValue.toDouble + 1))

	// Exercide 6.6
	def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
		rng =>
			val (a, r1) = ra(rng)
			val (b, r2) = rb(r1)

			(f(a, b), r2)

	def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
		map2(ra, rb)((_, _))

	def randIntDouble(int: Rand[Int], double: Rand[Double]): Rand[(Int, Double)] =
		both(int, double)

	def randDoubleInt(double: Rand[Double], int: Rand[Int]): Rand[(Double, Int)] =
		both(double, int)

	// Exercise 6.7
	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
		fs.foldRight(unit(List.empty[A]))((r, acc) => map2(r, acc)(_ :: _))

	def sequenceViaFoldLeft[A](fs: List[Rand[A]]): Rand[List[A]] =
		map(fs.foldLeft(unit(List.empty[A]))((acc, r) => map2(r, acc)(_ :: _)))(_.reverse)

	def ints2(count: Int): Rand[List[Int]] =
		sequence(List.fill(count)(_.nextInt))

	def nonNegativeLessThan(n: Int): Rand[Int] =
		rng =>
			val (i, rng2) = nonNegativeInt(rng)
			val mod = i % n

			if i + n - 1 - mod >= 0 then
				(mod, rng2)
			else
				nonNegativeLessThan(n)(rng2)

	// Exercise 6.8
	def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
		rng =>
			val (a, rng2) = f(rng)

			g(a)(rng2)

	def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
		???

	// Exercise 6.9
	def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
		???

	def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
		???
