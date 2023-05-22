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

	object Rand:
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
		def mapTwo[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
			rng =>
				val (a, r1) = ra(rng)
				val (b, r2) = rb(r1)

				(f(a, b), r2)

		def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
			mapTwo(ra, rb)((_, _))

		def randIntDouble(int: Rand[Int], double: Rand[Double]): Rand[(Int, Double)] =
			both(int, double)

		def randDoubleInt(double: Rand[Double], int: Rand[Int]): Rand[(Double, Int)] =
			both(double, int)

		// Exercise 6.7
		def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
			fs.foldRight(unit(List.empty[A]))((r, acc) => mapTwo(r, acc)(_ :: _))

		def sequenceViaFoldLeft[A](fs: List[Rand[A]]): Rand[List[A]] =
			map(fs.foldLeft(unit(List.empty[A]))((acc, r) => mapTwo(r, acc)(_ :: _)))(_.reverse)

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
			flatMap(nonNegativeInt):
				i =>
					val mod = i % n
					if i + n - 1 - mod >= 0 then
						unit(mod)
					else
						nonNegativeLessThanViaFlatMap(n)

		// Exercise 6.9
		def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
			flatMap(s)(a => unit(f(a)))

		def mapTwoViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
			flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

		def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

	// Exercise 6.10
	opaque type State[S, +A] = S => (A, S)

	object State:
		extension [S, A](underlying: State[S, A])
			def run(s: S): (A, S) = underlying(s)

			def map[B](f: A => B): State[S, B] =
				s =>
					val (as, ss) = underlying.run(s)

					(f(as), ss)

			def map2[B](f: A => B): State[S, B] =
				flatMap(a => unit(f(a)))

			def mapTwo[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
				s =>
					val (a, sa) = underlying.run(s)
					val (b, sb) = rb.run(sa)

					(f(a, b), sb)

			def mapTwo2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
				for
					a <- underlying
					b <- rb
				yield
					f(a, b)

			def flatMap[B](g: A => State[S, B]): State[S, B] =
				s =>
					val (fa, fs) = underlying.run(s)
					val (ga, gs) = g(fa).run(fs)

					(ga, gs)

			// As per p. 90
		def modify[S](f: S => S): State[S, Unit] =
			for
				s <- get
				_ <- set(f(s))
			yield ()

		def get[S]: State[S, S] =
			s => (s, s)

		def set[S](s: S): State[S, Unit] =
			_ => ((), s)

		def unit[S, A](a: A): State[S, A] =
			s => (a, s)

		def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] =
			fs.foldRight(unit(List.empty[A])):
				(s, acc) => s.mapTwo(acc)(_ :: _)

	// Playing with the example on p. 89
	import Chapter6.State._

	type Rand2[+A] = State[RNG, A]

	private val int: Rand2[Int] =
		rng => Rand.nonNegativeLessThan(10)(rng)

	val ns: Rand2[List[Int]] =
		int.flatMap(x =>
			int.flatMap(y =>
				Rand.ints(x).map(xs =>
					xs.map(_ % y))))

	val ns2: Rand2[List[Int]] =
		for
			x <- int
			y <- int
			xs <- Rand.ints(x)
		yield
			xs.map(_ % y)

	pprint.log(ns2(SimpleRNG(42)))

	// Exercise 6.11
	enum Input:
		case Coin, Turn

	case class Machine(locked: Boolean, candies: Int, coins: Int)

	object Machine:
		def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
			for
				_ <- sequence(inputs.map(i => modify(updateMachine(i))))
				s <- get
			yield
				(s.coins, s.candies)

		def simulateMachine3(inputs: List[Input]): State[Machine, (Int, Int)] =
			m =>
				val result = inputs.foldRight(m):
					(i, m) => updateMachine(i)(m)

				((result.coins, result.candies), result)

		def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] =
			m: Machine =>
				val result = inputs.foldLeft(m):
					(m, i) => updateMachine(i)(m)

				((result.coins, result.candies), result)

		private def updateMachine(input: Input)(state: Machine): Machine =
			(input, state) match
				case (_, Machine(_, 0, _)) => state
				case (Input.Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
				case (Input.Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
				case _ => state
