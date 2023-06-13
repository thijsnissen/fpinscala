object Chapter8 extends App:
	// Exercise 8.1
	// val randomIntList = Gen.ListOf(Gen.choose(0, 100))
	// val constantIntList = Gen.ListOf(Gen.choose(10, 10))
	// val sequenceIntList = Gen.ListOf(gen.range(1, 100))
	//
	// val prop =
	//  forAll(List.empty)(ns => ns.sum == 0)
	// 	forAll(intList)(ns => ns.reverse.sum == ns.sum) &&
	// 	forAll(constantIntList)(ns => ns.length * 10 == ns.sum) &&
	// 	forAll(sequenceIntList)(ns => ns.length * (ns.length + 1) / 2 == ns.sum) &&
	// 	forAll(intList):
	// 		ns =>
	// 			val n = ns.splitAt(ns.length / 2)
	//
	// 			n._1.sum + n._2.sum == ns.sum

	// Exercise 8.2
	// val prop =
	// 	forAll(intList)(ns => ns.max == x)
	// 	forAll(intList)(ns => ns <= ns.max)
	// 	ForAll(List(10))(ns => ns.max == 10)

	import Chapter6.Rand
	import Chapter6.State
	import Chapter6.RNG

	case class Gen[A](sample: State[RNG, A]):
		// Exercise 8.6
		def flatMap[B](f: A => Gen[B]): Gen[B] =
			Gen(sample.flatMap(a => f(a).sample))

		def listOfNViaFlatMap(size: Gen[Int]): Gen[List[A]] =
			size.flatMap(n => Gen.listOfN(n, this))

	object Gen:
		def listOf[A](a: Gen[A]): Gen[List[A]] =
			listOfN(3, a)

		def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
			???

		// Exercise 8.4
		def choose(start: Int, stopExclusive: Int): Gen[Int] =
			Gen(State(Rand.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

		def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
			val test = (rng: RNG) =>
				val (a, b) = Rand.nonNegativeInt(rng)
				val (c, d) = Rand.nonNegativeInt(b)

				((a, c), d)

			Gen(State(test).map(n => (start + n._1 % (stopExclusive - start), start + n._2 % (stopExclusive - start))))

		// Exercise 8.5
		def unit[A](a: => A): Gen[A] =
			Gen(State.unit(a))

		def boolean: Gen[Boolean] =
			Gen(State(Rand.boolean))

		def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
			Gen(State.sequence(List.fill(n)(g.sample)))

		// Exercise 8.7
		def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
			boolean.flatMap(b => if b then g1 else g2)

		// Exercise 8.8
		def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
			val (ga, wa) = g1
			val (gb, wb) = g2

			val treshold = wa.abs / (wa.abs + wb.abs)
			Gen(State(Rand.double)).flatMap(d => if d <= treshold then ga else gb)

	import Prop._

	trait Prop:
		self =>
			def check: Either[(FailedCase, SuccessCount), SuccessCount]

			// Exercise 8.3
			// def &&(that: Prop): Prop =
			// 	new Prop:
			// 		def check: Boolean =
			// 			self.check && that.check

	object Prop:
		opaque type SuccessCount = Int

		opaque type FailedCase = String
