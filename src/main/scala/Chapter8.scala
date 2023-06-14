import Chapter6.SimpleRNG
import Chapter8.Prop.Result.Falsified

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

	// case class Gen[A](sample: State[RNG, A]):
	// 	// Exercise 8.6
	// 	def flatMap[B](f: A => Gen[B]): Gen[B] =
	// 		Gen(sample.flatMap(a => f(a).sample))
	//
	// 	def listOfNViaFlatMap(size: Gen[Int]): Gen[List[A]] =
	// 		size.flatMap(n => Gen.listOfN(n, this))

	// Exercise 8.3
	// def &&(that: Prop): Prop =
	// 	new Prop:
	// 		def check: Boolean =
	// 			self.check && that.check

	import Chapter6.Rand
	import Chapter6.State
	import Chapter6.RNG
	import SGen._

	object Gen:
		opaque type Gen[+A] = State[RNG, A]

		def listOf[A](a: Gen[A]): Gen[List[A]] =
			listOfN(3, a)

		// Exercise 8.4
		def choose(start: Int, stopExclusive: Int): Gen[Int] =
			State(Rand.nonNegativeInt).map(n => start + n % (stopExclusive - start))

		def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
			val test = (rng: RNG) =>
				val (a, b) = Rand.nonNegativeInt(rng)
				val (c, d) = Rand.nonNegativeInt(b)

				((a, c), d)

			State(test).map(n => (start + n._1 % (stopExclusive - start), start + n._2 % (stopExclusive - start)))

		// Exercise 8.5
		def unit[A](a: => A): Gen[A] =
			State.unit(a)

		def boolean: Gen[Boolean] =
			State(Rand.boolean)

		def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
			State.sequence(List.fill(n)(g))

		// Exercise 8.7
		def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
			boolean.flatMap(b => if b then g1 else g2)

		// Exercise 8.8
		def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
			val (ga, wa) = g1
			val (gb, wb) = g2

			val treshold = wa.abs / (wa.abs + wb.abs)

			State(Rand.double).flatMap(d => if d <= treshold then ga else gb)

		extension [A](self: Gen[A])
			// Exercise 8.6
			def flatMap[B](f: A => Gen[B]): Gen[B] =
				State.flatMap(self)(a => f(a))

			def listOfNViaFlatMap(size: Gen[Int]): Gen[List[A]] =
				size.flatMap(n => Gen.listOfN(n, self))

			def run(rng: RNG): (A, RNG) =
				State.run(self)(rng)

			// Exercise 8.10
			def unsized: SGen[A] =
				SGen.fromGen(self)

	import Gen._
	import SGen._

	object Prop:
		opaque type Prop = (MaxSize, TestCases, RNG) => Result

		opaque type SuccessCount = Int

		opaque type FailedCase = String

		opaque type MaxSize = Int

		object MaxSize:
			extension (m: MaxSize)
				def toInt: Int = m

			def fromInt(m: Int): MaxSize = m

		opaque type TestCases = Int

		object TestCases:
			extension (t: TestCases)
				def toInt: Int = t

			def fromInt(t: Int): TestCases = t

		enum Result:
			case Passed
			case Falsified(failure: FailedCase, successes: SuccessCount)

			def isFalsified: Boolean = this match
				case Passed => false
				case Falsified(_, _) => true

		def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
			(_, n, rng) =>
				randomStream(as)(rng).zip(LazyList.from(0)).take(n).map:
					case (a, i) =>
						try
							if (f(a)) Result.Passed else Result.Falsified(a.toString, i)
						catch
							case e: Exception => Result.Falsified(buildMsg(a, e), i)
				.find(_.isFalsified).getOrElse(Result.Passed)

		@annotation.targetName("forAllSized")
		def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
			(max, n, rng) =>
				val casesPerSize = (n + (max - 1)) / max

				val props: LazyList[Prop] =
					LazyList.from(0).take(math.min(n, max) + 1).map:
						i => forAll(g.toGen(i))(f)

				val prop: Prop =
					props.map[Prop]:
						p => (max, _, rng) => p.check(max, casesPerSize, rng)
					.toList
					.reduce(_ && _)

				prop.check(max, n, rng)

		def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
			LazyList.unfold(rng)(rng => Some(g.run(rng)))

		def buildMsg[A](s: A, e: Exception): String =
			s"test case: ${s}\n" +
			s"generated an exception: ${e.getMessage}\n" +
			s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

		extension (self: Prop)
			def run(
				m: MaxSize = 100,
				n: TestCases = 100,
				rng: RNG = SimpleRNG(System.currentTimeMillis)
			): Unit =
				self.check(m, n, rng) match
					case Result.Falsified(msg, c) =>
						println(s"${Console.RED}! Falsified after $c passes tests:\n $msg${Console.RESET}")
					case Result.Passed =>
						println(s"${Console.GREEN}+ OK, passed $n tests.${Console.RESET}")

			def check(
				m: MaxSize = 100,
				n: TestCases = 100,
				rng: RNG = SimpleRNG(System.currentTimeMillis)
			): Result =
				self(m, n, rng)

			@annotation.targetName("and")
			def &&(that: Prop): Prop =
				(m, n, rng) => self.tag("and-left").check(m, n, rng) match
					case Result.Passed => that.tag("and-right").check(m, n, rng)
					case falsified => falsified

			@annotation.targetName("or")
			def ||(that: Prop): Prop =
				(m, n, rng) => self.tag("or-left").check(m, n, rng) match
					case Result.Falsified(msg, _) => that.tag("or-right").tag(msg).check(m, n, rng)
					case passed => passed

			def tag(msg: FailedCase): Prop =
				(m, n, rng) => self.check(m, n, rng) match
					case Result.Falsified(fc, sc) => Result.Falsified(s"$msg \n $fc", sc)
					case passed => passed

	object SGen:
		opaque type SGen[+A] = Int => Gen[A]

		def fromGen[A](g: Gen[A]): SGen[A] =
			(_: Int) => g

		def unit[A](a: => A): SGen[A] =
			(_: Int) => Gen.unit(a)

		// Exercise 8.13
		def listOf[A](g: Gen[A]): SGen[List[A]] =
			(n: Int) => Gen.listOfN(n, g)

		extension [A](self: SGen[A])
			// Exercise 8.11
			def map[B](f: A => B): SGen[B] =
				self.flatMap(a => unit(f(a)))

			def flatMap[B](f: A => SGen[B]): SGen[B] =
				(n: Int) => self(n).flatMap(a => f(a)(n))

			def run(n: Int, rng: RNG): (A, RNG) =
				self(n).run(rng)

			def toGen(n: Int): Gen[A] =
				self(n)
