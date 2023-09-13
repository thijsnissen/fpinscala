import Chapter11.optionMonad
import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite:
	test("Chapter 2"):
		import Chapter2._

		assertResult(expected = 120)(factorial(5))

		// Exercise 2.1
		assertResult(expected = 3)(fibonacci(4))

		// Exercise 2.2
		assertResult(expected = true)(isSorted(Array(1, 2, 3, 4), (a, b) => a < b))

		assertResult(expected = true)(cos(2) == cos2 && cos2 == cos3)

	test("Chapter 3"):
		import Chapter3._
		import Chapter3.List._
		import Chapter3.Tree._

		assertResult(expected = 6)(sum(List(1, 2, 3)))
		assertResult(expected = 6.0)(product(List(2.0, 3.0)))
		assertResult(expected = 6)(sum2(List(1, 2, 3)))
		assertResult(expected = 6.0)(product2(List(2.0, 3.0)))

		// Exercise 3.1
		assertResult(expected = 3)(x)

		// Exercise 3.2
		assertResult(List(2, 3))(tail(List(1, 2, 3)))

		// Exercise 3.3
		assertResult(List(6, 2, 3))(setHead(List(1, 2, 3), 6))

		// Exercise 3.4
		assertResult(List(3))(drop(List(1, 2, 3), 2))

		// Exercise 3.5
		assertResult(List(4, 5))(dropWhile(List(1, 2, 3, 4, 5), _ <= 3))
		assertResult(List(4, 5))(dropWhile2(List(1, 2, 3, 4, 5))(_ <= 3))

		// Exercise 3.6
		assertResult(List(1, 2, 3))(init(List(1, 2, 3, 4)))

		// Exercise 3.8
		assertResult(List(1, 2, 3))(foldRight(List(1, 2, 3), Nil: List[Int])((n, z) => Cons(n, z)))

		// Exercise 3.9
		assertResult(5)(length(List(0, 1, 2, 3, 4)))

		// Exercise 3.10
		assertResult(6)(foldLeft(List(1, 2, 3), acc = 0)((n, acc) => acc + n))

		// Excersise 3.11
		assertResult(expected = 6)(sum3(List(1, 2, 3)))
		assertResult(expected = 6)(product3(List(1, 2, 3)))
		assertResult(expected = 5)(length2(List(0, 1, 2, 3, 4)))

		// Excersise 3.12
		assertResult(List(5, 4, 3, 2, 1))(reverse(List(1, 2, 3, 4, 5)))

		// Exercise 3.13
		assertResult(expected = 10)(foldRightViaFoldLeft(List(1, 2, 3, 4), acc = 0)((n, acc) => acc + n))
		assertResult(expected = 10)(foldLeftViaFoldRight(List(1, 2, 3, 4), acc = 0)((n, acc) => acc + n))

		// Excersise 3.14
		assertResult(List(1, 2, 3, 4))(append2(List(1, 2), List(3, 4)))

		// Exercise 3.15
		assertResult(List(1, 2, 3, 4, 5, 6))(concat(List(List(1, 2), List(3, 4), List(5, 6))))
		assertResult(List(1, 2, 3, 4, 5, 6))(concat2(List(List(1, 2), List(3, 4), List(5, 6))))

		// Exercise 3.16
		assertResult(List(2, 3, 4))(incrementBy1(List(1, 2, 3)))

		// Exercise 3.17
		assertResult(List("2.0", "3.0", "4.0"))(doubleToString(List(2.0, 3.0, 4.0)))

		// Exercise 3.18
		assertResult(List(2, 3, 4))(List.map(List(1, 2, 3))(_ + 1))

		// Exercise 3.19
		assertResult(List(2, 4))(filter(List(1, 2, 3, 4))(_ % 2 == 0))

		// Exercise 3.20
		assertResult(List(1, 1, 2, 2, 3, 3))(flatMap(List(1, 2, 3))(x => List(x, x)))

		// Exercise 3.21
		assertResult(List(2, 4))(filterViaFlatmap(List(1, 2, 3, 4))(_ % 2 == 0))

		// Exercise 3.22
		assertResult(List(5, 7, 9))(zipSum(List(1, 2, 3), List(4, 5, 6)))

		// Exercise 3.23
		assertResult(List("Thijs is 31", "Fleur is 30"))(zipWith(List("Thijs", "Fleur"), List(31, 30))((a, b) => a + " is " + b.toString))

		// Exercise Take
		assertResult(List(1, 2, 3))(take(List(1, 2, 3, 4, 5), 3))

		// Exercise 3.24
		assertResult(expected = true)(hasSubsequence(List(1, 2, 3), List(1, 2)))
		assertResult(expected = false)(hasSubsequence(List(1, 2, 3), List(4, 5)))

		// Exercise 3.25
		assertResult(expected = 7)(size(aTree))

		// Exercise 3.26
		assertResult(expected = 9)(maximum(aTreeOfInt))

		// Exercise3.27
		assertResult(expected = 3)(depth(aTreeOfInt))

		// Exercise 3.28
		assertResult(maximum(aTreeOfInt) + 1)(maximum(Tree.map(aTreeOfInt)(_ + 1)))

		// Exercise 3.29
		assertResult(expected = 7)(sizeViaFold(aTree))
		assertResult(expected = 9)(maximumViaFold(aTreeOfInt))
		assertResult(expected = 3)(depthViaFold(aTreeOfInt))
		assertResult(maximumViaFold(aTreeOfInt) + 1)(maximumViaFold(mapViaFold(aTreeOfInt)(_ + 1)))

		// Example to show that List is not lazy evaluated as opposed to Stream (chapter 3/chapter 5).
		assertResult(List(2, 3, 4))(filter(List.map(List(1, 2, 3, 4, 5))(plusOne))(smallerThan))

	test("Chapter 4"):
		import Chapter4._
		import Chapter4.Option._
		import Chapter4.Either._

		// Excercise 4.1
		assertResult(expected = true)(Some(1).map(_ + 1).contains(2))
		assertResult(expected = true)(Some(1).flatMap(x => Some(x + 1)).contains(2))
		assertResult(expected = 3)(None.getOrElse(3))
		assertResult(expected = true)(None.orElse(Some(2)).contains(2))
		assertResult(expected = true)(Some(3).filter(_ < 5).contains(3))

		// Exercise 4.2
		assertResult(Some(2))(Option.mean(Seq(1.0, 2.0, 3.0)))
		assertResult(Some(0.6666666666666666))(variance(Seq(1.0, 2.0, 3.0)))

		assertResult(Some(3))(absO(Some(-3)))

		// Exercise 4.3
		assertResult(Some(8))(mapTwo(Some(2), Some("4"))((x, y) => x * y.toInt))
		assertResult(Some(8))(mapTwo2(Some(2), Some("4"))((x, y) => x * y.toInt))

		// Exercise 4.4
		assertResult(Some(List(1, 2, 3)))(Option.sequence(List(Some(1), Some(2), Some(3))))
		assertResult(Some(List(1, 2, 3)))(Option.sequence2(List(Some(1), Some(2), Some(3))))

		// Exercise 4.5
		assertResult(Some(List("1", "2", "3")))(Option.traverse(List(1, 2, 3))(x => Some(x.toString)))
		assertResult(Some(List(1, 2, 3)))(sequence3(List(Some(1), Some(2), Some(3))))

		assertResult(Some("HR"))(lookupByName(name = "Joe").map(_.department))
		assertResult(Some("Fleur"))(lookupByName(name = "Joe").flatMap(_.manager))
		assertResult(expected = "HR")(lookupByName(name = "Joe").map(_.department).getOrElse(default = "Default Dept."))
		assertResult(expected = "Manager NOT Marco")(lookupByName(name = "Joe").map(_.manager).filter(_ == Some("Marco")).getOrElse(default = "Manager NOT Marco"))

		assertResult(Some(1420.0))(parseInsuranceRateQuote("31", "3"))

		// Exercise 4.6
		assertResult(Right(2))(Right(1).map(_ + 1))
		assertResult(Right(2))(Right(1).flatMap(x => Right(x + 1)))
		assertResult(Right(1))(Either.Try(3 / 0).orElse(Right(1)))
		assertResult(Right(3))(Right(1).mapTwo(Right(2))(_ + _))

		assertResult(Right(2.0))(Either.mean(IndexedSeq(1.0, 2.0, 3.0)))

		assertResult(Right(5))(safeDiv(10, 2))
		assertResult(expected = true)(safeDiv(10, 0).isLeft)

		// Exercise 4.7
		assertResult(Right(List(1, 2, 3)))(Either.sequence(List(Right(1), Right(2), Right(3))))
		assertResult(Right(List(1, 2, 3)))(Either.sequence2(List(Right(1), Right(2), Right(3))))
		assertResult(Right(List(2, 3, 4)))(Either.traverse(List(1, 2, 3))(x => Right(x + 1)))

		assertResult(Right(1420.0))(parseInsuranceRateQuote2("31", "3"))

	test("Chapter 5"):
		import Chapter5._
		import Chapter5.Stream._

		assertResult(Some(1))(Stream(1, 2, 3).headOption)

		// Exercise 5.1
		assertResult(List(1, 2, 3))(Stream(1, 2, 3).toList)
		assertResult(List(1, 2, 3))(Stream(1, 2, 3).toListRecursive)

		// Exercise 5.2
		assertResult(List(1, 2))(Stream(1, 2, 3).take(2).toList)
		assertResult(List(3))(Stream(1, 2, 3).drop(2).toList)

		// Exercise 5.3
		assertResult(List(1, 2, 3))(Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList)

		assertResult(expected = true)(Stream(1, 2, 3, 4, 5).exists(_ > 4))
		assertResult(expected = true)(Stream(1, 2, 3, 4, 5).existsViaFoldRight(_ > 4))

		assertResult(expected = 10)(Stream(1, 2, 3, 4).foldRight(0)((a, b) => a + b))

		// Exercise 5.4
		assertResult(expected = false)(Stream(1, 2, 3, 4, 5).forAll(_ < 4))
		assertResult(expected = false)(Stream(1, 2, 3, 4, 5).forAllViaFoldRight(_ < 4))

		// Exercise 5.5
		assertResult(List(1, 2, 3))(Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ < 4).toList)

		// Exercise 5.6
		assertResult(Some(1))(Stream(1, 2, 3).headOptionViaFoldRight)

		// Exercise 5.7
		assertResult(Stream(2, 3, 4).toList)(Stream(1, 2, 3).map(_ + 1).toList)
		assertResult(Stream(1, 2).toList)(Stream(1, 2, 3).filter(_ != 3).toList)
		assertResult(Stream(1, 2, 3, 4).toList)(Stream(1, 2).append(Stream(3, 4)).toList)
		assertResult(Stream(2, 3, 4).toList)(Stream(1, 2, 3).flatMap(x => Stream(x + 1)).toList)

		assertResult(Some(2))(Stream(1, 2, 3).find(_ == 2))

		// Exercise 5.13
		assertResult(List(2, 3, 4))(Stream(1, 2, 3).mapViaUnfold(_ + 1).toList)
		assertResult(List(1, 2))(Stream(1, 2, 3).takeViaUnfold(2).toList)
		assertResult(List(1, 2, 3))(Stream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ < 4).toList)

		assertResult(List("Thijs Nissen", "Fleur van Beek"))(Stream("Thijs", "Fleur").zipWith(Stream("Nissen", "van Beek"))((a, b) => a + " " + b).toList)
		assertResult(List((Some("Thijs"), Some("Nissen")), (Some("Fleur"), Some("van Beek"))))(Stream("Thijs", "Fleur").zipAll(Stream("Nissen", "van Beek")).toList)

		assertResult(expected = true)(Stream(1, 2, 3, 4, 5).hasSubsequence(Stream(2, 3)))
		assertResult(expected = true)(Stream(1, 2, 3, 4, 5).hasSubsequence2(Stream(2, 3)))

		// Exercise 5.14
		assertResult(expected =true)(Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3)))
		assertResult(expected =true)(Stream(1, 2, 3, 4, 5).startsWith2(Stream(1, 2, 3)))

		// Exercise 5.15
		assertResult(List(List(1, 2, 3), List(2, 3), List(3)))(Stream(1, 2, 3).tails.map(_.toList).toList)

		// Exercise 5.16
		assertResult(List(6, 5, 3, 0))(Stream(1, 2, 3).scanRight(0)(_ + _).toList)

		// Example to show that List is not lazy evaluated as opposed to Stream (chapter 3/chapter 5).
		assertResult(List(2, 3, 4))(List(1, 2, 3, 4, 5).map(plusOne).filter(smallerThan))
		assertResult(List(2, 3, 4))(Stream(1, 2, 3, 4, 5).map(plusOne).filter(smallerThan).toList)

	test("Chapter 6"):
		import Chapter6._
		import Chapter6.Rand._

		val rng = SimpleRNG(42) // used only to invoke test state

		// Exercise 6.1
		assertResult(16159453)(nonNegativeInt(rng)._1)

		// Exercise 6.2
		assertResult(0.007524831686168909)(double(rng)._1)

		// Exercise 6.3
		assertResult((16159453, 0.5967354848980904))(intDouble(rng)._1)
		assertResult((0.5967354848980904, 16159453))(doubleInt(rng)._1)
		assertResult((0.007524831686168909, 0.5967354848980904, 0.15846728393808007))(doubleThree(rng)._1)

		// Exercise 6.4
		assertResult(List(1770001318, -2015756020, -340305902, -1281479697, 16159453))(ints(5)(rng)._1)

		assertResult(true)(unit("String").isInstanceOf[Rand[String]])
		assertResult(true)(nonNegativeEven(rng)._1 >= 0 && nonNegativeEven(rng)._1 % 2 == 0)

		assertResult(10)(map(unit("10"))(_.toInt)(rng)._1)

		// Exercise 6.5
		assertResult(0.007524831686168909)(double2(rng)._1)

		// Exercise 6.6
		assertResult(15)(mapTwo(unit("10"), unit("5"))((a, b) => a.toInt + b.toInt)(rng)._1)

		assertResult(true)(both(nonNegativeInt, double).isInstanceOf[Rand[(Int, Double)]])
		assertResult(true)(randIntDouble(unit(1), unit(1.0)).isInstanceOf[Rand[(Int, Double)]])
		assertResult(true)(randDoubleInt(unit(1.0), unit(1)).isInstanceOf[Rand[(Double, Int)]])

		// Exercise 6.7
		assertResult(List(1, 2, 3))(sequence(List(unit(1), unit(2), unit(3)))(rng)._1)
		assertResult(List(1, 2, 3))(sequenceViaFoldLeft(List(unit(1), unit(2), unit(3)))(rng)._1)
		assertResult(List(16159453, -1281479697, -340305902, -2015756020, 1770001318))(ints2(5)(rng)._1)

		assertResult(3)(nonNegativeLessThan(10)(rng)._1)

		// Exercise 6.8
		assertResult(3)(nonNegativeLessThanViaFlatMap(10)(rng)._1)

		// Exercise 6.9
		assertResult(10)(mapViaFlatMap(unit("10"))(_.toInt)(rng)._1)
		assertResult(15)(mapTwoViaFlatMap(unit("10"), unit("5"))((a, b) => a.toInt + b.toInt)(rng)._1)

		assertResult(2)(rollDie(rng)._1)

		//Exercise 6.10
		val state = State.unit("usedOnlyToInvokeTestState")

		assertResult(10)(State.unit(10).run(state)._1)
		assertResult(10)(State.unit("10").map(_.toInt).run(state)._1)
		assertResult(10)(State.unit("10").map2(_.toInt).run(state)._1)
		assertResult(15)(State.unit("10").mapTwo(State.unit("5"))((a, b) => a.toInt + b.toInt).run(state)._1)
		assertResult(15)(State.unit("10").mapTwo2(State.unit("5"))((a, b) => a.toInt + b.toInt).run(state)._1)
		assertResult(10)(State.unit("10").flatMap(a => State.unit(a.toInt)).run(state)._1)
		assertResult(List(1, 2, 3))(State.sequence(List(State.unit(1), State.unit(2), State.unit(3))).run(state)._1)

		// Playing with the example on p. 89
		assertResult(true)(Chapter6.ns.isInstanceOf[Rand2[List[Int]]])
		assertResult(true)(Chapter6.ns2.isInstanceOf[Rand2[List[Int]]])

		assertResult(List(4, -4, -2))(Chapter6.ns.run(rng)._1)
		assertResult(List(4, -4, -2))(Chapter6.ns2.run(rng)._1)

		// Exercise 6.11
		import Chapter6.Input._
		import Chapter6.Machine._

		// Example from the book
		assertResult((14, 1))(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10))._1)
		assertResult((14, 1))(simulateMachine2(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10))._1)

		// Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
		assertResult(false)(simulateMachine(List(Coin)).run(Machine(true, 1, 0))._2.locked)
		assertResult(false)(simulateMachine2(List(Coin)).run(Machine(true, 1, 0))._2.locked)

		// Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
		assertResult(true)(simulateMachine(List(Turn)).run(Machine(false, 1, 1))._2.locked)
		assertResult(0)(simulateMachine(List(Turn)).run(Machine(false, 1, 1))._2.candies)
		assertResult(true)(simulateMachine2(List(Turn)).run(Machine(false, 1, 1))._2.locked)
		assertResult(0)(simulateMachine2(List(Turn)).run(Machine(false, 1, 1))._2.candies)

		// Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
		assertResult(true)(simulateMachine(List(Turn)).run(Machine(true, 1, 0))._2.locked)
		assertResult(false)(simulateMachine(List(Coin)).run(Machine(false, 1, 1))._2.locked)
		assertResult(true)(simulateMachine2(List(Turn)).run(Machine(true, 1, 0))._2.locked)
		assertResult(false)(simulateMachine2(List(Coin)).run(Machine(false, 1, 1))._2.locked)

		// A machine that’s out of candy ignores all inputs.
		assertResult(true)(simulateMachine(List(Turn)).run(Machine(true, 0, 1))._2.locked)
		assertResult(true)(simulateMachine(List(Coin)).run(Machine(true, 0, 1))._2.locked)
		assertResult(true)(simulateMachine2(List(Turn)).run(Machine(true, 0, 1))._2.locked)
		assertResult(true)(simulateMachine2(List(Coin)).run(Machine(true, 0, 1))._2.locked)

	test("Chapter 7"):
		import Chapter7._
		import Chapter7.APIExplorations._

		// Exercise 7.1 & Exercise 7.2
		assertResult(15)(APIExplorations.sum1(Vector(1, 2, 3, 4, 5)))
		assertResult(APIExploration(15))(APIExplorations.sum2(Vector(1, 2, 3, 4, 5)))

		import Chapter7.Par._

		val executorService = java.util.concurrent.Executors.newFixedThreadPool(4)

		// Exercise 7.5
		assertResult(List(1, 2, 3, 4, 5))(sortPar(Par.lazyUnit(List(2, 4, 3, 5, 1))).run(executorService).get)
		assertResult(List(2, 3, 4, 5, 6))(parMap(List(1, 2, 3, 4, 5))(_ + 1).run(executorService).get)

		// Exercise 7.6
		assertResult(List(1, 2))(parFilter(List(1, 2, 3, 4, 5))(_ < 3).run(executorService).get)

		assertResult(15)(parallelCombination(Vector(1, 2, 3, 4, 5), 0)(identity)(_ + _).run(executorService).get)
		assertResult(4)(max(Vector(1, 2, 3, 4)).run(executorService).get)
		assertResult(10)(totalNoOfWords(List("Hi I am Thijs", "I love Scala", "Welcome to DHL")).run(executorService).get)

		// Exercise 7.9 -> Will not work when theadSize is <= 1.
		val a2 = Par.unit(42 + 1)
		assertResult(true)(equal(executorService)(a2, Par.fork(Par.fork(a2))))

		// Exercise 7.11
		assertResult("ViaTrue")(choice(Par.unit(true))(Par.unit("ViaTrue"), Par.unit("ViaFalse")).run(executorService).get)
		assertResult("ViaFalse")(choice2(Par.unit(false))(Par.unit("ViaTrue"), Par.unit("ViaFalse")).run(executorService).get)

		// Exercise 7.12 & exercise 7.13
		assertResult("ViaFalse")(choice3(Par.unit(false))(Par.unit("ViaTrue"), Par.unit("ViaFalse")).run(executorService).get)
		assertResult("Via2")(choiceN2(Par.unit(2))(List(Par.unit("Via0"), Par.unit("Via1"), Par.unit("Via2"))).run(executorService).get)
		assertResult("ViaOne")(choiceMap(Par.unit("ViaOne"))(Map("ViaZero" -> Par.unit("ViaZero"), "ViaOne" -> Par.unit("ViaOne"), "ViaTwo" -> Par.unit("ViaTwo"))).run(executorService).get)

		// Exercise 7.14
		assertResult(10)(Par.unit(5).flatMap(n => Par.unit(n * 2)).run(executorService).get)
		assertResult(10)(flatMapViaJoin(Par.unit(5))(n => Par.unit(n * 2)).run(executorService).get)
		assertResult(5)(joinViaFlatMap(Par.unit(5).map(Par.unit)).run(executorService).get)

		assertResult(10)(mapTwoViaFlatMapUnit(Par.unit(1 + 1), Par.unit(2 + 3))(_ * _).run(executorService).get)

		import Chapter7.nonBlocking._

		assertResult(100)(nonBlocking.Par.parMap((1 to 100).toList)(_ + 1).run(executorService).size)

		executorService.shutdown()

	test("Chapter 8"):
		import Chapter8.Gen
		import Chapter8.Gen._
		import Chapter8.Prop
		import Chapter8.Prop.TestCases
		import Chapter8.Prop.MaxSize
		import Chapter8.Prop.*

		val rng = Chapter6.SimpleRNG(42) // used only to invoke test state

		assertResult(List("Thijs", "Thijs", "Thijs"))(listOf(unit("Thijs")).run(rng)._1)

		// Exercise 8.4
		assertResult(true)(choose(0, 7).run(rng)._1 < 7)

		// Exercise 8.5
		assertResult("Thijs")(unit("Thijs").run(rng)._1)
		assertResult(true)(List(true, false).contains(boolean.run(rng)._1))
		assertResult(List("Thijs", "Thijs", "Thijs"))(listOfN(3, unit("Thijs")).run(rng)._1)

		// Exercise 8.6
		assertResult(List("Thijs", "Thijs", "Thijs"))(unit("Thijs").listOfNViaFlatMap(unit(3)).run(rng)._1)

		// Exercise 8.9
		val intList = Gen.listOf(Gen.choose(10, 100))

		val passedProp1 =
			Prop.forAll(intList)(ns => ns.reverse.reverse == ns) &&
			Prop.forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
		val falsifiedProp = Prop.forAll(intList)(ns => ns.reverse == ns)
		val passedProp2 = passedProp1 || falsifiedProp

		passedProp1.run("Exercise 8.9")
		falsifiedProp.run("Exercise 8.9")
		passedProp2.run("Exercise 8.9")

		assertResult(false)(passedProp1.check().isFalsified)
		assertResult(true)(falsifiedProp.check().isFalsified)
		assertResult(false)(passedProp2.check().isFalsified)

		// Exercise 8.12
		val smallInt = Gen.choose(-10, 10)

		val maxProp1 = Prop.forAll(Chapter8.SGen.listOf(smallInt)):
			(ns: List[Int]) =>
				val max = ns.max
				!ns.exists(_ > max)

		maxProp1.run("Exercise 8.12")

		assertResult(true)(maxProp1.check().isFalsified)

		// Exercise 8.13
		val maxProp2 = Prop.forAll(Chapter8.SGen.listOf1(smallInt)):
			(ns: List[Int]) =>
				val max = ns.max
				!ns.exists(_ > max)

		maxProp2.run("Exercise 8.13")

		assertResult(false)(maxProp2.check().isFalsified)

		// Exercise 8.14
		val sortedProp =
			Prop.forAll(Chapter8.SGen.listOf(smallInt))((ns: List[Int]) => ns.sorted == ns.reverse.sorted) &&
			Prop.forAll(Chapter8.SGen.listOf(smallInt))((ns: List[Int]) => ns.sorted.size == ns.size) &&
			Prop.forAll(Chapter8.SGen.listOf(smallInt))((ns: List[Int]) => ns.sorted.forall(ns.contains)) &&
			Prop.forAll(Chapter8.SGen.listOf1(smallInt)):
				(ns: List[Int]) =>
					val n = ns.sorted
					n.zip(n.tail).forall((a, b) => a <= b)

		sortedProp.run("Exercise 8.14")

		assertResult(false)(sortedProp.check().isFalsified)

		// Exercise 8.15
		val booleanProp =
			Prop.forAll(Gen.unit(true))((b: Boolean) => b) &&
			Prop.forAll(Gen.unit(false))((b: Boolean) => !b)

		booleanProp.run(name = "Exercise 8.15", m = MaxSize.fromInt(1), n = TestCases.fromInt(1))

		assertResult(false)(booleanProp.check(MaxSize.fromInt(1), TestCases.fromInt(1)).isFalsified)

		val charProp = forAll(Chapter8.SGen.listOf(Gen.char)):
			c => c.mkString.length == c.size

		charProp.run("Char test")

		assertResult(false)(charProp.check().isFalsified)

		import Chapter7.nonBlocking.Par

		val executor = java.util.concurrent.Executors.newFixedThreadPool(4)

		val parProp1 = Prop.forOne(Par.equal2(Par.unit(1).map(_ + 1), Par.unit(2)).run(executor))
		val parProp2 = Prop.forAll(smallInt):
			i => Par.equal2(Par.unit(i).map(_ + 1), Par.unit(i + 1)).run(executor)

		parProp1.run("Par test 1")
		parProp2.run("Par test 2")

		assertResult(false)(parProp1.check().isFalsified)
		assertResult(false)(parProp2.check().isFalsified)

		val pInt = Gen.choose(0, 10).map(Par.unit)
		val parProp3 = Prop.forAllPar(pInt):
			n => Par.equal2(n.map(identity), n)

		parProp3.run("Par test 3")

		assertResult(false)(parProp3.check().isFalsified)

		val pInt2 = Chapter8.SGen.listOf(Gen.choose(0, 20)).map:
			(l: List[Int]) => Par.unit(l)

		val parProp4 = Prop.forAllPar(pInt2):
			n => Par.equal2(n.map(identity), n)

		parProp4.run("Par test 4")

		assertResult(false)(parProp4.check().isFalsified)

		// Exercise 8.16
		val pInt3 = Chapter8.SGen.listOf(Gen.choose(0, 10)).map:
			l => l.foldLeft(Par.unit(0)):
				(acc, i) => Par.fork:
					acc.mapTwo(Par.fork(Par.unit(i)))(_ + _)

		// Exercise 8.17
		val parProp5 =
			Prop
				.forAllPar(pInt3):
					n => Par.equal2(Par.fork(n), n)
				.tag(Prop.FailedCase.fromString("fork"))

		parProp5.run("Exercise 8.17")

		assertResult(false)(parProp5.check().isFalsified)

		// Exercise 8.19
		val fn = Gen.genStringIntFn
		val fnProp = forAll(fn)(f => f("test") > 0)

		fnProp.run("Exercise 8.19")

		assertResult(false)(fnProp.check().isFalsified)

		val fn2 = Gen.genStringIntFn2(smallInt)
		val fnProp2 = forAll(fn2)(f => f("test").isInstanceOf[Int])

		fnProp2.run("Function test")

		assertResult(false)(fnProp2.check().isFalsified)

		import Chapter3.Tree
		import Chapter3.Tree._

		// Exercise 8.20
		val treeProp = Prop.forAll(Chapter8.Gen.treeOfN(6, smallInt)):
			(t: Tree[Int]) => Tree.size(t) == math.pow(2, Tree.depth(t)) - 1

		treeProp.run("Tree test unsized")

		assertResult(false)(treeProp.check().isFalsified)

		val treeProp2 = Prop.forAll(Chapter8.SGen.treeOf(smallInt)):
			(t: Tree[Int]) => Tree.size(t) == math.pow(2, Tree.depth(t)) - 1

		treeProp2.run(name = "Tree test sized", m = MaxSize.fromInt(10))

		assertResult(false)(treeProp2.check(MaxSize.fromInt(10)).isFalsified)

		executor.shutdown()

	test("Chapter 9"):
		import Chapter9._
		import Chapter9.Result._

		// Char
		assertResult(Right('c'))(Parser.char('c').run("c").extract)

		// String
		assertResult(Right("string"))(Parser.string("string").run("string").extract)

		// Or
		assertResult(Right("abra"))((Parser.string("abra") | Parser.string("cadabra")).run("abra").extract)
		assertResult(Right("cadabra"))((Parser.string("abra") | Parser.string("cadabra")).run("cadabra").extract)

		// zeroOrMore & oneOrMore
		assertResult(Right(3))(Parser.char('a').zeroOrMore.map(_.size).run("aaaba").extract)
		assertResult(Right(5))((Parser.char('a') | Parser.char('b')).oneOrMore.map(_.size).run("aaaba").extract)

		// Map
		assertResult(Right("structurePreserving"))(Parser.string("structurePreserving").map(identity).run("structurePreserving").extract)

		import Chapter8.Gen
		import Chapter8.Gen._
		import Chapter8.Prop
		import Chapter8.Prop._

		def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
			forAll(in)(s =>	p1.run(s) == p2.run(s))

		val mapLawProp = equal(
				Parser.string("structurePreserving"),
				Parser.string("structurePreserving").map(identity))(Gen.string(10)
			).check().isFalsified

		assertResult(false)(mapLawProp)

		// Succeed
		assertResult(Right("unit"))(Parser.succeed("unit").run("NotUnit").extract)

		// Times, Regex & Slice
		assertResult(Right("4aaaa"))(Parser.regex("[0-9]".r).flatMap(i => Parser.regex("[a-zA-Z]".r).times(i.toInt)).slice.run("4aaaa").extract)

		import Chapter9.JSON

		val jsonTxt = """
			|{
			|	"Company name" : "Microsoft Corporation",
			|	"Ticker"  : "MSFT",
			|	"Active"  : true,
			|	"Price"   : 30.66,
			|	"Shares outstanding" : 8.38e9,
			|	"Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
			|}""".stripMargin

		val malformedJson1 = """
			|{
			|	"Company name" ; "Microsoft Corporation"
			|}""".stripMargin

		val malformedJson2 ="""
			|[
			|	[ "HPQ", "IBM",
			|	"YHOO", "DELL" ++
			|	"GOOG"
			|	]
			|]""".stripMargin

		val timelyJson =
			"""{
				|  "github": "github",
				|  "team":   "#team",
				|  "name":   "name",
				|  "status": "active",
				|  "credit": {
				|    "Mon": 0,
				|    "Tue": 1,
				|    "Wed": 2,
				|    "Thu": 3,
				|    "Fri": 4,
				|    "Sat": 5,
				|    "Sun": 6
				|  },
				|  "debit": {
				|    "Mon": 0,
				|    "Tue": 1,
				|    "Wed": 2,
				|    "Thu": 3,
				|    "Fri": 4,
				|    "Sat": 5,
				|    "Sun": 6
				|  },
				|  "fee": "3000/month"
				|}
				|""".stripMargin

		println(JSON.json.run(jsonTxt))
		println(JSON.json.run(malformedJson1))
		println(JSON.json.run(malformedJson2))
		println(JSON.json.run(timelyJson))

	test("Chapter 10"):
		import Chapter10.Monoid

		import Chapter8.Gen
		import Chapter8.Gen.*
		import Chapter8.Prop
		import Chapter8.Prop.*
		import Chapter8.Prop.Result.*

		val stringMonoid: Monoid[String] = Monoid.stringMonoid
		val charMonoid: Monoid[Char] = Monoid.charMonoid
		val ListMonoid: Monoid[List[Int]] = Monoid.listMonoid[Int]
		val intAdditionMonoid: Monoid[Int] = Monoid.intAddition
		val intMultiplicationMonoit: Monoid[Int] = Monoid.intMultiplication
		val booleanOrMonoid: Monoid[Boolean] = Monoid.booleanOr
		val booleanAndMonoid: Monoid[Boolean] = Monoid.booleanAnd
		val firstOptionMonoid: Monoid[Option[Int]] = Monoid.firstOptionMonoid[Int]
		val lastMonoidOption: Monoid[Option[Int]] = Monoid.lastOptionMonoid[Int]
		//val combineOptionMonoid: Monoid[Option[Int]] = Monoid.combineOptionMonoid(_ + _)
		//val endoMonoid: Monoid[Int => Int] = Monoid.endoMonoid[Int]

		val stringGen: Gen[String] = Gen.string(10)
		val intGen: Gen[Int] = Gen.choose(10, 100)
		val listGen: Gen[List[Int]] = Gen.listOf(intGen)
		val booleanGen: Gen[Boolean] = Gen.boolean
		val optionGen: Gen[Option[Int]] = intGen.map(i => if i < 10 then None else Some(i))
		//val endoGen = intGen.map((a: Int) => (b: Int) => a + b)

		assertResult(Result.Passed)(Monoid.monoidLaws(stringMonoid, stringGen).check())
		assertResult(Result.Passed)(Monoid.monoidLaws(ListMonoid, listGen).check())

		// Exercise 10.1
		assertResult(Result.Passed)(Monoid.monoidLaws(intAdditionMonoid, intGen).check())
		assertResult(Result.Passed)(Monoid.monoidLaws(intMultiplicationMonoit, intGen).check())
		assertResult(Result.Passed)(Monoid.monoidLaws(booleanOrMonoid, booleanGen).check())
		assertResult(Result.Passed)(Monoid.monoidLaws(booleanAndMonoid, booleanGen).check())

		// Exercise 10.2
		assertResult(Result.Passed)(Monoid.monoidLaws(firstOptionMonoid, optionGen).check())
		assertResult(Result.Passed)(Monoid.monoidLaws(lastMonoidOption, optionGen).check())
		//assertResult(Result.Passed)(Monoid.monoidLaws(combineOptionMonoid, optionGen).check())

		// Exercise 10.3
		//assertResult(Result.Passed)(Monoid.monoidLaws(endoMonoid, endoGen).check())

		// Exercise 10.5
		val listChar   = List('a', 'b', 'c', 'd', 'e')
		val listString = List("a", "b", "c", "d", "e")
		val vectorChar = Vector('a', 'b', 'c', 'd', 'e')

		assertResult(Monoid.foldMap(listChar, Monoid.intAddition)(_.toInt)):
			Monoid.foldMapViaConcatenate(listChar, Monoid.intAddition)(_.toInt)

		// Exercise 10.6
		assertResult(Monoid.foldLeft(listChar, 0)((b, a) => a.toInt + b)):
			Monoid.foldRight(listChar, 0)((a, b) => a.toInt + b)

		assertResult(Monoid.foldLeft(listString, "")((b, a) => a + b)):
			Monoid.foldRight(listString, "")((a, b) => a + b)

		// Exercise 10.7
		assertResult(Monoid.foldMap(listChar, Monoid.intAddition)(_.toInt)):
			Monoid.foldMapV(vectorChar, Monoid.intAddition)(_.toInt)

		// Exercise 10.8
		val executorService = java.util.concurrent.Executors.newFixedThreadPool(4)

		assertResult(Monoid.foldMapV(vectorChar, Monoid.intAddition)(_.toInt)):
			Monoid.parFoldMap(vectorChar, Monoid.intAddition)(_.toInt).run(executorService)

		executorService.shutdown()

		// Exercise 10.9
		assertResult(true)(Monoid.isOrdered(Vector(1, 2, 3, 4, 5)))
		assertResult(true)(Monoid.isOrdered(Vector.empty[Int]))
		assertResult(true)(Monoid.isOrdered(Vector(1, 1, 2, 2, 2, 3, 4, 4)))
		assertResult(false)(Monoid.isOrdered(Vector(1, 2, 3, 2, 4, 9, 5)))

		// Exercise 10.10 & Exercise 10.11
		assertResult(5)(Monoid.wordCount("Lorem ipsum dolor sit amet, "))

		// Exercise 10.12
		import Monoid.Foldable.listFoldable

		val list1 = Chapter3.List(1, 2, 3, 4, 5)
		val list2 = List(1, 2, 3, 4, 5)

		assertResult("12345")(listFoldable.foldMap(list1, Monoid.stringMonoid)(_.toString))
		assertResult("12345")(listFoldable.foldLeft(list1, "")((b, a) => a.toString + b))
		assertResult("12345")(listFoldable.foldRight(list1, "")((a, b) => a.toString + b))
		assertResult(15)(listFoldable.concat(list1, Monoid.intAddition))
		assertResult(list2)(listFoldable.toList(list1))

		// Exercise 10.13
		import Chapter3.Tree
		import Chapter3.Tree.Leaf
		import Chapter3.Tree.Branch
		import Monoid.Foldable.treeFoldable

		val tree =
			Branch(
				Branch(
					Branch(
						Leaf(1), Leaf(2)
					),
					Leaf(3)
				),
				Branch(
					Leaf(4), Leaf(5)
				)
			)

		assertResult("12345")(treeFoldable.foldMap(tree, Monoid.stringMonoid)(_.toString))
		assertResult("12345")(treeFoldable.foldLeft(tree, "")((b, a) => a.toString + b))
		assertResult("12345")(treeFoldable.foldRight(tree, "")((a, b) => a.toString + b))
		assertResult(15)(treeFoldable.concat(tree, Monoid.intAddition))
		assertResult(list2)(treeFoldable.toList(tree))

		// Exercise 10.14
		import Chapter4.Option
		import Chapter4.Option.Some
		import Chapter4.Option.None
		import Monoid.Foldable.optionFoldable

		val option1 = Some(10)
		val option2 = None

		assertResult("10")(optionFoldable.foldMap(option1, Monoid.stringMonoid)(_.toString))
		assertResult("")(optionFoldable.foldMap(option2, Monoid.stringMonoid)(_.toString))
		assertResult("10")(optionFoldable.foldLeft(option1, "")((b, a) => a.toString + b))
		assertResult("")(optionFoldable.foldLeft(option2, "")((b, a) => a.toString + b))
		assertResult("10")(optionFoldable.foldRight(option1, "")((a, b) => a.toString + b))
		assertResult("")(optionFoldable.foldRight(option2, "")((a, b) => a.toString + b))
		assertResult(10)(optionFoldable.concat(option1, Monoid.intAddition))
		assertResult(0)(optionFoldable.concat(option2, Monoid.intAddition))
		assertResult(List(10))(optionFoldable.toList(option1))
		assertResult(Nil)(optionFoldable.toList(option2))

		// Exercise 10.16
		val productMonoid: Monoid[(String, Int)] =
			Monoid.productMonoid(stringMonoid, intAdditionMonoid)

		val stringIntGen: Gen[(String, Int)] =
			for
				s <- stringGen
				i <- intGen
			yield
				(s, i)

		assertResult(Result.Passed)(Monoid.monoidLaws(productMonoid, stringIntGen).check())

		val map1: Map[Int, String] = Map(1 -> "Thijs", 2 -> "Nissen")
		val map2: Map[Int, String] = Map(3 -> "Koen", 4 -> "Van Den", 5 -> "Bergh")
		val map3: Map[Int, String] = Map(1 -> "Thijs", 2 -> "Nissen", 3 -> "Koen", 4 -> "Van Den", 5 -> "Bergh")

		val map4: Map[Char, Map[Int, String]] = Map('a' -> Map(1 -> "A", 2 -> "a"), 'b' -> Map(1 -> "B", 2 -> "b"))
		val map5: Map[Char, Map[Int, String]] = Map('c' -> Map(1 -> "C", 2 -> "c"))
		val map6: Map[Char, Map[Int, String]] = Map('a' -> Map(1 -> "A", 2 -> "a"), 'b' -> Map(1 -> "B", 2 -> "b"), 'c' -> Map(1 -> "C", 2 -> "c"))

		val mapMonoid: Monoid[Map[Char, Map[Int, String]]] =
			Monoid.mapMergeMonoid:
				Monoid.mapMergeMonoid(stringMonoid)

		assertResult(map3)(Monoid.mapMergeMonoid(stringMonoid).combine(map1, map2))
		assertResult(map6)(mapMonoid.combine(map4, map5))

		// Exercise 10.17
		val fnMonoid: Monoid[Int => String] =
			Monoid.fuctionMonoid(stringMonoid)

		val fn1: Int => String =
			(i: Int) => i.toString

		val fn2: Int => String =
			(i: Int) => s"Age: ${i.toString}"

		assertResult(fn1(10) + fn2(10))(fnMonoid.combine(fn1, fn2)(10))

		// Identity
		assertResult(fn1(10))(fnMonoid.combine(fnMonoid.zero, fn1)(10))
		assertResult(fn1(10))(fnMonoid.combine(fn1, fnMonoid.zero)(10))

		// Associativity
		assertResult(fnMonoid.combine(fn1, fnMonoid.combine(fn2, fnMonoid.zero))(10)):
			fnMonoid.combine(fnMonoid.combine(fn1, fn2), fnMonoid.zero)(10)

		// Exercise 10.18
		assertResult(Map("a" -> 2, "rose" -> 2, "is" -> 1)):
			Monoid.bag(Chapter3.List("a", "rose", "is", "a", "rose"))

		val intProductMonoid: Monoid[(Int, Int)] =
			Monoid.productMonoid(intAdditionMonoid, intAdditionMonoid)

		assertResult((4, 10))(listFoldable.foldMap(Chapter3.List(1, 2, 3, 4), intProductMonoid)(i => (1, i)))

	test("Chapter 11"):
		import Chapter11.*

		// Functor
		assertResult(List(2, 3, 4))(listFunctor.map(List(1, 2, 3))(_ + 1))
		assertResult((List(1, 2), List("a", "b")))(listFunctor.distribute(List((1, "a"), (2, "b"))))
		assertResult(List(Right(1), Right(2), Right(3)))(listFunctor.codistribute(Right(List(1, 2, 3))))

		// Monad
		assertResult(List("Thijs"))(listMonad.unit("Thijs"))
		assertResult(List('T', 'h', 'i', 'j', 's'))(listMonad.flatMap(List("Thijs"))(s => s.toList))
		assertResult(List(2, 3, 4))(listMonad.map(List(1, 2, 3))(_ + 1))
		assertResult(List(4, 5, 5, 6))(listMonad.mapTwo(List(1, 2), List(3, 4))(_ + _))

		assertResult(Some("Thijs"))(optionMonad.unit("Thijs"))
		assertResult(Some('T'))(optionMonad.flatMap(Some("Thijs"))(s => s.lift(0)))
		assertResult(Some(11))(optionMonad.map(Some(10))(_ + 1))
		assertResult(Some(10))(optionMonad.mapTwo(Some(5), Some(2))(_ * _))

		// Exercise 11.3
		val list1 = List('a', 'b', 'c', 'd')
		val list2 = List(97, 98, 99, 100)
		val list3 = List(Some(97), Some(98), Some(99), Some(100))
		assertResult(Some(list2))(optionMonad.sequence(list3))
		assertResult(Some(list1))(optionMonad.traverse(list2)((a: Int) => Some(a.toChar)))

		// Exercise: 11.4
		assertResult(Some(List.fill(10)("test123")))(optionMonad.replicateM(10, Some("test123")))
		assertResult(Some(List.fill(10)("test123")))(optionMonad.replicateMRec(10, Some("test123")))
		assertResult(Some(List.fill(10)("test123")))(optionMonad.replicateMRec2(10, Some("test123")))

		val listr = List(
			('a', 97), ('a', 98), ('a', 99), ('a', 100),
			('b', 97), ('b', 98), ('b', 99), ('b', 100),
			('c', 97), ('c', 98), ('c', 99), ('c', 100),
			('d', 97), ('d', 98), ('d', 99), ('d', 100)
		)

		assertResult(listr)(listMonad.product(list1, list2))

		// Exercise 11.6
		val list4 = List(97, 98, 99)
			assertResult(Some(list4)):
				optionMonad.filterM(list2)((i: Int) => if i < 100 then Some(true) else Some(false))

		// Exercise 11.7 & Monad Law of Associativity
		val f: Int => List[Int] = (a: Int) => List(a + 1)
		val g: Int => List[Char] = (b: Int) => List((b + 97).toChar)
		val h: Char => List[String] = (c: Char) => List(c.toString)

		val compose1: Int => List[String] = listMonad.compose(listMonad.compose(f, g), h)
		val compose2: Int => List[String] = listMonad.compose(f, listMonad.compose(g, h))

		assertResult(listMonad.flatMap(List(1, 2, 3))(compose1)):
			listMonad.flatMap(List(1, 2, 3))(compose2)

		// Exercise 11.8
		assertResult(List('T', 'h', 'i', 'j', 's')):
			listMonad.flatMapViaCompose(List("Thijs"))(s => s.toList)

		assertResult(Some('T')):
			optionMonad.flatMapViaCompose(Some("Thijs"))(s => s.lift(0))

		// Exercise 11.9
		val compose3: Int => List[String] =
			(a: Int) => listMonad.flatMap(f(a))((b: Int) => listMonad.flatMap(g(b))(h))

		assertResult(listMonad.flatMap(List(1, 2, 3))(compose1)):
			listMonad.flatMap(List(1, 2, 3))(compose3)

		// Monadlaw of (left and right) Identity
		val compose4: Int => List[Int] = listMonad.compose(f, listMonad.unit)
		val compose5: Int => List[Int] = listMonad.compose(listMonad.unit, f)
		val compose6: Int => List[Int] = (a: Int) => listMonad.flatMap(f(a))(listMonad.unit)

		assertResult(listMonad.flatMap(List(1, 2, 3))(f)):
			listMonad.flatMap(List(1, 2, 3))(compose4)

		assertResult(listMonad.flatMap(List(1, 2, 3))(f)):
			listMonad.flatMap(List(1, 2, 3))(compose5)

		// Exercise 11.10 & 11.11
		assertResult(listMonad.flatMap(List(1, 2, 3))(f)):
			listMonad.flatMap(List(1, 2, 3))(compose6)

		// Exercise 11.12
		val join = List(List(1, 2), List(3, 4))
		assertResult(List(1, 2, 3, 4))(listMonad.join(join))

		// Exercise 11.13
		assertResult(List('T', 'h', 'i', 'j', 's')):
			listMonad.flatMapViaJoin(List("Thijs"))(s => s.toList)

		assertResult(Some('T')):
			optionMonad.flatMapViaJoin(Some("Thijs"))(s => s.lift(0))

		val compose7: Int => List[String] = listMonad.composeViaJoin(listMonad.compose(f, g), h)
		val compose8: Int => List[String] = listMonad.composeViaJoin(f, listMonad.compose(g, h))

		assertResult(listMonad.flatMap(List(1, 2, 3))(compose7)):
			listMonad.flatMap(List(1, 2, 3))(compose8)

		// The Identity Monad
		assertResult(Id("Hello, world!"))(Id("Hello, ").flatMap(v => Id(v + "world!")))

		// The State Monad
		import Chapter6.State

		val fromIntState: IntState[String] =
			Chapter6.State.apply:
				(s: Int) => (s.toString, s + 1)

		val intState: IntState[String] =
			stateMonad[Int].unit("Thijs")

		// Exercise 11.2
		assertResult(stateMonad[Int].unit("Test").run(123)):
			intStateMonad.unit("Test").run(123)
		assertResult(("7 added!", 8)):
			fromIntState.flatMap(v => intStateMonad.unit(v + " added!")).run(7)
		assertResult(("7Thijs", 8)):
			fromIntState.flatMap(v => intState.map(v + _)).run(7)
		assertResult(("7 added!", 8))(fromIntState.map(_ + " added!").run(7))

		// Exercise 11.18
		assertResult(("7Thijs", 8))(fromIntState.mapTwo(intState)(_ + _).run(7))
		assertResult((List("1", "2", "3"), 4))(stateMonad[Int].replicateM(3, fromIntState).run(1))
		assertResult((List("1", "Thijs"), 2))(stateMonad[Int].sequence(List(fromIntState, intState)).run(1))

		// Exercise 11.19
		assertResult(State.get.flatMap(State.set).run(0))(State.unit(()).run(0))
		//assertResult(State.set(10).flatMap(_ => State.get).run(0))(State.unit(10).run(0))

		// Example on p. 202
		val list5 = List((0, 'a'), (1, 'b'), (2, 'c'), (3, 'd'))
		assertResult(list5)(zipWithIndex(list1))

	test("Chapter 12"):
		import Chapter12.*

		val listI  = List(1, 2, 3, 4, 5)
		val listL  = List(List(1), List(2), List(3), List(4), List(5))
		val listF  = List((i: Int) => i + 1)
		val listR1 = List(2, 3, 4, 5, 6)
		val listR2 = List(
			2, 3, 4, 5, 6, 3, 4, 5, 6, 7, 4, 5, 6, 7, 8, 5, 6, 7, 8, 9, 6, 7, 8, 9, 10
		)
		val listR3 = List(
			3, 4, 5, 6, 7, 4, 5, 6, 7, 8, 5, 6, 7, 8, 9, 6, 7, 8, 9, 10, 7, 8, 9, 10,
			11, 4, 5, 6, 7, 8, 5, 6, 7, 8, 9, 6, 7, 8, 9, 10, 7, 8, 9, 10, 11, 8, 9,
			10, 11, 12, 5, 6, 7, 8, 9, 6, 7, 8, 9, 10, 7, 8, 9, 10, 11, 8, 9, 10, 11,
			12, 9, 10, 11, 12, 13, 6, 7, 8, 9, 10, 7, 8, 9, 10, 11, 8, 9, 10, 11, 12,
			9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 7, 8, 9, 10, 11, 8, 9, 10, 11, 12,
			9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15
		)
		val listR4 = List(
			4, 5, 6, 7, 8, 5, 6, 7, 8, 9, 6, 7, 8, 9, 10, 7, 8, 9, 10, 11, 8, 9, 10,
			11, 12, 5, 6, 7, 8, 9, 6, 7, 8, 9, 10, 7, 8, 9, 10, 11, 8, 9, 10, 11, 12,
			9, 10, 11, 12, 13, 6, 7, 8, 9, 10, 7, 8, 9, 10, 11, 8, 9, 10, 11, 12, 9,
			10, 11, 12, 13, 10, 11, 12, 13, 14, 7, 8, 9, 10, 11, 8, 9, 10, 11, 12, 9,
			10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12,
			9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15, 12, 13, 14, 15,
			16, 5, 6, 7, 8, 9, 6, 7, 8, 9, 10, 7, 8, 9, 10, 11, 8, 9, 10, 11, 12, 9,
			10, 11, 12, 13, 6, 7, 8, 9, 10, 7, 8, 9, 10, 11, 8, 9, 10, 11, 12, 9, 10,
			11, 12, 13, 10, 11, 12, 13, 14, 7, 8, 9, 10, 11, 8, 9, 10, 11, 12, 9, 10,
			11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12, 9,
			10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15, 12, 13, 14, 15,
			16, 9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15, 12, 13, 14,
			15, 16, 13, 14, 15, 16, 17, 6, 7, 8, 9, 10, 7, 8, 9, 10, 11, 8, 9, 10, 11,
			12, 9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 7, 8, 9, 10, 11, 8, 9, 10, 11,
			12, 9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15, 8, 9, 10,
			11, 12, 9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15, 12, 13,
			14, 15, 16, 9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15, 12,
			13, 14, 15, 16, 13, 14, 15, 16, 17, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15,
			12, 13, 14, 15, 16, 13, 14, 15, 16, 17, 14, 15, 16, 17, 18, 7, 8, 9, 10, 11,
			8, 9, 10, 11, 12, 9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15,
			8, 9, 10, 11, 12, 9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14, 15,
			12, 13, 14, 15, 16, 9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12, 13, 14,
			15, 12, 13, 14, 15, 16, 13, 14, 15, 16, 17, 10, 11, 12, 13, 14, 11, 12, 13,
			14, 15, 12, 13, 14, 15, 16, 13, 14, 15, 16, 17, 14, 15, 16, 17, 18, 11, 12,
			13, 14, 15, 12, 13, 14, 15, 16, 13, 14, 15, 16, 17, 14, 15, 16, 17, 18, 15, 16,
			17, 18, 19, 8, 9, 10, 11, 12, 9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11, 12,
			13, 14, 15, 12, 13, 14, 15, 16, 9, 10, 11, 12, 13, 10, 11, 12, 13, 14, 11,
			12, 13, 14, 15, 12, 13, 14, 15, 16, 13, 14, 15, 16, 17, 10, 11, 12, 13, 14,
			11, 12, 13, 14, 15, 12, 13, 14, 15, 16, 13, 14, 15, 16, 17, 14, 15, 16, 17,
			18, 11, 12, 13, 14, 15, 12, 13, 14, 15, 16, 13, 14, 15, 16, 17, 14, 15, 16,
			17, 18, 15, 16, 17, 18, 19, 12, 13, 14, 15, 16, 13, 14, 15, 16, 17, 14, 15,
			16, 17, 18, 15, 16, 17, 18, 19, 16, 17, 18, 19, 20
		)
		val listP  = List(
			(1, 1), (1, 2), (1, 3), (1, 4), (1, 5),
			(2, 1), (2, 2), (2, 3), (2, 4), (2, 5),
			(3, 1), (3, 2), (3, 3), (3, 4), (3, 5),
			(4, 1), (4, 2), (4, 3), (4, 4), (4, 5),
			(5, 1), (5, 2), (5, 3), (5, 4), (5, 5)
		)

		assertResult(listR2)(listApplicative.mapTwo(listI, listI)(_ + _))
		assertResult(List("test"))(listApplicative.unit("test"))
		assertResult(listR1)(listApplicative.map(listI)(_ + 1))
		assertResult(List(listI))(listApplicative.traverse(listI)((a: Int) => List(a)))

		// Exercise 12.1
		assertResult(List(listI))(listApplicative.sequence(listL))
		assertResult(listP)(listApplicative.product(listI, listI))
		assertResult(List(List("test", "test", "test")))(listApplicative.replicateM(3, List("test")))

		// Exercise 12.2
		assertResult(listR1)(listApplicative.apply(listF)(listI))
		assertResult(listR2)(listApplicative.mapTwoViaApply(listI, listI)(_ + _))
		assertResult(listR1)(listApplicative.mapViaApply(listI)(_ + 1))

		// Exercise 12.3
		assertResult(listR3)(listApplicative.mapThree(listI, listI, listI)(_ + _ + _))
		assertResult(listR4)(listApplicative.mapFour(listI, listI, listI, listI)(_ + _ + _ + _))

		// Example on p. 210
		val parserInput1: String =
			"""
				|1/1/2010, 25
				|2/1/2010, 28
				|3/1/2010, 42
				|""".stripMargin

		val parserInput2: String =
			"""
				|# Temperature, Date
				|25, 1/1/2010
				|28, 2/1/2010
				|42, 3/1/2010
				|""".stripMargin

		assertResult(Parser.rowsApl.run(parserInput1).extract)(Parser.rowsMon.run(parserInput2).extract)

		// Exercise 12.4
		val lla = LazyList.iterate(0)(_ + 1)
		val llb = LazyList.iterate(0)(_ - 1)

		// TODO: Is this the desired result?
		assertResult(LazyList(List(0, 0)))(lazyListApplicative.sequence(List(lla, llb)))

		// Example on p. 214
		val e0 = WebForm("Thijs", java.time.LocalDate.parse("2023-09-05"), "0123456789")
		val e1 = List("Name cannot be empty")
		val e2 = List("Name cannot be empty", "Birthdate must be in the form yyyy-MM-dd")
		val e3 = List("Name cannot be empty", "Birthdate must be in the form yyyy-MM-dd", "Phone number must be 10 digits")

		assertResult(Validation.Valid(e0))(WebForm.validWebForm("Thijs", "2023-09-05", "0123456789"))
		assertResult(Validation.Invalid(e1))(WebForm.validWebForm("", "2023-09-05", "0123456789"))
		assertResult(Validation.Invalid(e2))(WebForm.validWebForm("", "09-05-2023", "0123456789"))
		assertResult(Validation.Invalid(e3))(WebForm.validWebForm("", "09-05-2023", "123456789"))

		// Example on p. 216
		import listApplicative.*

		// Structure preserving left & right Identity
		assertResult(listI)(mapTwo(unit(()), listI)((_, a) => a))
		assertResult(listI)(mapTwo(listI, unit(()))((a, _) => a))

		// Associativity
		assertResult(product(product(listR1, listR2), listR3)):
			map(product(listR1, product(listR2, listR3)))(assoc)

		// Naturality
		val f: Int => Int = _ + 1
		val g: Int => Int = _ - 1

		assertResult(mapTwo(listR1, listR2)(productF(f, g))):
			product(map(listR1)(f), map(listR2)(g))

		// Exercise 12.7
		import Chapter11.listMonad

		// Structure preserving left & right Identity
		assertResult(listI)(listMonad.mapTwo(listMonad.unit(()), listI)((_, a) => a))
		assertResult(listI)(listMonad.mapTwo(listI, listMonad.unit(()))((a, _) => a))

		// Associativity
		assertResult(product(product(listR1, listR2), listR3)):
			listMonad.map(product(listR1, product(listR2, listR3)))(assoc)

		// Naturality
		assertResult(listMonad.mapTwo(listR1, listR2)(productF(f, g))):
			product(listMonad.map(listR1)(f), listMonad.map(listR2)(g))

		// Exercise 12.8
		val p1 = (listApplicative.unit("Test"), lazyListApplicative.unit("Test"))
		val p2 = listApplicative.product(using lazyListApplicative).unit("Test")
		val p3 = listApplicative.product(using lazyListApplicative).mapTwo(p1, p2)(_ + _)
		val p4 = (listApplicative.unit("TestTest"), lazyListApplicative.unit("TestTest"))

		assertResult(p1)(p2)
		assertResult(p4)(p3)

		// Exercise 12.9
		val c1 = List(LazyList("Test"))
		val c2 = listApplicative.compose(using lazyListApplicative).unit("Test")
		val c3 = listApplicative.compose(using lazyListApplicative).mapTwo(c1, c2)(_ + _)
		val c4 = List(LazyList("TestTest"))

		assertResult(c1)(c2)
		assertResult(c3)(c4)

    // Exercise 12.10
		val composedApl = lazyListApplicative.compose(using listApplicative)

		// Structure preserving left & right Identity
		assertResult(LazyList(listI))(composedApl.mapTwo(composedApl.unit(()), LazyList(listI))((_, a) => a))
		assertResult(LazyList(listI))(composedApl.mapTwo(LazyList(listI), composedApl.unit(()))((a, _) => a))

		// Associativity
		assertResult(composedApl.product(composedApl.product(LazyList(listR1), LazyList(listR2)), LazyList(listR3))):
			composedApl.map(composedApl.product(LazyList(listR1), composedApl.product(LazyList(listR2), LazyList(listR3))))(composedApl.assoc)

		// Naturality
		assertResult(composedApl.mapTwo(LazyList(listR1), LazyList(listR2))(productF(f, g))):
			composedApl.product(composedApl.map(LazyList(listR1))(f), composedApl.map(LazyList(listR2))(g))

		// Exercise 12.12
		val mapI = Map(1 -> List('a'), 2 -> List('b'), 3 -> List('c'), 4 -> List('d'), 5 -> List('e'))
		val mapM = List(Map(1 -> 'a', 2 -> 'b', 3 -> 'c', 4 -> 'd', 5 -> 'e'))

		assertResult(mapM)(listApplicative.sequenceMap(mapI))

		// Exercise 12.13
		import Tree.*

		val listC: List[Char]      = List('a', 'b', 'c', 'd', 'e')
		val mapC: Map[Int, Char]   = Map(1 -> 'a', 2 -> 'b', 3 -> 'c')
		val mapS: Map[Int, String] = Map(1 -> "a", 2 -> "b", 3 -> "c")
		val treeI: Tree[Int]       = Tree(1, List(Tree(2, Nil), Tree(3, Nil)))
		val treeC: Tree[Char]      = Tree('a', List(Tree('b', Nil), Tree('c', Nil)))

		assertResult(Some(listC))(Traverse.listTraverse.traverse(listI)(a => Some((a + 96).toChar))(using optionMonad))
		assertResult(Some(mapS))(Traverse.mapTraverse.traverse(mapC)(a => Some(a.toString))(using optionMonad))
		assertResult(List(Some('1'), Some('2'), Some('3')))(Traverse.optionTraverse.traverse(Some("123"))(_.toList)(using listMonad))
		assertResult(Some(treeC))(Traverse.treeTraverse.traverse(treeI)(a => Some((a + 96).toChar))(using optionMonad))

		// Exercise 12.14
		assertResult(listI)(Traverse.listTraverse.map(listC)(_.toInt - 96))

		// Exercise 12.16
		val law1 = Traverse.treeTraverse.toList(Traverse.treeTraverse.reverse(treeI)) ++ Traverse.listTraverse.toList(Traverse.listTraverse.reverse(listI))
		val law2 = Traverse.listTraverse.reverse(Traverse.listTraverse.toList(listI) ++ Traverse.treeTraverse.toList(treeI))

		assertResult(law1)(law2)

		// Exercise 12.17
		assertResult(15)(Traverse.listTraverse.foldLeft(listI)(0)((a, i) => a + i))

		// Exercise 12.18
		val fl = List(1, 2, 3)
		val ff = (a: Int) => Some(a)
		val fg = (a: Int) => List(a)

		assertResult((Some(fl), List(fl)))(Traverse.listTraverse.fuse(fl)(ff, fg)(using optionMonad, listMonad))

		// Exercise 12.20
		assertResult(Some(List("Test")))(Monad.composeM(using optionMonad, listMonad, Traverse.listTraverse).unit("Test"))

	test("Chapter 13"):
		import Chapter13.*

		val testData1   = IO5.TestData(List("Thijs", "Koen"), List.empty[String])
		val testResult1 = IO5.TestData(List("Koen"), List("What's your name?", "Hello, Thijs!"))

		assertResult(testResult1)(IO5.greetState.run(testData1)._2)

		val testData2 = IO5.TestData(List("Thijs", "Koen"), List.empty[String])
		val testResult2 = IO5.TestData(List("Koen"),List("Type something:", "THIJS"))

		assertResult(testResult2)(IO6.program.ConsoleState.run.run(testData2)._2)

	test("SummerSchoolPatterns"):
		import SummerSchoolPatterns.*
		import SummerSchoolPatterns.List.*

		assertResult(Cons(2,Cons(3,Cons(4,Cons(6,Nil)))))(List(1, 2, 3, 5) >>= (a => List(a + 1)))

		assertResult(Cons(2,Cons(3,Cons(4,Cons(6,Nil)))))(List(1, 2, 3, 5).foldMap(a => List(a + 1)))

		assertResult(Cons(2,Cons(3,Cons(4,Cons(6,Nil)))))(List(1, 2, 3, 5).foldRight(List())(a => List(a + 1)))

		assertResult(Cons(Cons(2,Cons(3,Cons(4,Cons(6,Nil)))),Nil))(List(1, 2, 3, 5).traverse(a => List(a + 1)))

		assertResult(Cons(Cons(1,Cons(3,Cons(5,Nil))),Nil))(List.traversable.sequence(List(List(1, 2), List(3, 4), List(5))))

	test("TypeClasses"):
		import TypeClasses._
		import TypeClasses.Adder._

		assertResult(expected = "<header>/home/123</header><footer>/home/123</footer>")(renderWebsite("/home")(using config))
		assertResult(expected = 3)(genericAdder(1, 2))
		assertResult(expected = "3")(genericAdder("1", "2"))

	test("Algorithms"):
		val r = scala.util.Random

		val randomSeq =
			for
				_ <- 1 to 1000000
			yield
				r.nextInt(1000000)

		assertResult(randomSeq.toList.sorted)(Algorithms.quickSort(randomSeq.toList))
