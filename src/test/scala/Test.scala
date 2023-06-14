import Chapter8.Prop.MaxSize
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

		val executorService = java.util.concurrent.Executors.newFixedThreadPool(20)

		// Exercise 7.5
		assertResult(List(1, 2, 3, 4, 5))(sortPar(Par.lazyUnit(List(2, 4, 3, 5, 1))).run(executorService).get)
		assertResult(List(2, 3, 4, 5, 6))(parMap(List(1, 2, 3, 4, 5))(_ + 1).run(executorService).get)

		// Exercise 7.6
		assertResult(List(1, 2))(parFilter(List(1, 2, 3, 4, 5))(_ < 3).run(executorService).get)
		assertResult(15)(parallelCombination(Vector(1, 2, 3, 4, 5), 0)(identity)(_ + _).run(executorService).get)
		assertResult(5)(max(Vector(1, 2, 3, 4, 5)).run(executorService).get)
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

		passedProp1.run()
		falsifiedProp.run()
		passedProp2.run()

		assertResult(false)(passedProp1.check().isFalsified)
		assertResult(true)(falsifiedProp.check().isFalsified)
		assertResult(false)(passedProp2.check().isFalsified)

		val smallInt = Gen.choose(-10, 10)
		val maxProp = Prop.forAll(Gen.listOf(smallInt)):
			ns =>
				val max = ns.max
				!ns.exists(_ > max)

		// This should fail on empty list. Does it recieve an empty list in the test?
		maxProp.run()

		assertResult(false)(maxProp.check().isFalsified)

	test("TypeClasses"):
		import TypeClasses._
		import TypeClasses.Adder._

		assertResult(expected = "<header>/home/123</header><footer>/home/123</footer>")(renderWebsite("/home")(using config))
		assertResult(expected = 3)(genericAdder(1, 2))
		assertResult(expected = "3")(genericAdder("1", "2"))
