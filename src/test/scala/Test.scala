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

	test("TypeClasses"):
		import TypeClasses._
		import TypeClasses.Adder._

		assertResult(expected = "<header>/home/123</header><footer>/home/123</footer>")(renderWebsite("/home")(using config))
		assertResult(expected = 3)(genericAdder(1, 2))
		assertResult(expected = "3")(genericAdder("1", "2"))
