// Adapted from Fun(c) 2018.7: John De Goes - FP to the Max
// Source: https://www.youtube.com/watch?v=sxudIMiOo68

object FpToTheMax extends App:
	trait Program[F[_]]:
		def finish[A](a: => A): F[A]

		def chain[A, B](fa: F[A], afb: A => F[B]): F[B]

		def map[A, B](fa: F[A], ab: A => B): F[B]

	object Program:
		def apply[F[_]](using F: Program[F]): Program[F] = F

	trait Console[F[_]]:
		def putStrLn(line: ConsoleOut): F[Unit]

		def getStrLn: F[String]

	object Console:
		def apply[F[_]](using F: Console[F]): Console[F] = F

	trait Random[F[_]]:
		def nextInt(upper: Int): F[Int]

	object Random:
		def apply[F[_]](using F: Random[F]): Random[F] = F

	extension[F[_], A] (self: F[A])(using F: Program[F])
		def flatMap[B](f: A => F[B]): F[B] =
			Program[F].chain(self, f)

		def map[B](f: A => B): F[B] =
			Program[F].map(self, f)

	enum ConsoleOut:
		case YouGuessedRight(name: String, num: Int)
		case YouGuessedWrong(name: String, num: Int)
		case DoYouWantToContinue(name: String)
		case PleaseGuess(name: String)
		case ThatIsNotValid(name: String)
		case WhatIsYourName
		case WelcomeToGame(name: String)

	object ConsoleOut:
		extension (self: ConsoleOut)
			def en: String = self match
				case YouGuessedRight(name, num) => s"You guessed right, $name! The number was $num :-)"
				case YouGuessedWrong(name, num) => s"You guessed wrong, $name! The number was: $num :-("
				case DoYouWantToContinue(name)  => s"Do you want to continue, $name? [y/n]"
				case PleaseGuess(name)          => s"Dear $name, please guess a number between 1 and 5:"
				case ThatIsNotValid(name)       => s"You did not enter a number, $name..."
				case WhatIsYourName             => "Please provide your name:"
				case WelcomeToGame(name)        => s"Hello, $name, welcome to the game!"

			def nl: String = self match
				case YouGuessedRight(name, num) => s"Dat heb je goed geraden, $name! Het nummer was $num :-)"
				case YouGuessedWrong(name, num) => s"Helaas, dat is fout, $name! Het nummer was: $num :-("
				case DoYouWantToContinue(name)  => s"Wil je doorgaan, $name? [y/n]"
				case PleaseGuess(name)          => s"Beste $name, raad een cijfer tussen 1 en 5:"
				case ThatIsNotValid(name)       => s"Je hebt geen geldig cijfer ingevoerd, $name..."
				case WhatIsYourName             => "Wat is je naam:"
				case WelcomeToGame(name)        => s"Hallo, $name, welkom bij dit spel!"

	opaque type IO[A] =
		() => A

	object IO:
		def point[A](a: => A): IO[A] =
			() => a

		extension[A] (self: IO[A])
			def unsafeRun: A =
				self()

			def map[B](f: A => B): IO[B] =
				point(f(self.unsafeRun))

			def flatMap[B](f: A => IO[B]): IO[B] =
				point(f(self.unsafeRun).unsafeRun)

		given ProgramIO: Program[IO] with
			def finish[A](a: => A): IO[A] =
				point(a)

			def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] =
				fa.flatMap(afb)

			def map[A, B](fa: IO[A], ab: A => B): IO[B] =
				fa.map(ab)

		given ConsoleIO: Console[IO] with
			def putStrLn(line: ConsoleOut): IO[Unit] =
				point(println(line.en))

			def getStrLn: IO[String] =
				point(scala.io.StdIn.readLine())

		given RandomIO: Random[IO] with
			def nextInt(upper: Int): IO[Int] =
				point(scala.util.Random.nextInt(upper))

	opaque type TestIO[A] =
		TestData => (TestData, A)

	object TestIO:
		def point[A](a: => A): TestIO[A] =
			(t: TestData) => (t, a)

		extension[A] (self: TestIO[A])
			def run(t: TestData): (TestData, A) =
				self(t)

			def eval(t: TestData): TestData =
				self.run(t)._1

			def map[B](ab: A => B): TestIO[B] =
				(t: TestData) => self.run(t) match
					case (t, a) => (t, ab(a))

			def flatMap[B](afb: A => TestIO[B]): TestIO[B] =
				(t: TestData) => self.run(t) match
					case (t, a) => afb(a).run(t)

		given ProgramTestIO: Program[TestIO] with
			def finish[A](a: => A): TestIO[A] =
				point(a)

			def chain[A, B](fa: TestIO[A], afb: A => TestIO[B]): TestIO[B] =
				fa.flatMap(afb)

			def map[A, B](fa: TestIO[A], ab: A => B): TestIO[B] =
				fa.map(ab)

		given ConsoleTestIO: Console[TestIO] with
			def putStrLn(line: ConsoleOut): TestIO[Unit] =
				(t: TestData) => t.putStrLn(line)

			def getStrLn: TestIO[String] =
				(t: TestData) => t.getStrLn

		given RandomTestIO: Random[TestIO] with
			def nextInt(upper: Int): TestIO[Int] =
				(t: TestData) => t.nextInt

	case class TestData(input: List[String], output: List[ConsoleOut], nums: List[Int]):
		def showResults: String =
			output.reverse.map(_.nl).mkString("\n")

		def nextInt: (TestData, Int) =
			(copy(nums = nums.drop(1)), nums.head)

		def putStrLn(line: ConsoleOut): (TestData, Unit) =
			(copy(output = line :: output), ())

		def getStrLn: (TestData, String) = (copy(input = input.drop(1)), input.head)

	def parseInt(input: String): Option[Int] =
		scala.util.Try(input.toInt).toOption

	def checkAnswer[F[_] : Console](name: String, num: Int, guess: Int): F[Unit] =
		if guess == num then Console[F].putStrLn(ConsoleOut.YouGuessedRight(name, num))
		else Console[F].putStrLn(ConsoleOut.YouGuessedWrong(name, num))

	def checkContinue[F[_] : Program : Console](name: String): F[Boolean] =
		for
			_     <- Console[F].putStrLn(ConsoleOut.DoYouWantToContinue(name))
			input <- Console[F].getStrLn.map(_.toLowerCase)
			cont  <- input match
				case "y" => Program[F].finish(true)
				case "n" => Program[F].finish(false)
				case _   => checkContinue(name)
		yield cont

	def gameLoop[F[_] : Program : Random : Console](name: String): F[Unit] =
		for
			num   <- Random[F].nextInt(5).map(_ + 1)
			_     <- Console[F].putStrLn(ConsoleOut.PleaseGuess(name))
			input <- Console[F].getStrLn
			_     <- parseInt(input)
				         .fold(Console[F].putStrLn(ConsoleOut.ThatIsNotValid(name))):
					          guess => checkAnswer(name, num, guess)
			cont  <- checkContinue(name)
			_     <- if cont then gameLoop(name) else Program[F].finish(())
		yield ()

	def main[F[_] : Program : Random : Console]: F[Unit] =
		for
			_    <- Console[F].putStrLn(ConsoleOut.WhatIsYourName)
			name <- Console[F].getStrLn
			_    <- Console[F].putStrLn(ConsoleOut.WelcomeToGame(name))
			_    <- gameLoop(name)
		yield ()

	// Production
	import IO.*
	import IO.given

	def mainIO: IO[Unit] = main[IO]

	mainIO.unsafeRun

	// Test
	import TestIO.*
	import TestIO.given

	def mainTestIO: TestIO[Unit] = main[TestIO]

	val testExample = TestData(
		input = "John" :: "1" :: "n" :: Nil,
		output = Nil,
		nums = 0 :: Nil
	)

	println:
		mainTestIO.eval(testExample).showResults
