object Chapter13 extends App:
	import Part3Summary.*
	import Part3Summary.Monad.*

	object IO1:
		case class Player(name: String, score: Int)

		def winner(p1: Player, p2: Player): Option[Player] =
			if p1.score > p2.score then Some(p1)
			else if p2.score > p1.score then Some(p2)
			else None

		def winnerMsg(player: Option[Player]): String =
			player
				.map(p => s"${p.name} is the winner!")
				.getOrElse("It's a draw")

		def contest1(p1: Player, p2: Player): Unit =
			println(winnerMsg(winner(p1, p2)))

		def contest2(p1: Player, p2: Player): IO[Unit] =
			IO.PrintLine:
				winnerMsg(winner(p1, p2))

		opaque type IO[A] =
			() => A

		object IO:
			def zero: IO[Unit] =
				() => ()

			def PrintLine(msg: String): IO[Unit] =
				() => println(msg)

			def ReadLine: IO[String] =
				() => scala.io.StdIn.readLine

			def doWhile[A](ioa: IO[A])(f: A => IO[Boolean]): IO[Unit] =
				for
					a <- ioa
					b <- f(a)
					_ <- if b then doWhile(ioa)(f) else zero
				yield ()

			def forever[A, B](a: IO[A]): IO[B] =
				a.flatMap(_ => forever(a))

			extension[A] (self: IO[A])
				def unsafeRun: A =
					self()

				@annotation.targetName("combine")
				def ++(that: IO[A]): IO[A] =
					() => self.unsafeRun; that.unsafeRun

			given ioMonad: Monad[IO] with
				def unit[A](a: => A): IO[A] =
					() => a

				def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
					f(fa.unsafeRun)
	end IO1

	object IO2:
		enum TailRec[A]:
			case Return(a: A)
			case Suspend(resume: () => A)
			case Chain[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

			def flatMap[B](f: A => TailRec[B]): TailRec[B] =
				TailRec.ioMonad.flatMap(this)(f)

			def map[B](f: A => B): TailRec[B] =
				flatMap((a: A) => Return(f(a)))

			@annotation.tailrec
			final def unsafeRun: A = this match
				case Return(a)   => a
				case Suspend(r)  => r()
				case Chain(x, f) => x match
					case Return(a) => f(a).unsafeRun
					case Suspend(r) => f(r()).unsafeRun
					//case Chain(y, g) => IO.Chain(y.flatMap(g), f).unsafeRun
					//case Chain(y, g) => y.flatMap(a => g(a)).flatMap(f).unsafeRun
					case Chain(y, g) => y.flatMap(a => g(a).flatMap(f)).unsafeRun

		object TailRec:
			def PrintLine(s: String): TailRec[Unit] =
				TailRec.Suspend(() => println(s))

			def ReadLine: TailRec[String] =
				TailRec.Suspend(() => scala.io.StdIn.readLine)

			def forever[A, B](a: TailRec[A]): TailRec[B] =
				a.flatMap(_ => forever(a))

			given ioMonad: Monad[TailRec] with
				def unit[A](a: => A): TailRec[A] =
					TailRec.Suspend(() => a)

				def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
					TailRec.Chain(fa, f)
	end IO2

	object IO3:
		enum Free[F[_], A]:
			case Return(a: A)
			case Suspend(s: F[A])
			case Chain[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

			def translate[G[_]](fToG: [x] => F[x] => G[x]): Free[G, A] =
				this.runFree([x] => (fx: F[x]) => Suspend(fToG(fx)))

		object Free:
			// Exercise 13.1
			given freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
				def unit[A](a: => A): Free[F, A] =
					Free.Return(a)

				def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
					Free.Chain(fa, f)

			// Exercise 13.2
			extension [A](self: TailRec[A])
				@annotation.tailrec
				def runTrampoline: A =
					self match
						case Return(a)   => a
						case Suspend(s)  => s()
						case Chain(x, f) => x match
							case Return(a)   => f(a).runTrampoline
							case Suspend(s)  => f(s()).runTrampoline
							case Chain(y, g) => y.flatMap(a => g(a).flatMap(f)).runTrampoline

			// Exercise 13.3
			@annotation.tailrec
			def step[F[_], A](a: Free[F, A]): Free[F, A] =
				a match
					case Chain(Chain(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
					case Chain(Return(x), f) => step(f(x))
					case _ => a

			extension [F[_], A](self: Free[F, A])
				def run(using F: Monad[F]): F[A] =
					step(self) match
						case Return(a) => F.unit(a)
						case Suspend(s) => s
						case Chain(x, f) => x match
							case Suspend(s) => F.flatMap(s)(a => f(a).run)
							case _          => sys.error("Unreacheable case since step takes care of the other cases.")

				def runFree[G[_]](t: [x] => F[x] => G[x])(using G: Monad[G]): G[A] =
					step(self) match
						case Return(a) => G.unit(a)
						case Suspend(s) => t(s)
						case Chain(x, f) => x match
							case Suspend(s) => G.flatMap(t(s))(a => f(a).runFree(t))
							case _ => sys.error("Unreacheable case since step takes care of the other cases.")

			type TailRec[A] = Free[Function0, A]
			type Async[A]   = Free[Chapter7.nonBlocking.Par, A]
	end IO3

	object IO4:
		import Chapter7.nonBlocking.Par
		import Chapter7.nonBlocking.Par.*
		import Monads.State
		import Monads.State.*
		import Monads.State.stateMonad

		enum Console[A]:
			case ReadLine extends Console[Option[String]]
			case PrintLine(line: String) extends Console[Unit]

			def readLineOption: Option[String] =
				scala.util.Try(scala.io.StdIn.readLine).toOption

			def toThunk: () => A =
				this match
					case ReadLine        => () => readLineOption
					case PrintLine(line) => () => println(line)

			def toPar: Par[A] =
				this match
					case ReadLine        => lazyUnit(readLineOption)
					case PrintLine(line) => lazyUnit(println(line))

			import Part3Summary.Monad.flatMap
			import Part3Summary.Monad.map

			def toState: State[Buffers, A] =
				this match
					case ReadLine =>
						for
							s <- State.get[Buffers]
							_ <- State.set(s.copy(in = if s.in.isEmpty then s.in else s.in.tail))
						yield
							s.in.headOption

					case PrintLine(line) =>
						for
							s <- State.get[Buffers]
							_ <- State.set(s.copy(out = s.out :+ line))
						yield ()

		case class Buffers(in: List[String], out: List[String])

		import IO3.*

		object Console:
			type ConsoleIO[A] = Free[Console, A]

			def readLine: ConsoleIO[Option[String]] =
				Free.Suspend(ReadLine)

			def printLine(line: String): ConsoleIO[Unit] =
				Free.Suspend(PrintLine(line))

			// Exercise 13.4
			extension [A](self: Free[Console, A])
				def unsafeRunConsole: A =
					self.translate([x] => (c: Console[x]) => c.toThunk).runTrampoline

				def unsafeRunConsoleState: State[Buffers, A] =
					self.runFree([x] => (c: Console[x]) => c.toState)

				def toPar: Par[A] =
					self.runFree([x] => (c: Console[x]) => c.toPar)

				def toState: State[Buffers, A] =
					self.runFree([x] => (c: Console[x]) => c.toState)

				def toThunk: () => A =
					self.runFree([x] => (c: Console[x]) => c.toThunk)

			given function0Monad: Monad[Function0] with
				def unit[A](a: => A): () => A =
					() => a

				def flatMap[A, B](fa: () => A)(f: A => () => B): () => B =
					f(fa())

			given parMonad: Monad[Par] with
				def unit[A](a: => A): Par[A] = Par.unit(a)

				def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
					fa.flatMap(f)

	end IO4

	//import IO1.IO
	//import IO1.IO.*

	import IO2.TailRec as IO
	import IO2.TailRec.*

	def fahrenheitToCelcius(t: Double): Double =
		(t - 32) * 5 / 9

	def fahrenheitToCelciusConverter: IO[Unit] =
		for
			_ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
			t <- ReadLine
			_ <- PrintLine(s"Fahrenheit to Celcius: ${fahrenheitToCelcius(t.toDouble).toString} 🌡")
		yield ()

	def echo: IO[Unit]           = ReadLine.flatMap(PrintLine)
	def readInt: IO[Int]         = ReadLine.map(_.toInt)
	def replInts: IO[List[Int]]  = IO.ioMonad.replicateM(10, readInt)

	def p: IO[Unit] = forever(PrintLine("Still going..."))

	//p.unsafeRun
	//fahrenheitToCelciusConverter.unsafeRun

	import IO3.*
	import IO4.*
	import IO3.Free.*

	val f1: Free[Console, Option[String]] =
		for
			_ <- Console.printLine("I can only interact with the console.")
			l <- Console.readLine
			_ <- Console.printLine("Thanks for understanding!")
		yield l

	val buffer  = Buffers(List("That's Ok.", "No problem ;-)", "All good!"), List.empty[String])
	//val f1State = f1.unsafeRunConsoleState.run(buffer)
	//val f1Test  = f1.unsafeRunConsole

	object IO5:
		import NonBlockingPar.Par
		import NonBlockingPar.Par.*
		import NonBlockingPar.Par.parMonad

		import Monads.State
		import Monads.State.*
		import Monads.State.stateMonad

		import Monads.Free
		import Monads.Free.*
		import Monads.Free.freeMonad

		given thunkMonad: Monad[Function0] with
			def unit[A](a: => A): () => A =
				() => a

			def flatMap[A, B](fa: () => A)(f: A => () => B): () => B =
				f(fa())

		trait Console[F[_]]:
			def readLine: F[Option[String]]
			def printLine(line: String): F[Unit]

		object Console:
			def readLineOption: Option[String] =
				scala.util.Try(scala.io.StdIn.readLine).toOption.filter(_.nonEmpty)

			given parConsole: Console[Par] with
				def readLine: Par[Option[String]] =
					parMonad.unit(readLineOption)

				def printLine(line: String): Par[Unit] =
					parMonad.unit(println(line))

			given freeConsole: Console[[x] =>> Free[Par, x]] with
				def readLine: Free[Par, Option[String]] =
					freeMonad.unit(readLineOption)

				def printLine(line: String): Free[Par, Unit] =
					freeMonad.unit(println(line))

			given thunkConsole: Console[Function0] with
				def readLine: () => Option[String] =
					thunkMonad.unit(readLineOption)

				def printLine(line: String): () => Unit =
					thunkMonad.unit(println(line))

			given stateConsole: Console[[x] =>> State[TestData, x]] with
				def readLine: State[TestData, Option[String]] =
					for
						s <- State.get[TestData]
						_ <- State.set(s.copy(input = if s.input.isEmpty then s.input else s.input.tail))
					yield
						s.input.headOption

				def printLine(line: String): State[TestData, Unit] =
					for
						s <- State.get[TestData]
						_ <- State.set(s.copy(output = s.output :+ line))
					yield ()

		def greet[F[_]: Monad](using c: Console[F]): F[Unit] =
			for
				_ <- c.printLine("What's your name?")
				l <- c.readLine
				_ <- l match
					case Some(name) => c.printLine(s"Hello, $name!")
					case None       => greet
			yield ()

		case class TestData(input: List[String], output: List[String])

		def greetPar: Par[Unit]               = greet[Par]
		def greetFree: Free[Par, Unit]        = greet[[x] =>> Free[Par, x]]
		def greetThunk: () => Unit            = greet[Function0]
		def greetState: State[TestData, Unit] = greet[[x] =>> State[TestData, x]]

	end IO5

	// greetPar
	//val pool = java.util.concurrent.Executors.newFixedThreadPool(4)
	//IO5.greetPar.run(pool)
	//pool.shutdown()

	// greetFree
	//val pool = java.util.concurrent.Executors.newFixedThreadPool(4)
	//IO5.greetFree.run.run(pool)
	//pool.shutdown()

	// greetThunk
	//IO5.greetThunk()

	// greetState
	//val testData = IO5.TestData(List("Thijs", "Koen"), List.empty[String])
	//println(IO5.greetState.run(testData)._2)

	object IO6:
		import Monads.Free
		import Monads.Free.*

		import NonBlockingPar.Par
		import NonBlockingPar.Par.*

		import Monads.State
		import Monads.State.*

		import IO5.TestData

		type IO[A] = Free[Par, A]

		enum Console[A]:
			case ReadLine extends Console[Option[String]]
			case PrintLine(line: String) extends Console[Unit]

			def readLineOption: Option[String] =
				scala.util.Try(scala.io.StdIn.readLine).toOption

			def toPar: Par[A] =
				this match
					case ReadLine        => lazyUnit(readLineOption)
					case PrintLine(line) => lazyUnit(println(line))

			def toState: State[TestData, A] =
				this match
					case ReadLine =>
						for
							s <- State.get[TestData]
							_ <- State.set(s.copy(input = if s.input.isEmpty then s.input else s.input.tail))
						yield
							s.input.headOption

					case PrintLine(line) =>
						for
							s <- State.get[TestData]
							_ <- State.set(s.copy(output = s.output :+ line))
						yield ()

		object Console:
			def readLine: Free[Console, Option[String]] =
				Free.Suspend(ReadLine)

			def printLine(line: String): Free[Console, Unit] =
				Free.Suspend(PrintLine(line))

			extension[A] (self: Free[Console, A])
				def ConsoleIO: IO[A] = self.translate([x] => (c: Console[x]) => c.toPar)

				def ConsoleState: Free[[x] =>> State[TestData, x], A] =
					self.translate([x] => (c: Console[x]) => c.toState)

		val program: Free[Console, Unit] =
			for
				_ <- Console.printLine("Type something:")
				l <- Console.readLine
				_ <- Console.printLine(l.fold("You typed nothing!")(_.toUpperCase))
			yield ()

	end IO6

	// Program IO
	//val pool = java.util.concurrent.Executors.newFixedThreadPool(4)
	//IO6.program.ConsoleIO.run.run(pool)
	//pool.shutdown()

	// Program State
	//val testData = IO5.TestData(List("Thijs", "Koen"), List.empty[String])
	//println(IO6.program.ConsoleState.run.run(testData)._2)
