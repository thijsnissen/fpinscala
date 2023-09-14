object STArrayTest extends App:
	import Monads.State
	import Monads.State.*
	import Monads.State.stateMonad
	import Monads.RunnableST

	import scala.reflect.ClassTag

	final class STArray[S, A] private (private var value: Array[A]):
		def size: State[S, Int] =
			stateMonad[S].unit(value.size)

		def write(i: Int, a: A): State[S, Unit] =
			stateMonad[S].unit(value(i) = a)

		def read(i: Int): State[S, A] =
			stateMonad[S].unit(value(i))

		def freeze: State[S, List[A]] =
			stateMonad[S].unit(value.toList)

		def swap(i: Int, j: Int): State[S, Unit] =
			for
				x <- read(i)
				y <- read(j)
				_ <- write(i, y)
				_ <- write(j, x)
			yield ()

	object STArray:
		def empty[S, A: ClassTag]: State[S, STArray[S, A]] =
			stateMonad[S].unit(new STArray[S, A](Array.empty[A]))

		def fromList[S, A: ClassTag](la: List[A]): State[S, STArray[S, A]] =
			stateMonad[S].unit(new STArray[S, A](la.toArray))

	def program(i: Int): RunnableST[String] =
		RunnableST.unit:
			[s] => () =>
				for
					a1 <- STArray.fromList[s, Int](List(1, 2, 3))
					a2 <- STArray.fromList[s, Char](List('a', 'b', 'c'))
					_  <- a1.swap(0, i)
					_  <- a2.swap(i, 0)
					r1 <- a1.read(i)
					s1 <- a1.size
					r2 <- a2.read(i)
					_  <- a1.write(i, r1 * s1)
					_  <- a2.write(i, (r2 - 32).toChar)
					a  <- a1.read(i)
					b  <- a2.read(i)
				yield
					s"$b: $a"

	println(program(1).runST)

	assert(program(1).runST == "A: 3")

	// This won't compile because of polymorphic function types.
	// This way we cannot instantiate a State that returns a
	// mutable array :-)
	//val test: [s] => () => State[s, STArray[Nothing, Int]] =
	//	[s] => () => STArray.fromList[Nothing, Int](List(1, 2, 3))
