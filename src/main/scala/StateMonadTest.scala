object StateMonadTest extends App:
	import Monads.State
	import Monads.State.*
	import Monads.State.stateMonad

	val myList: List[Char] =
		List('a', 'b', 'c', 'd', 'e')

	def zipWithIndex[A](la: List[A]) =
		val result =
			(a: A) =>
				for
					i <- get[Int]
					_ <- set(i + 1)
				yield
					List((i, a))

		la.flatMap(result)

	pprint.log(zipWithIndex(myList))
