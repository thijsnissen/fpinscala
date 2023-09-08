object StateMonadTest extends App:
	import Monads.State
	import Monads.State.*
	import Monads.State.stateMonad

	val myList: List[Char] =
		List('a', 'b', 'c', 'd', 'e')

	def zipWithIndex[A](la: List[A]): List[(A, Int)] =
		val (result: List[(A, Int)], _) =
			la
				.foldLeft(stateMonad.unit(List.empty[(A, Int)])):
					(acc: State[Int, List[(A, Int)]], a: A) =>
						for
							l	<- acc
							s <- get[Int]
							_ <- set(s + 1)
						yield
							(a, s) :: l
				.run(0)

		result.reverse

	assert(myList.zipWithIndex == zipWithIndex(myList))

	pprint.pprintln(myList.zipWithIndex)
	pprint.pprintln(zipWithIndex(myList))
