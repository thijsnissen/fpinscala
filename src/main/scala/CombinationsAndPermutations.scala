import scala.annotation.tailrec

object CombinationsAndPermutations extends App:

	/** Get all the possible combinations of length 3 given elements X and Y
	 * Results in 2 ^^ 3 = 8 possibilities
	 */

	@tailrec def combinations[A](
		todo: List[A],
		state: List[List[A]],
		acc: List[List[A]]
	): List[List[A]] =
		todo match
			case h :: t => combinations(t, state, acc ::: state.map(h :: _))
			case Nil => acc

	def makeCombinations[A](length: Int, elements: List[A]): List[List[A]] =
		(1 to length).foldLeft(List(List.empty[A])): (acc, _) =>
			combinations(elements, acc, List.empty)

	val result1 =
		makeCombinations(length = 3, elements = List('X', 'Y'))


	/** Get all the possible permutations of List('Y', 'X', 'Z') Results in 3! = 6
	 * possibilities
	 */

	def split[A](list: List[A]): List[(A, List[A])] =
		list match
			case Nil => Nil
			case x :: xs => (x, xs) :: split(xs).map((y, ys) => (y, x :: ys))

	def permutations[A](xs: List[A]): List[List[A]] =
		if xs.isEmpty then List(List.empty[A])
		else split(xs).flatMap((v, vs) => permutations(vs).map(p => v :: p))

	val result2 = permutations(List('Y', 'X', 'Z'))

	result1.foreach(println)
	println("---")
	result2.foreach(println)

