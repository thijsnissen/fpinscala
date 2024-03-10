object CanIFollowThisRecipe extends App:
	/* Solution to 'Can I follow this recipe?'
	 *
	 * Task: Take as input sets of positive integers representing parts of a
	 *       recipe. Output one of two distinct consistent values depending on
	 *       whether the input represents a possible or impossible recipe.
	 * Note: You may assume there is at least one part in the input,
	 *       and at least one ingredient in every part.
	 *
	 * See: https://codegolf.stackexchange.com/questions/271522/can-i-follow-this-recipe
	 */
	def recipeIsPossible(i: List[Set[Int]]): Boolean =
		i
			.permutations
			.exists: (p: List[Set[Int]]) =>
				val r: List[Set[Int]] =
					p
						.tail
						.scanLeft(p.head)((b: Set[Int], a: Set[Int]) => b ++ a)
						.distinct

				r.size == p.size

	// Testcases
	val a = List(Set(9, 2, 3))
	val b = List(Set(5), Set(5))
	val c = List(Set(2), Set(9))
	val d = List(Set(5), Set(3, 5), Set(2, 5, 6), Set(1, 2, 4, 5), Set(1, 2, 3, 5))
	val e = List(Set(1, 2, 3, 5), Set(5), Set(2, 4, 5), Set(1, 2, 4, 5), Set(3, 5))
	val f = List(Set(5), Set(5), Set(9, 5))
	val g = List(Set(1, 2, 3, 5), Set(5), Set(2, 4, 5), Set(1, 2, 4, 5), Set(3, 5), Set(9, 5))

	// Tests
	assert( recipeIsPossible(a))
	assert(!recipeIsPossible(b))
	assert( recipeIsPossible(c))
	assert( recipeIsPossible(d))
	assert(!recipeIsPossible(e))
	assert(!recipeIsPossible(f))
	assert(!recipeIsPossible(g))
