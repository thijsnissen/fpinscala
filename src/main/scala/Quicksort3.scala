object Quicksort3 extends App:
	import math.Ordering.Implicits.infixOrderingOps

	def quicksort[A](s: Seq[A])(using Ordering[A]): Seq[A] =
		if s.isEmpty then s else
			val (lessThan, equalTo, greaterThan) =
				val pivot = s.head

				s.foldLeft((Seq.empty[A], Seq.empty[A], Seq.empty[A])):
					case ((l, e, g), x) =>
						if      x > pivot  then (l, e, x +: g)
						else if x == pivot then (l, x +: e, g)
						else                    (x +: l, e, g) // x < pivot

			quicksort(lessThan) ++ equalTo ++ quicksort(greaterThan)

	import scala.util.control.TailCalls.*

	def quicksortTailRec[A](s: Seq[A])(using Ordering[A]): TailRec[Seq[A]] =
		if s.isEmpty then done(s) else
			val (lessThan, equalTo, greaterThan) =
				val pivot = s.head

				s.foldLeft((Seq.empty[A], Seq.empty[A], Seq.empty[A])):
					case ((l, e, g), x) =>
						if      x > pivot  then (l, e, x +: g)
						else if x == pivot then (l, x +: e, g)
						else                    (x +: l, e, g) // x < pivot

			???

			//for
			//	lt <- tailcall(quicksort(lessThan))
			//	gt <- tailcall(quicksort(greaterThan))
			//yield
			// lt ++ equalTo ++ gt

	val randomSeq: List[Int] =
		(for _ <- 0 until 1000000 yield scala.util.Random.nextInt(Int.MaxValue)).toList

	assert(quicksort(randomSeq) == quicksortTailRec(randomSeq).result)
