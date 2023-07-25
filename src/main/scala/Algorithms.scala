object Algorithms extends App:
	import scala.math.Ordering.Implicits.*

	def quickSort[A](s: Seq[A])(using Ordering[A]): Seq[A] =
		if s.length <= 1 then
			s
		else
			val (lessThan, equalTo, greaterThan) =
				val pivot = s.head

				s.foldLeft((Seq.empty[A], Seq.empty[A], Seq.empty[A])):
					case ((l, e, g), x) if x > pivot => (l, e, x +: g)
					case ((l, e, g), x) if x == pivot => (l, x +: e, g)
					case ((l, e, g), x) if x < pivot => (x +: l, e, g)

			quickSort(lessThan) ++ equalTo ++ quickSort(greaterThan)

