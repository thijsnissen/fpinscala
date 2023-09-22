object Quicksort2 extends App:
	import Part3Summary.Monad
	import Part3Summary.Monad.*
	import Monads.Free.*
	import Monads.Free.function0Monad
	import NonBlockingPar.Par.parMonad

	import math.Ordering.Implicits.infixOrderingOps

	def qs[A](s: Seq[A])(using Ordering[A]): Seq[A] =
		if s.isEmpty then s else
			val (lessThan, equalTo, greaterThan) =
				val pivot = s.head

				s.foldLeft((Seq.empty[A], Seq.empty[A], Seq.empty[A])):
					case ((l, e, g), x) if x > pivot => (l, e, x +: g)
					case ((l, e, g), x) if x == pivot => (l, x +: e, g)
					case ((l, e, g), x) if x < pivot => (x +: l, e, g)

			qs(lessThan) ++ equalTo ++ qs(greaterThan)

	def quicksort[F[_], A](s: Seq[A])(using f: Monad[F]): F[Seq[A]] =
		???

	import java.util.concurrent.ExecutorService
	import scala.util.Random

	val r: Random = scala.util.Random

	val size: Int = 100

	val randomSeq: List[Int] =
		(for _ <- 0 until size yield r.nextInt(Int.MaxValue)).toList

	val pool: ExecutorService =
		java.util.concurrent.Executors.newFixedThreadPool(4)

	def quicksortTailRec[A]: Seq[A] => TailRec[Seq[A]] = quicksort[TailRec, A]
	def quicksortAsync[A]: Seq[A] => Async[Seq[A]]     = quicksort[Async, A]

	pprint.pprintln(quicksortTailRec(randomSeq).run())
	pprint.pprintln(quicksortAsync(randomSeq).run.run(pool))

	pool.shutdown()
