object Quicksort extends App:
	import Monads.Free
	import Monads.Free.*

	import math.Ordering.Implicits.infixOrderingOps

	def quicksort[A](s: Seq[A])(using Ordering[A]): Seq[A] =
		if s.isEmpty then s else
			val (lessThan, equalTo, greaterThan) =
				val pivot = s.head

				s.foldLeft((Seq.empty[A], Seq.empty[A], Seq.empty[A])):
					case ((l, e, g), x) if x > pivot => (l, e, x +: g)
					case ((l, e, g), x) if x == pivot => (l, x +: e, g)
					case ((l, e, g), x) if x < pivot => (x +: l, e, g)

			quicksort(lessThan) ++ equalTo ++ quicksort(greaterThan)

	def quicksortTailRec[A](s: Seq[A])(using Ordering[A]): TailRec[Seq[A]] =
		if s.isEmpty then Return(s) else
			val (lessThan, equalTo, greaterThan) =
				val pivot = s.head

				s.foldLeft((Seq.empty[A], Seq.empty[A], Seq.empty[A])):
					case ((l, e, g), x) if x > pivot  => (l, e, x +: g)
					case ((l, e, g), x) if x == pivot => (l, x +: e, g)
					case ((l, e, g), x) if x < pivot  => (x +: l, e, g)

			for
				lt <- quicksortTailRec(lessThan)
				gt <- quicksortTailRec(greaterThan)
			yield
				lt ++ equalTo ++ gt

	def quicksortAsync[A](s: Seq[A])(using Ordering[A]): Async[Seq[A]] =
		if s.isEmpty then Return(s) else
			val (lessThan, equalTo, greaterThan) =
				val pivot = s.head

				s.foldLeft((Seq.empty[A], Seq.empty[A], Seq.empty[A])):
					case ((l, e, g), x) if x > pivot => (l, e, x +: g)
					case ((l, e, g), x) if x == pivot => (l, x +: e, g)
					case ((l, e, g), x) if x < pivot => (x +: l, e, g)

			for
				lt <- quicksortAsync(lessThan)
				gt <- quicksortAsync(greaterThan)
			yield
				lt ++ equalTo ++ gt

	import java.util.concurrent.ExecutorService
	import scala.util.Random

	val r: Random = scala.util.Random

	val size: Int = 25000000

	val randomSeq: List[Int] =
		(for _ <- 0 until size yield r.nextInt(Int.MaxValue)).toList

	val pool: ExecutorService =
		java.util.concurrent.Executors.newFixedThreadPool(4)

	val t1: Long = System.currentTimeMillis
	def r1: Seq[Int] = quicksort(randomSeq)

	val t2: Long = System.currentTimeMillis
	def r2: Seq[Int] = quicksortTailRec(randomSeq).runTailRec

	val t3: Long = System.currentTimeMillis
	def r3: Seq[Int] = quicksortAsync(randomSeq).run.run(pool)

	val t4: Long = System.currentTimeMillis

	pool.shutdown()

	// size: 25.000.000
	println(s"Quicksort: ${t2 - t1}ms") // 24196ms
	println(s"TailRec:   ${t3 - t2}ms") // 24691ms
	println(s"Async:     ${t4 - t3}ms") // 39576ms

	def program[F[_], A]: F[Seq[A]] = ???

	def pTailRec[A]: TailRec[Seq[A]] = program[TailRec, A]
	def pAsync[A]: Async[Seq[A]]     = program[Async, A]

	pTailRec.runTailRec
	pAsync.run
