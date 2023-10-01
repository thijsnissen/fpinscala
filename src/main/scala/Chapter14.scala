import java.util.concurrent.ExecutorService
import scala.util.Random

object Chapter14 extends App:
	import Part3Summary.Monad
	import Part3Summary.Monad.*

	import Monads.Free
	import Monads.Free.*
	import Monads.Free.freeMonad

	import NonBlockingPar.Par
	import NonBlockingPar.Par.*
	import NonBlockingPar.Par.parMonad

	import scala.math.Ordering.Implicits.*

	def quicksortMutable(xs: List[Int]): List[Int] =
		if xs.isEmpty then xs else
			val arr = xs.toArray

			def swap(x: Int, y: Int): Unit =
				val tmp = arr(x)
				arr(x) = arr(y)
				arr(y) = tmp

			def partition(n: Int, r: Int, pivot: Int) =
				val pivotVal = arr(pivot)

				swap(pivot, r)

				var j = n

				for
					i <- n until r
					if arr(i) < pivotVal
				do
					swap (i, j)
					j += 1

				swap(j, r)

				j

			def qs(n: Int, r: Int): Unit =
				if n < r then
					val pi = partition(n, r, n + (r - n) / 2)
					qs(n, pi - 1)
					qs(pi + 1, r)

			qs(0, arr.length - 1)

			arr.toList

	def quicksortImmutable[A](s: Seq[A])(using Ordering[A]): Seq[A] =
		if s.isEmpty then s else
			val (lessThan, equalTo, greaterThan) =
				val pivot = s.head

				s.foldLeft((Seq.empty[A], Seq.empty[A], Seq.empty[A])):
					case ((l, e, g), x) if x > pivot  => (l, e, x +: g)
					case ((l, e, g), x) if x == pivot => (l, x +: e, g)
					case ((l, e, g), x) if x < pivot  => (x +: l, e, g)

			quicksortImmutable(lessThan) ++ equalTo ++ quicksortImmutable(greaterThan)

	opaque type ST[S, A] =
		S => (A, S)

	object ST:
		def apply[S, A](a: => A): ST[S, A] =
			lazy val memo = a

			s => (memo, s)

		def lift[S, A](f: S => (A, S)): ST[S, A] = f

		def run[A](st: [s] => () => ST[s, A]): A =
			val su: ST[Unit, A] = st[Unit]()

			su(())(0)

		given STMonad[S]: Monad[[x] =>> ST[S, x]] with
			def unit[A](a: => A): ST[S, A] =
				s => (a, s)

			def flatMap[A, B](fa: ST[S, A])(f: A => ST[S, B]): ST[S, B] =
				s =>
					val (aa, as) = fa(s)

					f(aa)(as)

	final class STRef[S, A](protected var cell: A):
		def read: ST[S, A] = ST(cell)

		def write(a: => A): ST[S, Unit] =
			ST.lift[S, Unit]:
				s => cell = a; ((), s)

	object STRef:
		def apply[S, A](a: A): ST[S, STRef[S, A]] =
			ST(new STRef[S, A](a))

	import ST.*
	import ST.STMonad

	opaque type RunnableST[A] =
		[s] => () => ST[s, A]

	val program: RunnableST[(Int, Int)] =
		[s] => () =>
			for
				r1 <- STRef[s, Int](1)
				r2 <- STRef[s, Int](2)
				x  <- r1.read
				y  <- r2.read
				_  <- r1.write(y + 1)
				_  <- r2.write(x + 1)
				a  <- r1.read
				b  <- r2.read
			yield
				(a, b)

	// (3,2)
	//println(ST.run(program))

	final class STArray[S, A](private var value: Array[A]):
		def size: ST[S, Int] = ST(value.size)

		def write(i: Int, a: A): ST[S, Unit] =
			ST.lift[S, Unit]:
				s => value(i) = a; ((), s)

		def read(i: Int): ST[S, A] =
			ST(value(i))

		def freeze: ST[S, List[A]] =
			ST(value.toList)

		// Exercise 14.1
		def fill(xs: Map[Int, A]): ST[S, Unit] =
			xs.foldRight(ST[S, Unit](())):
				case ((k, v), st: ST[S, Unit] ) =>
					st.flatMap(_ => write(k, v))

		def swap(i: Int, j: Int): ST[S, Unit] =
			for
				x <- read(i)
				y <- read(j)
				_ <- write(i, y)
				_ <- write(j, x)
			yield ()

	import scala.reflect.ClassTag

	object STArray:
		def apply[S, A: ClassTag](sz: Int, v: A): ST[S, STArray[S, A]] =
			ST(new STArray[S, A](Array.fill(sz)(v)))

		def fromList[S, A: ClassTag](xs: List[A]): ST[S, STArray[S, A]] =
			ST(new STArray[S, A](xs.toArray))

		// Exercise 14.2
		def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] =
			for
				vp <- a.read(pivot)
				_  <- a.swap(pivot, r)
				j  <- STRef(l)
				_  <- (l until r).foldLeft(ST[S, Unit](())):
					(s: ST[S, Unit], i) =>
						for
							_  <- s
							vi <- a.read(i)
							_  <-
								if vi < vp then
									for
										vj <- j.read
										_  <- a.swap(i, vj)
										_  <- j.write(vj + 1)
									yield ()
								else ST[S, Unit](())
						yield ()
				x <- j.read
				_ <- a.swap(x, r)
			yield x

		def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] =
			if l < r then
				for
					pi <- partition(a, l, r, l + (r - l) / 2)
					_  <- qs(a, l, pi - 1)
					_  <- qs(a, pi + 1, r)
				yield ()
			else ST[S, Unit](())

		def quicksort(xs: List[Int]): List[Int] =
			def result =
				[s] => () =>
					for
						arr    <- STArray.fromList[s, Int](xs)
						size   <- arr.size
						_      <- qs(arr, 0, size - 1)
						sorted <- arr.freeze
					yield
						sorted

			if xs.isEmpty then xs else ST.run(result)

	import scala.collection.mutable

	// Exercise 14.3
	final class STHashMap[S, K, V](private var value: mutable.HashMap[K, V]):
		def size: ST[S, Int] = ST(value.size)

		def write(k: K, v: V): ST[S, Unit] =
			ST.lift[S, Unit]:
				s => value(k) = v; ((), s)

		def read(k: K): ST[S, V] =
			ST(value(k))

		def freeze: ST[S, Map[K, V]] =
			ST(value.toMap)

		def fill(xs: Map[K, V]): ST[S, Unit] =
			xs.foldRight(ST[S, Unit](())):
				case ((k, v), st: ST[S, Unit]) =>
					st.flatMap(_ => write(k, v))

		def swap(k1: K, k2: K): ST[S, Unit] =
			for
				v1 <- read(k1)
				v2 <- read(k2)
				_  <- write(k1, v2)
				_  <- write(k2, v1)
			yield ()

	import scala.reflect.ClassTag

	object STHashMap:
		def empty[S, K, V]: ST[S, STHashMap[S, K, V]] =
			ST(new STHashMap(mutable.HashMap.empty))
