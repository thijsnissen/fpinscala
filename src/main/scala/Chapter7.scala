object Chapter7 extends App:
	object APIExplorations:
		def unit[A](a: => A): APIExploration [A] =
			APIExploration(a)

		def lazyUnit[A](a: => A): APIExploration[A] =
			fork(unit(a))

		def run[A](a: APIExploration[A]): A =
			a.value

		def mapTwo[A, B, C](a: APIExploration[A], b: APIExploration[B])(f: (A, B) => C): APIExploration[C] =
			unit(f(run(a), run(b)))

		def fork[A](a: => APIExploration[A]): APIExploration[A] =
			a

		// Exercise 7.1
		def sum1(ints: IndexedSeq[Int]): Int =
			if ints.size <= 1 then
				ints.headOption.getOrElse(0)
			else
				val (l, r) = ints.splitAt(ints.length / 2)
				val sumL: APIExploration[Int] = unit(sum1(l))
				val sumR: APIExploration[Int] = unit(sum1(r))

				run(sumL) + run(sumR)

		def sum2(ints: IndexedSeq[Int]): APIExploration[Int] =
			if ints.size <= 1 then
				unit(ints.headOption.getOrElse(0))
			else
				val (l, r) = ints.splitAt(ints.length / 2)

				mapTwo(sum2(l), sum2(r))(_ + _)

		def sum3(ints: IndexedSeq[Int]): APIExploration[Int] =
			if ints.length <= 1 then
				APIExplorations.unit(ints.headOption.getOrElse(0))
			else
				val (l, r) = ints.splitAt(ints.length / 2)

				APIExplorations.mapTwo(APIExplorations.fork(sum3(l)), APIExplorations.fork(sum3(r)))(_ + _)

		// Exercise 7.2
		case class APIExploration[A](value: A)

	object Par:
		import java.util.concurrent._

		opaque type Par[A] = ExecutorService => Future[A]

		private case class UnitFuture[A](get: A) extends Future[A]:
			def get(timeout: Long, units: TimeUnit): A = get
			def cancel(evenIfRunning: Boolean): Boolean = false
			def isCancelled: Boolean = false
			def isDone: Boolean = true

		def unit[A](a: A): Par[A] =
			(_: ExecutorService) => UnitFuture(a)

		def lazyUnit[A](a: => A): Par[A] =
			fork(unit(a))

		def fork[A](a: => Par[A]): Par[A] =
			es => es.submit:
				new Callable[A]:
					def call: A = a(es).get

		// Exercise 7.4
		def asyncF[A, B](f: A => B): A => Par[B] =
			a => lazyUnit(f(a))

		def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
			parList.map(_.sorted)

		// Note that we've wrapped our implementation in a call to 'fork'. With this
		// implementation, parMap will return immediately, even for a huge input list.
		// When we later call run, it will fork a single asynchronous computation
		// which itself spawns N parallel computations, and then waits for these
		// computations to finish, collecting their result into a list.
		def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
			fork(sequence(ps.map(asyncF(f))))

		// This is wrong because it does not do the computation part in parallel and only wraps the result after the fact.
		// def parMap2[A, B](ps: List[A])(f: A => B): Par[List[B]] =
		// 	unit(ps.map(f))

		def sequence[A](ps: List[Par[A]]): Par[List[A]] =
			ps.foldRight(unit(List.empty[A])):
				(p, acc) => p.mapTwo(acc)(_ :: _)

		// This implementation forks the recursive step off to a new logical thread
		def sequenceRecursive[A](ps: List[Par[A]]): Par[List[A]] =
			ps match
				case h :: t => h.mapTwo(fork(sequenceRecursive(t)))(_ :: _)
				case Nil    => unit(Nil)

		def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
			fork:
				sequenceRecursive:
					as.map(asyncF(a => if f(a) then List(a) else Nil))
				.map:
					_.flatten

		def parallelCombination[A, B](is: IndexedSeq[A], init: A)(g: A => B)(f: (B, B) => B): Par[B] =
			fork:
				if is.length <= 1 then
					unit(g(is.headOption.getOrElse(init)))
				else
					val (l, r) = is.splitAt(is.length / 2)

					fork(parallelCombination(l, init)(g)(f))
						.mapTwo(fork(parallelCombination(r, init)(g)(f)))(f)

		def max(ints: IndexedSeq[Int]): Par[Int] =
			parallelCombination(ints, 0)(identity)(_ max _)

		def totalNoOfWords(ps: List[String]): Par[Int] =
			parallelCombination(ps.toIndexedSeq, "")(a => a.split(" ").length)(_ + _)

		def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
			p1(e).get == p2(e).get

		def delay[A](fa: => Par[A]): Par[A] =
			es => fa(es)

		extension [A](a: Par[A])
			def run(s: ExecutorService): Future[A] = a(s)

			def map[B](f: A => B): Par[B] =
				a.mapTwo(unit(()))((a, _) => f(a))

			def mapTwo[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
				(es: ExecutorService) =>
					val af = a(es)
					val bf = b(es)

					UnitFuture(f(af.get, bf.get))

			// Exercise 7.3
			def mapTwoTimeouts[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
				(es: ExecutorService) => new Future[C]:
					private val af = a(es)
					private val bf = b(es)

					def get: C = get(Long.MaxValue, TimeUnit.NANOSECONDS)
					def cancel(evenIfRunning: Boolean): Boolean = false
					def isCancelled: Boolean = false
					def isDone: Boolean = true

					def get(timeout: Long, units: TimeUnit): C =
						val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
						val started = System.nanoTime
						val af2 = af.get(timeoutNanos, TimeUnit.NANOSECONDS)
						val elapsed = System.nanoTime - started
						val bf2 = bf.get(timeoutNanos - elapsed, TimeUnit.NANOSECONDS)

						f(af2, bf2)

	// Exercise 7.7
	// map(unit(x))(f) == unit(f(x)) initial law
	// map(unit(x))(id) == unit(id(x))
	// map(unit(x))(id) == unit(x)
	//
	// given: map(y)(id) == y
	// prove: map(map(y)(g))(f) == map(y)(f compose g)
	//
	// map(map(y)(g))(f) == (???)
	// map(g(y))(f) ==
	// map(y)(g(f)) ==
	// map(y)(f compose g)

	// Exercise 7.9
	val a2 = Par.lazyUnit(42 + 1)
	val S2 = java.util.concurrent.Executors.newFixedThreadPool(2)
	println(Par.equal(S2)(a2, Par.fork(a2)))
	S2.shutdown()

	object nonBlockingPar:
		???
