import java.util.concurrent.atomic.AtomicReference

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
					def call: A = a(es).get(5, TimeUnit.SECONDS)

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
				(p, acc) => p.mapTwoTimeouts(acc)(_ :: _)

		// This implementation forks the recursive step off to a new logical thread
		def sequenceRecursive[A](ps: List[Par[A]]): Par[List[A]] =
			ps match
				case h :: t => h.mapTwoTimeouts(sequenceRecursive(t))(_ :: _)
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

					parallelCombination(l, init)(g)(f)
						.mapTwoTimeouts(parallelCombination(r, init)(g)(f))(f)

		def max(ints: IndexedSeq[Int]): Par[Int] =
			parallelCombination(ints, 0)(identity)(_ max _)

		def totalNoOfWords(ps: List[String]): Par[Int] =
			parallelCombination(ps.toIndexedSeq, "")(a => a.split(" ").length)(_ + _)

		def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
			p1(e).get == p2(e).get

		// Let's us delay instantiation of a computation until it is actually needed.
		def delay[A](fa: => Par[A]): Par[A] =
			es => fa(es)

		def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
			es =>
				if cond.run(es).get then t(es)
				else f(es)

    // Exercise 7.11
		def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
			val n = cond.map:
				case true => 1
				case false => 0

			choiceN(n)(List(f, t))

		def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
			es => choices(n.run(es).get)(es)

		// Exercise 7.12 & exercise 7.13
		def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
			es => choices(pa.run(es).get)(es)

		def choice3[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
			chooser(cond):
				case true => t
				case false => f

		def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
			chooser(n)(a => choices(a))

		def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
			chooser(key)(k => choices(k))

		// Exercise 7.14
		def join[A](a: Par[Par[A]]): Par[A] =
			es => a.run(es).get.run(es)

		def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
			join(a.map(f))

		def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
			flatMapViaJoin(a)(identity)

		// This function is different because ???
		def mapTwoViaFlatMapUnit[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
			pa.flatMap(a => pb.flatMap(b => unit(f(a, b))))

		// Par(Par(x)).flatten == join(Par(Par(x))) == Par(x)

		// ???

		extension [A](a: Par[A])
			def run(s: ExecutorService): Future[A] = a(s)

			def map[B](f: A => B): Par[B] =
				a.mapTwoTimeouts(unit(()))((a, _) => f(a))

			def mapTwo[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
				(es: ExecutorService) =>
					val af = a.run(es)
					val bf = b.run(es)

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

			def flatMap[B](f: A => Par[B]): Par[B] =
				(es: ExecutorService) => f(a.run(es).get)(es)

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

	object nonBlocking:
		import java.util.concurrent._

		opaque type Future[+A] = (A => Unit) => Unit

		opaque type Par[A] = ExecutorService => Future[A]

		object Par:
			def unit[A](a: A): Par[A] =
				(_: ExecutorService) => cb => cb(a)

			def lazyUnit[A](a: => A): Par[A] =
				fork(unit(a))

			def asyncF[A, B](f: A => B): A => Par[B] =
				a => lazyUnit(f(a))

			def fork[A](a: => Par[A]): Par[A] =
				(es: ExecutorService) => cb => eval(es)(a(es)(cb))

			def eval(es: ExecutorService)(r: => Unit): Unit =
				es.submit:
					new Callable[Unit]:
						def call: Unit = r

			def equal2[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
				p1.mapTwo(p2)(_ == _)

			def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
				fork(sequence(ps.map(asyncF(f))))

			def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
				sequenceBalanced(as.map(asyncF(f)))

			def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork:
				if as.isEmpty then unit(Vector())
				else if as.length == 1 then map(as.head)(a => Vector(a))
				else
					val (l, r) = as.splitAt(as.length / 2)
					sequenceBalanced(l).mapTwo(sequenceBalanced(r))(_ ++ _)

			def sequence[A](ps: List[Par[A]]): Par[List[A]] =
				ps.foldRight(unit(List.empty[A])):
					(p, acc) => p.mapTwo(acc)(_ :: _)

			extension [A](p: Par[A])
				def run(es: ExecutorService): A =
					val ref = new AtomicReference[A]
					val latch = new CountDownLatch(1)

					p(es): a =>
						ref.set(a)
						latch.countDown()

					latch.await()
					ref.get

				def map[B](f: A => B): Par[B] =
					(es: ExecutorService) => cb => p(es)(a => eval(es)(cb(f(a))))

				def mapTwo[B, C](p2: Par[B])(f: (A, B) => C): Par[C] =
					(es: ExecutorService) => cb =>
						var ar: Option[A] = None
						var br: Option[B] = None

						val combiner = Actor[Either[A, B]](es):
							case Left(a) => br match
								case None => ar = Some(a)
								case Some(b) => eval(es)(cb(f(a, b)))
							case Right(b) => ar match
								case None => br = Some(b)
								case Some(a) => eval(es)(cb(f(a, b)))

						p(es)(a => combiner ! Left(a))
						p2(es)(b => combiner ! Right(b))

				def flatMap[B](f: A => Par[B]): Par[B] =
					fork(es => cb => p(es)(a => f(a)(es)(cb)))

	import java.util.concurrent._
	import java.util.concurrent.atomic.AtomicInteger

	/*
	* Implementation is taken from `scalaz` library, with only minor changes. See:
	*
	* https://github.com/scalaz/scalaz/blob/scalaz-seven/concurrent/src/main/scala/scalaz/concurrent/Actor.scala
	*
	* This code is copyright Andriy Plokhotnyuk, Runar Bjarnason, and other contributors,
	* and is licensed using 3-clause BSD, see LICENSE file at:
	*
	* https://github.com/scalaz/scalaz/blob/scalaz-seven/etc/LICENCE
	*/

	/**
	 * Processes messages of type `A`, one at a time. Messages are submitted to
	 * the actor with the method `!`. Processing is performed asynchronously using the provided executor.
	 *
	 * Memory consistency guarantee: when each message is processed by the `handler`, any memory that it
	 * mutates is guaranteed to be visible by the `handler` when it processes the next message, even if
	 * the `executor` runs the invocations of `handler` on separate threads. This is achieved because
	 * the `Actor` reads a volatile memory location before entering its event loop, and writes to the same
	 * location before suspending.
	 *
	 * Implementation based on non-intrusive MPSC node-based queue, described by Dmitriy Vyukov:
	 * [[http://www.1024cores.net/home/lock-free-algorithms/queues/non-intrusive-mpsc-node-based-queue]]
	 *
	 * @see scalaz.concurrent.Promise for a use case.
	 * @param handler  The message handler
	 * @param onError  Exception handler, called if the message handler throws any `Throwable`.
	 * @param executor Execution strategy
	 * @tparam A The type of messages accepted by this actor.
	 */
	final class Actor[A](executor: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw (_)):
		self =>

		private val tail = new AtomicReference(new Node[A]())
		private val suspended = new AtomicInteger(1)
		private val head = new AtomicReference(tail.get)

		infix def !(a: A): Unit =
			val n = new Node(a)
			head.getAndSet(n).lazySet(n)
			trySchedule()

		def contramap[B](f: B => A): Actor[B] =
			new Actor[B](executor)((b: B) => this ! f(b), onError)

		private def trySchedule(): Unit =
			if suspended.compareAndSet(1, 0) then schedule()

		private def schedule(): Unit =
			executor.submit(new Callable[Unit] {
				def call: Unit = act()
			})
			()

		private def act(): Unit =
			val t = tail.get
			val n = batchHandle(t, 1024)
			if n ne t then
				n.a = null.asInstanceOf[A]
				tail.lazySet(n)
				schedule()
			else
				suspended.set(1)
				if n.get ne null then trySchedule()

		@annotation.tailrec
		private def batchHandle(t: Node[A], i: Int): Node[A] =
			val n = t.get
			if n ne null then
				try
					handler(n.a)
				catch
					case ex: Throwable => onError(ex)
				if i > 0 then batchHandle(n, i - 1) else n
			else t

	private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]
