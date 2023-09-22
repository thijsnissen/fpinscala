object Chapter15 extends App:
	import Part3Summary.Monoid
	import Part3Summary.Monad
	import Part3Summary.Monad.*

	object P1:

		enum Pull[+O, +R]:
			case Result[+R](result: R) extends Pull[Nothing, R]
			case Output[+O](value: O) extends Pull[O, Unit]
			case FlatMap[X, +O, +R](source: Pull[O, X], f: X => Pull[O, R]) extends Pull[O, R]

			final def step: Either[R, (O, Pull[O, R])] = this match
				case Result(r)          => Left(r)
				case Output(o)          => Right(o, Pull.done)
				case FlatMap(source, f) => source match
					case FlatMap(s2, g) => s2.flatMap(x => g(x).flatMap(y => f(y))).step
					case other          => other.step match
						case Left(r)         => f(r).step
						case Right((hd, tl)) => Right((hd, tl.flatMap(f)))

			@annotation.tailrec
			final def fold[A](init: A)(f: (A, O) => A): (R, A) =
				step match
					case Left(r)       => (r, init)
					case Right((h, t)) => t.fold(f(init, h))(f)

			def toList: List[O] =
				fold(List.newBuilder[O])((bldr, o) => bldr += o)(1).result

			def flatMap[O2 >: O, R2](f: R => Pull[O2, R2]): Pull[O2, R2] =
				FlatMap(this, f)

			@annotation.targetName("concat")
			def >>[O2 >: O, R2](next: => Pull[O2, R2]): Pull[O2, R2] =
				flatMap(_ => next)

			def map[R2](f: R => R2): Pull[O, R2] =
				flatMap(r => Result(f(r)))

			def repeat: Pull[O, Nothing] =
				this >> repeat

			def take(n: Int): Pull[O, Option[R]] =
				if n <= 0 then Result(None)
				else uncons.flatMap:
					case Left(r)     => Result(Some(r))
					case Right(h, t) => Output(h) >> t.take(n - 1)

			def uncons: Pull[Nothing, Either[R, (O, Pull[O, R])]] =
				Pull.done >> Result(step)

			def drop(n: Int): Pull[O, R] =
				if n <= 0 then this
				else uncons.flatMap:
					case Left(r)     => Result(r)
					case Right(_, t) => t.drop(n - 1)

			// Exercise 15.3
			def takeWhile(p: O => Boolean): Pull[O, Pull[O, R]] =
				uncons.flatMap:
					case Left(r)     => Result(Result(r))
					case Right(h, t) => if p(h) then Output(h) >> t.takeWhile(p) else Result(Output(h) >> t)

			def dropWhile(p: O => Boolean): Pull[Nothing, Pull[O, R]] =
				uncons.flatMap:
					case Left(r: R)                 => Result(Result(r))
					case Right(h: O, t: Pull[O, R]) => if p(h) then t.dropWhile(p) else Result(Output(h) >> t)

			def mapOutput[O2](f: O => O2): Pull[O2, R] =
				uncons.flatMap:
					case Left(r)     => Result(r)
					case Right(h, t) => Output(f(h)) >> t.mapOutput(f)

			def filter(p: O => Boolean): Pull[O, R] =
				uncons.flatMap:
					case Left(r)     => Result(r)
					case Right(h, t) =>
						(if p(h) then Output(h) else Pull.done) >> t.filter(p)

			def count: Pull[Int, R] =
				def go(total: Int, p: Pull[O, R]): Pull[Int, R] =
					p.uncons.flatMap:
						case Left(r)     => Result(r)
						case Right(_, t) => Output(total + 1) >> go(total + 1, t)

				Output(0) >> go(0, this)

			// Exercise 15.4
			def tally[O2 >: O](using m: Monoid[O2]): Pull[O2, R] =
				def loop(total: O2, p: Pull[O, R]): Pull[O2, R] =
					p.uncons.flatMap:
						case Left(r)     => Result(r)
						case Right(h, t) =>
							val newTotal = m.combine(total, h)

							Output(newTotal) >> loop(newTotal, t)

				Output(m.zero) >> loop(m.zero, this)

			def mapAccumulate[S, O2](init: S)(f: (S, O) => (S, O2)): Pull[O2, (S, R)] =
				uncons.flatMap:
					case Left(r)     => Result((init, r))
					case Right(h, t) =>
						val (s, out) = f(init, h)

						Output(out) >> t.mapAccumulate(s)(f)

			// Exercise 15.6
			def countViaMapAcc: Pull[Int, R] =
				Output(0) >> mapAccumulate(0)((s, _) => (s + 1, s + 1)).map(_._2)

			def tallyViaMapAcc[O2 >: O](using m: Monoid[O2]): Pull[O2, R] =
				Output(m.zero) >> mapAccumulate(m.zero):
					(s: O2, o: O) =>
						val s2 = m.combine(s, o)

						(s2, s2)
				.map(_._2)

		object Pull:
			val done: Pull[Nothing, Unit] = Result(())

			def fromList[O](os: List[O]): Pull[O, Unit] =
				os match
					case Nil    => done
					case h :: t => Output(h) >> fromList(t)

			def fromLazyList[O](os: LazyList[O]): Pull[O, Unit] =
				os match
					case LazyList() => done
					case h #:: t    => Output(h) >> fromLazyList(t)

			def unfold[O, R](init: R)(f: R => Either[R, (O, R)]): Pull[O, R] =
				f(init) match
					case Left(r)        => Result(r)
					case Right((o, r2)) => Output(o) >> unfold(r2)(f)

			// Exercise 15.1
			def fromListViaUnfold[O](os: List[O]): Pull[O, Unit] =
				unfold(os):
					case Nil    => Left(Nil)
					case h :: t => Right(h, t)
				.map(_ => ())

			def fromLazyListViaUnfold[O](os: LazyList[O]): Pull[O, Unit] =
				unfold(os):
					case LazyList() => Left(LazyList())
					case h #:: t    => Right(h, t)
				.map(_ => ())

			def continually[O](o: O): Pull[O, Nothing] =
				Output(o).repeat

			// Exercise 15.2
			def iterate[O](initial: O)(f: O => O): Pull[O, Nothing] =
				Output(initial) >> iterate(f(initial))(f)

			// Exercise 15.4
			import scala.collection.immutable.Queue

			extension[R] (self: Pull[Int, R])
				def slidingMean(n: Int): Pull[Double, R] =
					def loop(window: Queue[Int], p: Pull[Int, R]): Pull[Double, R] =
						p.uncons.flatMap:
							case Left(r)     => Result(r)
							case Right(h, t) =>
								val newWindow       = if window.size < n then window :+ h else window.tail :+ h
								val meanOfNewWindow = newWindow.sum / newWindow.size.toDouble

								Output(meanOfNewWindow) >> loop(newWindow, t)

					loop(Queue.empty[Int], self)

			extension[O] (self: Pull[O, Unit])
				def toStream: Stream[O] = self

				def flatMapOutput[O2](f: O => Pull[O2, Unit]): Pull[O2, Unit] =
					self.uncons.flatMap:
						case Left(()) => Result(())
						case Right((hd, tl)) =>
							f(hd) >> tl.flatMapOutput(f)

			given [O]: Monad[[x] =>> Pull[O, x]] with
				def unit[A](a: => A): Pull[O, A] = Result(a)

				def flatMap[A, B](fa: Pull[O, A])(f: A => Pull[O, B]): Pull[O, B] =
					fa.flatMap(f)

		opaque type Stream[+O] = Pull[O, Unit]

		object Stream:
			import Pull.*
			import Pull.given

			def apply[O](os: O*): Stream[O] =
				Pull.fromList(os.toList).toStream

			extension[O] (self: Stream[O])
				def toPull: Pull[O, Unit] = self

				def fold[A](init: A)(f: (A, O) => A): A =
					self.fold(init)(f)(1)

				def toList: List[O] =
					self.toList

				def take(n: Int): Stream[O] =
					self.take(n).map(_ => ())

				@annotation.targetName("concat")
				def ++(that: => Stream[O]): Stream[O] =
					self >> that

			given Monad[Stream] with
				def unit[A](a: => A): Stream[A] = Output(a)

				def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
					fa.uncons.flatMap:
						case Left(()) => Result(())
						case Right(h, t) =>
							f(h) >> t.flatMapOutput(f)

		type Pipe[-I, +O] = Stream[I] => Stream[O]

		//object Pipe:
		//	import P1.Stream
		//	import Stream.*
		//
		//	import Pull.*
		//	import Pull.given
		//
		//	val nonEmpty: Pipe[String, String] =
		//		_.filter(_.nonEmpty)
		//
		//	val lowerCase: Pipe[String, String] =
		//		_.map(_.toLowerCase)
		//
		//	val normalize: Pipe[String, String] =
		//		nonEmpty andThen lowerCase
		//
		//	def exists[I](f: I => Boolean): Pipe[I, Boolean] =
		//		src => src.map(f).toPull.tally(using Monoid.booleanOr).toStream
		//
		//	def count[A]: Pipe[A, Int] =
		//		_.toPull.count.map(_ => ()).toStream
		//
		//	import scala.util.chaining.scalaUtilChainingOps
		//
		//	import Monads.IO
		//	import Monads.IO.*
		//
		//	def fromIterator[O](itr: Iterator[O]): Stream[O] =
		//		Pull
		//			.unfold(itr):
		//				itr =>
		//					if itr.hasNext then Right((itr.next(), itr))
		//					else Left(itr)
		//			.map(_ => ())
		//			.toStream
		//
		//	def processFile[A](file: java.io.File, p: Pipe[String, A])(using m: Monoid[A]): IO[A] =
		//		IO.ioMonad.unit:
		//			val source = scala.io.Source.fromFile(file)
		//			try fromIterator(source.getLines).pipe(p).fold(m.zero)(m.combine)
		//			finally source.close()
		//
		//	import Part3Summary.Monoid
		//
		//	val booleanOr: Monoid[Boolean] = new Monoid[Boolean]:
		//		def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
		//
		//		def zero: Boolean = false
		//
		//	def checkFileForGt40K(file: java.io.File): IO[Boolean] =
		//		processFile(file, count andThen exists(_ > 40000))(using booleanOr)
		//end Pipe
	end P1

	object P2:

	end P2
