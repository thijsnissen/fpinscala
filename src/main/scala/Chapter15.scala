object Chapter15 extends App:
	object P1:

		enum Pull[+O, +R]:
			case Result[+R](result: R) extends Pull[Nothing, R]
			case Output[+O](value: O) extends Pull[O, Unit]
			case FlatMap[X, +O, +R](source: Pull[O, X], f: X => Pull[O, R]) extends Pull[O, R]

			def step: Either[R, (O, Pull[O, R])] = this match
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
					case Left(r)         => (r, init)
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
					case Left(r)     => Result(Result(r))
					case Right(h, t) => if p(h) then t.dropWhile(p) else Result(Output(h) >> t)

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

			// Exervise 15.2
			def iterate[O](initial: O)(f: O => O): Pull[O, Nothing] =
				Output(initial) >> iterate(f(initial))(f)
	end P1
