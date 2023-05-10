object Chapter5 extends App:
	enum Stream[+A]:
		import Stream._

		case Empty
		case Cons(h: () => A, t: () => Stream[A])

		def headOption: Option[A] =
			this match
				case Empty => None
				case Cons(h, _) => Some(h())

		// Exercise 5.1
		def toList: List[A] =
			this match
				case Empty => Nil
				case Cons(h, t) => h() :: t().toList

		def toListRecursive: List[A] =
			@annotation.tailrec
			def go(ls: Stream[A], acc: List[A] = Nil): List[A] =
				ls match
					case Empty => acc.reverse
					case Cons(h, t) => go(t(), h() :: acc)

			go(this)

		// Exercise 5.2
		def take(n: Int): Stream[A] =
			this match
				case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
				case Cons(h, _) if n == 1 => cons(h(), empty)
				case _ => empty

		@annotation.tailrec
		final def drop(n: Int): Stream[A] =
			this match
				case Cons(_, t) if n > 0 => t().drop(n - 1)
				case _ => this

		// Exercise 5.3
		def takeWhile(p: A => Boolean): Stream[A] =
			this match
				case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
				case _ => empty

		@annotation.tailrec
		final def exists(p: A => Boolean): Boolean =
			this match
				case Cons(h, t) => p(h()) || t().exists(p)
				case _ => false

		def existsViaFoldRight(p: A => Boolean): Boolean =
			this.foldRight(false)((a, b) => p(a) || b)

		def foldRight[B](z: => B)(f: (A, => B) => B): B =
			this match
				case Cons(h, t) => f(h(), t().foldRight(z)(f))
				case Empty => z

		// Exercise 5.4
		def forAll(p: A => Boolean): Boolean =
			this match
				case Cons(h, t) => p(h()) && t().forAll(p)
				case _ => true

		def forAllViaFoldRight(p: A => Boolean): Boolean =
			this.foldRight(true)((a, b) => p(a) && b)

		// Exercise 5.5
		def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
			this.foldRight(empty)((a, b) => if p(a) then cons(a, b) else empty)

		// Exercise 5.6
		def headOptionViaFoldRight: Option[A] =
			this.foldRight(None: Option[A])((a, _) => Some(a))

		// Exercise 5.7
		def map[B](f: A => B): Stream[B] =
			this.foldRight(empty)((a, b) => cons(f(a), b))

		def filter(f: A => Boolean): Stream[A] =
			this.foldRight(empty)((a, b) => if f(a) then cons(a, b) else b)

		def append[AA >: A](that: Stream[AA]): Stream[AA] =
			this.foldRight(that)((a, b) => cons(a, b))

		def flatMap[B](f: A => Stream[B]): Stream[B] =
			this.foldRight(empty)((a, b) => f(a).append(b))

		def find(p: A => Boolean): Option[A] =
			filter(p).headOption

		// Exercise 5.13
		def mapViaUnfold[B](f: A => B): Stream[B] =
			unfold(this):
				case Cons(h, t) => Some((f(h()), t()))
				case _ => None

		def takeViaUnfold(n: Int): Stream[A] =
			unfold((this, n)):
				case (Cons(h, t), n) if n >= 1 => Some((h(), (t(), n - 1)))
				case _ => None

		def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
			unfold(this):
				case Cons(h, t) if f(h()) => Some((h(), t()))
				case _ => None

		def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
			unfold((this, that)):
				case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
				case _ => None

		def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
			unfold((this, that)):
				case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
				case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
				case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
				case (Empty, Empty) => None

		def hasSubsequence[B](that: Stream[B]): Boolean =
			(this, that) match
				case (Empty, _) => that == Empty
				case (ti, ta) if ti.startsWith(ta) => true
				case (Cons(_, t), ta) => t().hasSubsequence(ta)

		def hasSubsequence2[B](s: Stream[B]): Boolean =
			tails exists (_ startsWith2 s)

		// Exercise 5.14
		def startsWith[B](that: Stream[B]): Boolean =
			(this, that) match
				case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
				case (_, Empty) => true
				case _ => false

		def startsWith2[B](that: Stream[B]): Boolean =
			this.zipAll(that).takeWhile(_._2.isDefined).forAll(x => x._1 == x._2)

		// Exercise 5.15
		def tails: Stream[Stream[A]] =
			unfold(this):
				case Cons(h, t) => Some((cons(h(), t()), t()))
				case Empty => None

		// Exercise 5.16
		def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
			this.foldRight((z, cons(z, empty[B]))):
				case (a, (b, acc)) =>
					val next = f(a, b)
					(next, cons(next, acc))
			._2

	object Stream:
		def cons[A](hd: => A, tl: Stream[A]): Stream[A] =
			lazy val head = hd
			lazy val tail = tl

			Cons(() => head, () => tail)

		def empty[A]: Stream[A] =
			Empty

		def apply[A](as: A*): Stream[A] =
			if as.isEmpty then empty
			else cons(as.head, apply(as.tail *))

		// Exercise 5.8
		@annotation.nowarn
		def constant[A](a: A): Stream[A] =
			lazy val s: Stream[A] = cons(a, s)
			s

		// Exercise 5.9
		def from(n: Int): Stream[Int] =
			cons(n, from(n + 1))

		// Exercise 5.10
		def fibs(current: Int = 0, next: Int = 1): Stream[Int] =
			cons(current, fibs(next + current))

		// Exercise 5.11
		def unfold[A, S](state: S)(f: S => Option[(A, S)]): Stream[A] =
			f(state) match
				case Some(a, s) => cons(a, unfold(s)(f))
				case None => empty

		def unfoldViaFold[A, S](state: S)(f: S => Option[(A, S)]): Stream[A] =
			f(state).fold(empty)((a, s) => cons(a, unfoldViaMap(s)(f)))

		def unfoldViaMap[A, S](state: S)(f: S => Option[(A, S)]): Stream[A] =
			f(state).map((a, s) => cons(a, unfoldViaMap(s)(f))).getOrElse(empty)

		// Exercise 5.12
		def constantViaUnfold[A](a: A): Stream[A] =
			unfold(a)(s => Some((a, s)))

		def fromViaUnfold(n: Int): Stream[Int] =
			unfoldViaFold(n)(s => Some((s, s + 1)))

		def fibsViaUnfold(current: Int = 0, next: Int = 1): Stream[Int] =
			unfoldViaMap((current, next))(s => Some((s._1, (s._2, s._1 + s._2))))

	// Example to show that List is not lazy evaluated as opposed to Stream (chapter 3/chapter 5).
	def plusOne(n: Int): Int =
		println("plusOne: " + n)
		n + 1

	def smallerThan(i: Int): Boolean =
		println("smallerThan: " + i)
		i < 5
