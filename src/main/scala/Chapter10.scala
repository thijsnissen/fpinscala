object Chapter10 extends App:
	trait Semigroup[A]:
		def combine(a1: A, a2: A): A

	trait Monoid[A] extends Semigroup[A]:
		def zero: A

	object Monoid:
		val stringMonoid: Monoid[String] = new Monoid[String]:
			def combine(a1: String, a2: String): String = a1 + a2
			def zero: String = ""

		val charMonoid: Monoid[Char] = new Monoid[Char]:
			def combine(a1: Char, a2: Char): Char = (a1 + a2).toChar

			def zero: Char = 0.toChar

		def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]]:
			def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
			def zero: List[A] = Nil

		// Exercise 10.1
		val intAddition: Monoid[Int] = new Monoid[Int]:
			def combine(a1: Int, a2: Int): Int = a1 + a2
			def zero: Int = 0

		val intMultiplication: Monoid[Int] = new Monoid[Int]:
			def combine(a1: Int, a2: Int): Int = a1 * a2
			def zero: Int = 1

		val booleanOr: Monoid[Boolean] = new Monoid[Boolean]:
			def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
			def zero: Boolean = false

		val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]:
			def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
			def zero: Boolean = true

		// Exercise 10.2
		def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]:
			def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
			def zero: Option[A] = None

		def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A]:
			def combine(x: A, y: A): A = m.combine(y, x)
			def zero: A = m.zero

		def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid

		def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

		def combineOptionMonoid[A](f: (A, A) => A): Monoid[Option[A]] = new Monoid[Option[A]]:
			def combine(a1: Option[A], a2: Option[A]): Option[A] = a1.mapTwo(a2)(f)
			def zero: Option[A] = None

		extension [A](self: Option[A])
			def mapTwo[B, C](that: Option[B])(f: (A, B) => C): Option[C] =
				for
					a <- self
					b <- that
				yield
					f(a, b)

		// Exercise 10.3
		def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]:
			def combine(a1: A => A, a2: A => A): A => A = a1 compose a2
			def zero: A => A = identity

		// Exercise 10.4
		import Chapter8.Gen.*
		import Chapter8.Prop
		import Chapter8.Prop.*

		def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
			val associativity = Prop
				.forAll(gen ** gen ** gen):
					case a ** b ** c =>
						m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
				.tag(FailedCase.fromString("associativity"))

			val identity = Prop
				.forAll(gen): a =>
					m.combine(a, m.zero) == a && m.combine(m.zero, a) == a
				.tag(FailedCase.fromString("identity"))

			associativity && identity

		// Exercise 10.5
    // The signature of foldLeft/Right perfetely fits the operations and laws of a Monoid.
		// Type A forms a monoid under the operations defined by the Monoid[A] instance.
		def concatenate[A](as: List[A], m: Monoid[A]): A =
			as.foldLeft(m.zero)(m.combine)

		def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
			as.foldLeft(m.zero)((b, a) => m.combine(b, f(a)))

		def foldMapViaConcatenate[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
			concatenate(as.map(f), m)

		// Exercise 10.6
		def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
			foldMap(as, endoMonoid)(a => b => f(b, a))(z)

		def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
			foldMap(as, endoMonoid)(a => b => f(a, b))(z)

		// Exercise 10.7
		def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
			v.length match
				case 0 => m.zero
				case 1 => f(v.head)
				case _ =>
					val (l, r) = v.splitAt(v.length / 2)

					m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

		// Exercise 10.8
		import Chapter7.nonBlocking.Par

		def parMonoid[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]:
			def combine(a1: Par[A], a2: Par[A]): Par[A] = a1.mapTwo(a2)(m.combine)

			def zero: Par[A] = Par.unit(m.zero)

		def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
			Par.parMap(as)(f).flatMap: bs =>
				foldMapV(bs, parMonoid(m))(b => Par.lazyUnit(b))

		// Exercise 10.9
		case class Interval(ordered: Boolean, min: Int, max: Int)

		//def isOrderedMonoid2: Monoid[Option[Interval]] = new Monoid[Option[Interval]]:
		//	def combine(oa1: Option[Interval], oa2: Option[Interval]): Option[Interval] =
		//		(oa1, oa2) match
		//			case (Some(a1), Some(a2)) =>
		//				Some(Interval(a1.ordered && a2.ordered && a1.max <= a2.min, a1.min, a2.max))
		//			case (None, x) => x
		//			case (x, None) => x
		//
		//	def zero: Option[Interval] = None
		//
		//def isOrdered2(as: IndexedSeq[Int]): Boolean =
		//	foldMapV(as, isOrderedMonoid2):
		//		(i: Int) => Some(Interval(true, i, i))
		//	.forall(_.ordered)

		def isOrderedMonoid: Monoid[Interval] = new Monoid[Interval]:
			def combine(a1: Interval, a2: Interval): Interval =
				Interval(a1.ordered && a2.ordered && a1.max <= a2.min, a1.min, a2.max)

			def zero: Interval = Interval(true, 0, 0)

		def isOrdered(as: IndexedSeq[Int]): Boolean =
			foldMapV(as, isOrderedMonoid):
				(i: Int) => Interval(true, i, i)
			.ordered

		// Exercise 10.10 & Exercise 10.11
		enum WordCount:
			case Stub(chars: String)
			case Part(lStub: String, words: Int, rStub: String)

		def wordCountMonoid: Monoid[WordCount] = new Monoid[WordCount]:
			def combine(a1: WordCount, a2: WordCount): WordCount =
				(a1, a2) match
					case (WordCount.Stub(x), WordCount.Stub(y))       => WordCount.Stub(x + y)
					case (WordCount.Stub(x), WordCount.Part(y, z, w)) => WordCount.Part(x + y, z, w)
					case (WordCount.Part(x, y, z), WordCount.Stub(w)) => WordCount.Part(x, y, z + w)
					case (WordCount.Part(x1, y1, z1), WordCount.Part(x2, y2, z2)) =>
						WordCount.Part(x1, y1 + (if (z1 + x2).isEmpty then 0 else 1) + y2 ,z2)

			def zero: WordCount = WordCount.Stub("")

		def wordCount(s: String): Int =
			def f(c: Char): WordCount =
				if c.isWhitespace then
					WordCount.Part("", 0, "")
				else
					WordCount.Stub(c.toString)

			def unstub(s: String) =
				if s.isEmpty then 0 else 1

			foldMapV(s.toIndexedSeq, wordCountMonoid)(f) match
				case WordCount.Stub(s)       => unstub(s)
				case WordCount.Part(x, y, z) => unstub(x) + y + unstub(z)

		object Foldable:
			trait Foldable[F[_]]:
				def foldMap[A, B](as: F[A], m: Monoid[B])(f: A => B): B =
					foldLeft(as, m.zero)((b, a) => m.combine(f(a), b))

				def foldRight[A, B](as: F[A], z: B)(f: (A, B) => B): B =
					foldMap(as, endoMonoid[B])(f.curried)(z)

				def foldLeft[A, B](as: F[A], z: B)(f: (B, A) => B): B =
					foldMap(as, endoMonoid[B])((a: A) => (b: B) => f(b, a))(z)

				def concat[A](as: F[A], m: Monoid[A]): A =
					foldLeft(as, m.zero)(m.combine)

				// Exercise 10.15
				def toList[A](fa: F[A]): List[A] =
					foldRight(fa, List.empty[A])(_ :: _)

			// Exercise 10.12
			import Chapter3.List
			import Chapter3.List.Nil
			import Chapter3.List.Cons

			given listFoldable: Foldable[List] with
				override def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
					as match
						case Cons(h, t) => m.combine(f(h), foldMap(t, m)(f))
						case Nil        => m.zero

			// Exercise 10.13
			import Chapter3.Tree
			import Chapter3.Tree.Leaf
			import Chapter3.Tree.Branch

			given treeFoldable: Foldable[Tree] with
				override def foldMap[A, B](as: Tree[A], m: Monoid[B])(f: A => B): B =
					as match
						case Branch(l, r) => m.combine(foldMap(l, m)(f), foldMap(r, m)(f))
						case Leaf(a)      => f(a)

			// Exercise 10.14
			import Chapter4.Option
			import Chapter4.Option.Some
			import Chapter4.Option.None

			given optionFoldable: Foldable[Option] with
				override def foldMap[A, B](as: Option[A], m: Monoid[B])(f: A => B): B =
					as match
						case Some(a) => f(a)
						case None    => m.zero

		end Foldable

		// Exercise 10.16
		def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
			new Monoid[(A, B)]:
				def combine(ab1: (A, B), ab2: (A, B)): (A, B) =
					(a.combine(ab1._1, ab2._1), b.combine(ab1._2, ab2._2))

				def zero: (A, B) = (a.zero, b.zero)

		def mapMergeMonoid[K, V](mv: Monoid[V]): Monoid[Map[K, V]] =
			new Monoid[Map[K, V]]:
				def combine(m1: Map[K, V], m2: Map[K, V]): Map[K, V] =
					(m1.keySet ++ m2.keySet).foldLeft(zero):
						(acc, k) =>
							val v = mv.combine(m1.getOrElse(k, mv.zero), m2.getOrElse(k, mv.zero))

							acc.updated(k, v)

				def zero: Map[K, V] = Map.empty[K, V]

		// Exercise 10.17
		def fuctionMonoid[A, B](b: Monoid[B]): Monoid[A => B] =
			new Monoid[A => B]:
				def combine(a1: A => B, a2: A => B): A => B =
					(a: A) => b.combine(a1(a), a2(a))

				def zero: A => B =
					(a: A) => b.zero

		// Exercise 10.18
		def bag[A](as: Chapter3.List[A]): Map[A, Int] =
			Foldable
				.listFoldable
				.foldMap(as, mapMergeMonoid(intAddition)):
					a => Map(a -> 1)
