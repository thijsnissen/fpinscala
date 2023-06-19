object Chapter3 extends App:
	enum List[+A]:
		case Nil
		case Cons(head: A, tail: List[A])

	object List:
		def sum(ints: List[Int]): Int = ints match
			case Nil => 0
			case Cons(h, t) => h + sum(t)

		def product(ds: List[Double]): Double = ds match
			case Nil => 1.0
			case Cons(0.0, _) => 0.0
			case Cons(h, t) => h * product(t)

		def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
			as match
				case Nil => z
				case Cons(h, t) => f(h, foldRight(t, z)(f))

		def sum2(ns: List[Int]): Int =
			foldRight(ns, 0)((x, y) => x + y)

		def product2(ns: List[Double]): Double =
			foldRight(ns, 1.0)((x, y) => x * y)

		def apply[A](as: A*): List[A] =
			if as.isEmpty then Nil
			else Cons(as.head, apply(as.tail*))

		def append[A](a1: List[A], a2: List[A]): List[A] =
			a1 match
				case Cons(h, t) => Cons(h, append(t, a2))
				case Nil => a2

		// Exercise 3.1
		@annotation.nowarn
		val x: Any = List(1, 2, 3, 4, 5) match
			case Cons(x, Cons(2, Cons(4, _))) => x
			case Nil => 42
			case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
			case Cons(h, t) => h + sum(t)
			case _ => 101

		// Exercise 3.2
		def tail[A](ls: List[A]): List[A] =
			ls match
				case Cons(_, t) => t
				case Nil => Nil

		// Exercise 3.3
		def setHead[A](ls: List[A], h: A): List[A] =
			ls match
				case Cons(_, t) => Cons(h, t)
				case Nil => Cons(h, Nil)

		// Exercise 3.4
		@annotation.tailrec
		def drop[A](l: List[A], n: Int): List[A] =
			if n <= 0 then l
			else drop(tail(l), n - 1)

		// Exercise 3.5
		@annotation.tailrec
		def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
			l match
				case Cons(h, t) if f(h) => dropWhile(t, f)
				case _ => l

		@annotation.tailrec
		def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
			l match
				case Cons(h, t) if f(h) => dropWhile2(t)(f)
				case _ => l

		// Exercise 3.6
		def init[A](l: List[A]): List[A] =
			l match
				case Cons(_, Nil) => Nil
				case Cons(h, t) => Cons(h, init(t))
				case Nil => Nil

		// Exercise 3.7
		// Cannot be implemented, because foldRight will first recurse all the way
		// to the end before it starts analyzing and collapsing.

		// Exercise 3.8
		// FoldRight replaces Nil with z and Cos with f.
		// When z = Nil and f = Cons the original list is returned.

		// Exercise 3.9
		def length[A](as: List[A]): Int =
			foldRight(as, 0)((_, acc) => acc + 1)

		// Exercise 3.10
		@annotation.tailrec
		def foldLeft[A, B](as: List[A], acc: B)(f: (B, A) => B): B =
			as match
				case Nil => acc
				case Cons(h, t) => foldLeft(t, f(acc, h))(f)

		// Excersise 3.11
		def sum3(ns: List[Int]): Int =
			foldLeft(ns, 0)((x, y) => x + y)

		def product3(ns: List[Double]): Double =
			foldLeft(ns, 1.0)((x, y) => x * y)

		def length2[A](as: List[A]): Int =
			foldLeft(as, 0)((ac, _) => ac + 1)

		// Excersise 3.12
		def reverse[A](as: List[A]): List[A] =
			foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

		// Exercise 3.13
		def foldRightViaFoldLeft[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
			foldLeft(reverse(as), acc)((b, a) => f(a, b))

		def foldLeftViaFoldRight[A, B](l: List[A], acc: B)(f: (B, A) => B): B =
			foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(acc)

		// Excersise 3.14
		def append2[A](a1: List[A], a2: List[A]): List[A] =
			foldRight(a1, a2)((h, acc) => Cons(h, acc))

		// Exercise 3.15
		def concat[A](as: List[List[A]]): List[A] =
			foldRight(as, List[A]())(
				(h, acc) => foldRight(h, acc)(
					(h, acc) => Cons(h, acc)
				)
			)

		def concat2[A](as: List[List[A]]): List[A] =
			foldRight(as, List[A]())((h, acc) => append(h, acc))

		// Exercise 3.16
		def incrementBy1(as: List[Int]): List[Int] =
			foldRight(as, List[Int]())((h, a) => Cons(h + 1, a))

		// Exercise 3.17
		def doubleToString(as: List[Double]): List[String] =
			foldRight(as, List[String]())((h, a) => Cons(h.toString, a))

		// Exercise 3.18
		def map[A, B](as: List[A])(f: A => B): List[B] =
			foldRight(as, Nil: List[B])((h, a) => Cons(f(h), a))

		// Exercise 3.19
		def filter[A](as: List[A])(f: A => Boolean): List[A] =
			foldRight(as, Nil: List[A])((h, a) => if f(h) then Cons(h, a) else a)

		// Exercise 3.20
		def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
			foldRight(as, Nil: List[B])((h, a) => append(f(h), a))

		// Exercise 3.21
		def filterViaFlatmap[A](as: List[A])(f: A => Boolean): List[A] =
			flatMap(as)(i => if f(i) then List(i) else Nil)

		// Exercise 3.22
		def zipSum(a1: List[Int], a2: List[Int]): List[Int] =
			(a1, a2) match
				case (Nil, _) | (_, Nil) => Nil
				case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipSum(t1, t2))

		// Exercise 3.23
		@annotation.tailrec
		def zipWith[A, B, C](a1: List[A], a2: List[B], acc: List[C] = Nil)(f: (A, B) => C): List[C] =
			(a1, a2) match
				case (Nil, _) | (_, Nil) => reverse(acc)
				case (Cons(h1, t1), Cons(h2, t2)) => zipWith(t1, t2, Cons(f(h1, h2), acc))(f)

		// Exercise Take
		def take[A](as: List[A], n: Int): List[A] =
			@annotation.tailrec
			def go(ass: List[A], n: Int, acc: List[A] = List[A]()): List[A] =
				ass match
					case Nil => acc
					case Cons(_, _) if n <= 0 => acc
					case Cons(h, t) => go(t, n - 1, Cons(h, acc))

			reverse(go(as, n))

		// Exercise 3.24
		@annotation.tailrec
		def startsWith[A](sup: List[A], sub: List[A]): Boolean =
			(sup, sub) match
				case (_, Nil) => true
				case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
				case _ => false

		@annotation.tailrec
		def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
			sup match
				case Nil => sub == Nil
				case _ if startsWith(sup, sub) => true
				case Cons(_, t) => hasSubsequence(t, sub)

	enum Tree[+A]:
		case Leaf(value: A)
		case Branch(left: Tree[A], right: Tree[A])

	val aTree =
		Tree.Branch(
			Tree.Branch(
				Tree.Leaf("a"),
				Tree.Leaf("b")
			),
			Tree.Branch(
				Tree.Leaf("c"),
				Tree.Leaf("d")
			)
		)

	val aTreeOfInt=
		Tree.Branch(
			Tree.Branch(
				Tree.Leaf(3),
				Tree.Leaf(7)
			),
			Tree.Branch(
				Tree.Leaf(2),
				Tree.Branch(
					Tree.Leaf(5),
					Tree.Leaf(9)
				)
			)
		)

	object Tree:
		def fill[A](n: Int)(a: => A): Tree[A] =
			@annotation.tailrec
			def go(acc: Tree[A]): Tree[A] =
				if depth(acc) >= n then
					acc
				else
					acc match
						case l: Leaf[A] => go(Branch(l, l))
						case b: Branch[A] => go(Branch(b, b))

			go(Leaf(a))

		// Exercise 3.25
		def size[A](tree: Tree[A]): Int =
			tree match
				case Tree.Leaf(_) => 1
				case Tree.Branch(l, r) => 1 + size(l) + size(r)

		// Exercise 3.26
		def maximum(tree: Tree[Int]): Int =
			tree match
				case Tree.Leaf(n) => n
				case Tree.Branch(l, r) => maximum(l) max maximum(r)

		// Exercise3.27
		def depth[A](tree: Tree[A]): Int =
			tree match
				case Tree.Leaf(_) => 1
				case Tree.Branch(l, r) => 1 + depth(l) max depth(r)

		// Exercise 3.28
		def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
			tree match
				case Tree.Leaf(x) => Tree.Leaf(f(x))
				case Tree.Branch(l, r) => Tree.Branch(map(l)(f), map(r)(f))

		// Exercise 3.29
		def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
			tree match
				case Tree.Leaf(x) => f(x)
				case Tree.Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))

		def sizeViaFold[A](tree: Tree[A]): Int =
			fold(tree)(_ => 1)((x, y) =>  1 + x + y)

		def maximumViaFold(tree: Tree[Int]): Int =
			fold(tree)(identity)((x, y) => x max y)

		def depthViaFold[A](tree: Tree[A]): Int =
			fold(tree)(_ => 1)((x, y) => 1 + x max y)

		def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
			fold(tree)(x => Tree.Leaf(f(x)))((x, y) => Tree.Branch(x, y))

	// Example to show that List is not lazy evaluated as opposed to Stream (chapter 3/chapter 5).
	import List._

	def plusOne(n: Int): Int =
		println("plusOne: " + n)
		n + 1

	def smallerThan(i: Int): Boolean =
		println("smallerThan: " + i)
		i < 5

	// Invert binary tree
	import Tree._

	def invert[A](tree: Tree[A]): Tree[A] =
		def go(t: Tree[A]): Tree[A] =
			t match
				case Leaf(v) => Leaf(v)
				case Branch(l, r) =>  Branch(go(r), go(l))

		go(tree)

	invert(aTreeOfInt)
