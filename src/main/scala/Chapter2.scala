object Chapter2 extends App:
	private def factorial(n: Int): Int =
		@annotation.tailrec
		def go(n: Int, acc: Int = 1): Int =
			if n <= 0 then acc
			else go(n - 1, n * acc)

		go(n)

	assert(factorial(5) == 120)

	// Exercise 2.1
	private def fibonacci(n: Int): Int =
		@annotation.tailrec
		def go(n: Int, current: Int = 0, next: Int = 1): Int =
			if n <= 0 then current
			else go(n - 1, next, current + next)

		go(n)

	assert(fibonacci(4) == 3)

	// Exercise 2.2
	private def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
		@annotation.tailrec
		def go(n: Int): Boolean =
			if n >= as.length - 1 then true
			else if ! ordered(as(n), as(n + 1)) then false
			else go(n + 1)

		go(0)

	assert(isSorted(Array(1, 2, 3, 4), (a, b) => a < b))

	// Exercise 2.3
	def curry[A, B, C](f: (A, B) => C): A => (B => C) =
		(a: A) => (b: B) => f(a, b)

	// Exercise 2.4
	def uncurry[A, B, C](f: B => C): (A, B) => C =
		(a: A, b: B) => f(b)

	// Exercise 2.5
	def compose[A, B, C](f: B => C, g: A => B): A => C =
		(a: A) => f(g(a))

	import scala.util.chaining._

	private val f = (x: Double) => math.Pi / 2 - x

	private val cos = f andThen math.sin
	private val cos2 = math.sin(f(2))
	private val cos3 = 2.toDouble.pipe(f).pipe(math.sin)

	assert(cos(2) == cos2 && cos2 == cos3)
