object Monad extends App:
	def run[A, B](a: A => B)(b: A): B =
		a(b)

	def unit[A, B](b: B): A => B =
		(a: A) => b

	def flatMap[A, B](a: A)(f: A => B): B =
		???

	def map[A, B](a: A)(f: A => B): B =
		???

	def mapTwo[A, B, C](a: A)(b: B)(f: (A, B) => C): C =
		???

	// def sequence[A](ps: List[Par[A]]): Par[List[A]]
	//
	// def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]
	//
	// def flatten
