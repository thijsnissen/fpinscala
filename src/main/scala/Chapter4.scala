object Chapter4 extends App:
	enum Option[+A]:
		case Some(get: A)
		case None

		// Excercise 4.1
		def map[B](f: A => B): Option[B] =
			this match
				case None => None
				case Some(x) => Some(f(x))

		def flatMap[B](f: A => Option[B]): Option[B] =
			// this.map(f).getOrElse(None)
			this match
				case None => None
				case Some(x) => f(x)

		def getOrElse[B >: A](default: => B): B =
			this match
				case None => default
				case Some(x) => x

		def orElse[B >: A](ob: => Option[B]): Option[B] =
			// this.map(Some(_)).getOrElse(ob)
			this match
				case None => ob
				case _ => this

		def filter(f: A => Boolean): Option[A] =
			// this.flatMap(x => if f(x) then Some(x) else None)
			this match
				case Some(x) if f(x) => this
				case _ => None

		def contains[B](x: B): Boolean =
			this match
				case Some(v) if v == x => true
				case _ => false

	object Option:
		// Exercise 4.2
		def mean(xs: Seq[Double]): Option[Double] =
			if xs.isEmpty then None
			else Some(xs.sum / xs.length)

		def variance(xs: Seq[Double]): Option[Double] =
			mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

		def lift[A, B](f: A => B): Option[A] => Option[B] =
			x => x.map(f)

		def absO: Option[Double] => Option[Double] =
			lift(math.abs)

		// Exercise 4.3
		def mapTwo[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
			// a.flatMap(x => b.map(y => f(x, y)))
			(a, b) match
				case (Some(x), Some(y)) => Some(f(x, y))
				case _ => None

		def mapTwo2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
			for
				aa <- a // flatmap
				bb <- b // last one map
			yield
				f(aa, bb)

		// Exercise 4.4
		def sequence[A](a: List[Option[A]]): Option[List[A]] =
			a match
				case Nil => Some(Nil)
				// case h :: t => h.flatMap(x => sequence(t).map(y =>  x :: y))
				case h :: t => mapTwo(h, sequence(t))((x, y) => x :: y)

		def sequence2[A](a: List[Option[A]]): Option[List[A]] =
			@annotation.tailrec
			def go(l: List[Option[A]], acc: List[A] = List.empty[A]): Option[List[A]] =
				l match
					case h :: t => h match
						case Some(v) => go(t, acc :+ v)
						case None    => None
					case Nil    => Some(acc)

			go(a)

		// Exercise 4.5
		def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
			a match
				case Nil => Some(Nil)
				case h :: t => mapTwo(f(h), traverse(t)(f))((x, y) => x :: y)

		def sequence3[A](a: List[Option[A]]): Option[List[A]] =
			traverse(a)(identity)

		def Try[A](a: => A): Option[A] =
			try Some(a)
			catch {
				case e: Exception => None
			}

	import Option._

	case class Employee(name: String, department: String, manager: Option[String])

	def lookupByName(name: String): Option[Employee] =
		name match
			case "Joe" => Some(Employee("Joe", "HR", Some("Fleur")))
			case "Thijs" => Some(Employee("Thijs", "IT", Some("Marco")))
			case _ => None

	def insuranceRateQuote(age: Int, noSpeedingTickets: Int): Double =
		(age * 0.4 + noSpeedingTickets * 0.6) * 100

	def parseInsuranceRateQuote(age: String, noSpeedingTickets: String): Option[Double] =
		val optAge: Option[Int] = Try(age.toInt)
		val optNoSpeedingTickets: Option[Int] = Try(noSpeedingTickets.toInt)

		mapTwo(optAge, optNoSpeedingTickets)(insuranceRateQuote)

	enum Either[+E, +A]:
		case Left(value: E)
		case Right(value: A)

		def isRight: Boolean =
			this match
				case Right(_) => true
				case Left(_)  => false

		def isLeft: Boolean =
			!isRight

		// Exercise 4.6
		def map[B](f: A => B): Either[E, B] =
			this match
				case Right(x) => Right(f(x))
				case Left(e) => Left(e)

		def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
			this match
				case Right(x) => f(x)
				case Left(e) => Left(e)

		def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] =
			this match
				case Right(x) => Right(x)
				case Left(_) => b

		def mapTwo[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
			// this.flatMap(x => b.map(y => f(x, y)))
			for
				x <- this
				y <- b
			yield
				f(x, y)

	object Either:
		def mean(xs: IndexedSeq[Double]): Either[String, Double] =
			if xs.isEmpty then
				Left("mean of empty list!")
			else
				Right(xs.sum / xs.length)

		def Try[A](a: => A): Either[Exception, A] =
			try Right(a)
			catch case e: Exception => Left(e)

		def safeDiv(x: Int, y: Int): Either[Exception, Int] =
			Try(x / y)

		// Exercise 4.7
		def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
			es match
				case Nil => Right(Nil)
				case h :: t => h.mapTwo(sequence(t))((x, y) => x :: y)

		def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
			traverse(es)(identity)

		def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
			as match
				case Nil => Right(Nil)
				case h :: t => f(h).mapTwo(traverse(t)(f))((x, y) => x :: y)

	// Exercise 4.8
	// Create a mapping function that matches on a tuple of both inputs.

	import Either._

	def parseInsuranceRateQuote2(age: String, noSpeedingTickets: String): Either[Exception, Double] =
		for
			a <- Either.Try(age.toInt)
			t <- Either.Try(noSpeedingTickets.toInt)
		yield
			insuranceRateQuote(a, t)
