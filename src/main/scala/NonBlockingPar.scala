object NonBlockingPar:
	import java.util.concurrent.atomic.AtomicReference
	import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
	import Actor.*

	import Part3Summary.Monad
	import Part3Summary.Monad.*

	opaque type Future[+A] =
		(A => Unit) => Unit

	opaque type Par[+A] =
		ExecutorService => Future[A]

	object Par:
		def lazyUnit[A](a: => A): Par[A] =
			fork(parMonad.unit(a))

		def fork[A](a: => Par[A]): Par[A] =
			(s: ExecutorService) =>
				(cb: A => Unit) => eval(s)(a(s)(cb))

		def eval (s: ExecutorService)(r: => Unit): Unit =
			s.submit:
				new Callable[Unit]:
					def call: Unit = r

		def async[A](f: (A => Unit) => Unit): Par[A] =
			(s: ExecutorService) =>
				(cb: A => Unit) => f(cb)

		def asyncF[A, B](f: A => B): A => Par[B] =
			a => lazyUnit(f(a))

		def sequence[A](pas: List[Par[A]]): Par[List[A]] =
			pas match
				case Nil      => parMonad.unit(Nil)
				case h :: Nil => h.map(a => List(a))
				case _        =>
					val (l, r) = pas.splitAt(pas.length / 2)

					sequence(l).mapTwo(sequence(r))(_ ++ _)

		def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
			fork(sequence(ps.map(asyncF(f))))

		def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] =
			fork:
				val pars: List[Par[List[A]]] =
					l.map(asyncF(a => if f(a) then List(a) else List()))

				sequence(pars).map(_.flatten)

		def join[A](ppa: Par[Par[A]]): Par[A] =
			(s: ExecutorService) =>
				(cb: A => Unit) => ppa(s)(pa => eval(s)(pa(s)(cb)))

		extension [A](self: Par[A])
			def run(s: ExecutorService): A =
				val ref = new AtomicReference[A]
				val latch = new CountDownLatch(1)

				self(s): (a: A) =>
					ref.set(a)
					latch.countDown()

				latch.await()
				ref.get

			def mapTwo[B, C](that: Par[B])(f: (A, B) => C): Par[C] =
				(s: ExecutorService) =>
					(cb: C => Unit) =>
						var ar: Option[A] = None
						var br: Option[B] = None

						val combiner = Actor[Either[A, B]](s):
							case Left(a) =>
								if br.isDefined then Par.eval(s)(cb(f(a, br.get)))
								else ar = Some(a)
							case Right(b) =>
								if ar.isDefined then Par.eval(s)(cb(f(ar.get, b)))
								else br = Some(b)

						self(s)(a => combiner ! Left(a))
						that(s)(b => combiner ! Right(b))

		given parMonad: Monad[Par] with
			def unit[A](a: => A): Par[A] =
				(_: ExecutorService) =>
					(cb: A => Unit) => cb(a)

			def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
				fork:
					(s: ExecutorService) =>
						(cb: B => Unit) => fa(s)(a => f(a)(s)(cb))

			override def map[A, B](fa: Par[A])(f: A => B): Par[B] =
				(s: ExecutorService) =>
					(cb: B => Unit) => fa(s)(a => eval(s)(cb(f(a))))
