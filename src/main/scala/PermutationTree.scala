object PermutationTree extends App:
	enum Tree[+A]:
		case Leaf(a: A)
		case Node(l: Tree[A], a: A, r: Tree[A])
//
//	object Tree:
//		def listToTree[A](la: List[List[A]]): Tree[A] =
//			@annotation.tailrec
//			def loop(acc: Tree[A]): Tree[A] =
//				la match
//					case h :: t   =>
//					case h :: Nil => Leaf(h)
//					case Nil      => acc
//
//		def treeToPerms[A](t: Tree[A]): List[Tree[A]] =
//			???
//
//		def pruneSmoothPerms[A](t: Tree[A]): Tree[A] =
//			???
//
//		def smootPermutations[A](la: List[A], d: A): Tree[A] =
//			???
//
//
////		def split[A](list: List[A]): List[(A, List[A])] =
////			list match
////				case Nil => Nil
////				case x :: xs => (x, xs) :: split(xs).map:
////					case (y, ys) => (y, x :: ys)
////
////		def perms[A](list: List[A]): List[List[A]] =
////			list match
////				case Nil => List(Nil)
////				case _ =>
////					for
////						(v, vs) <- split(list)
////						p       <- perms(vs)
////					yield
////						v :: p
////
////		import math.Numeric.Implicits.infixNumericOps
////
////		def smooth[A](n: A, list: List[A])(using Numeric[A]): Boolean =
////			list match
////				case x :: y :: ys if math.abs(y - x) < n && smooth(n, y :: ys) => true
////				case _ :: _ => false
////				case _      => true
////
////		def smoothPerms(n: Int, list: List[Int]): List[List[Int]] =
////			perms(list).filter(smooth(n, _))
////
////	pprint.log(Tree.split(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)))
////
////	pprint.log("Test")
////
////
////def getPermutations(input: String): List[String] = {
////     |   if (input.isEmpty) List("")
////     |   else {
////     |     for {
////     |       i <- input.indices.toList
////     |       char = input(i)
////     |       rest = input.substring(0, i) + input.substring(i + 1)
////     |       perm <- getPermutations(rest)
////     |     } yield char + perm
////     |   }
////     | }
////     |
////     | val inputString = "abc"
////     | val permutations = getPermutations(inputString)
////     | permutations.foreach(println)
