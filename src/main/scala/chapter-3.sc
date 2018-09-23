// Lists are defined something like this:
//
// sealed trait List[+A]
// case object Nil extends List[Nothing]
// case class Cons[+A](head: A, tail: List[A]) extends List[A]
//

val x =  List(1, 2, 3, 4, 5) match {
  case x :: List(2, 3, 4, _) => x
  case Nil => 42
  case x :: List(y, 3, 4, _) => x + y
  case h :: hs => h + hs.sum
  case _ => 101
}

x == 1

// Variadic functions take zero or more arguments of type A.

def tail[A](list: List[A]): List[A] = list.tail

tail(List(1, 2, 3)) == List(2, 3)

def setHead[A](head: A, list: List[A]) = head :: list.tail

setHead(100, List(1, 2, 3)) == List(100, 2, 3)

@annotation.tailrec
def drop[A](l: List[A], n: Int): List[A] = if (n == 0) l else drop(l.tail, n - 1)

drop(List(1, 2, 3, 4), 2) == List(3, 4)

@annotation.tailrec
def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = if (f(l.head)) dropWhile(l.tail)(f) else l

dropWhile(List(2, 4, 6, 1, 3, 5, 7))(n => n % 2 == 0) == List(1, 3, 5, 7)

def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case x :: Nil => Nil
  case x :: xs => x :: init(xs)
}

init(List(1, 2, 3)) == List(1, 2)

List(1, 2, 3, 4).foldRight(0)((_, el) => el + 1) == 4

def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
  case Nil => z
  case x :: xs => f(x, foldRight(xs, z)(f))
}

foldRight(List(1, 2, 3, 4), 0)(_ + _) == 10

// foldRight(List(1, 2, 3), 0)(_ + _)
// 1 + foldRight(List(2, 3), 0)(_ + _)
// 1 + (2 + foldRight(List(3), 0)(_ + _))
// 1 + (2 + (3 + foldRight(Nil, 0)(_ + _)))
// 1 + (2 + (3 + (0)))
// 6

@annotation.tailrec
def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  case Nil => z
  case x :: xs => foldLeft(xs, f(z, x))(f)
}

foldLeft(List(1, 2, 3, 4), 0)(_ + _) == 10

// foldLeft(List(1, 2, 3), 0)(_ + _)
// foldLeft(List(2, 3), 1)(_ + _)
// foldLeft(List(3), 3)(_ + _)
// foldLeft(Nil, 6)(_ + _)
// 6

def sumWithFoldLeft(l: List[Int]) = foldLeft(l, 0)(_ + _)

sumWithFoldLeft(List(1, 2, 3)) == 6

def productWithFoldLeft(l: List[Int]) = foldLeft(l, 1)(_ * _)

productWithFoldLeft(List(1, 2, 3)) == 6

def reverseListWithFoldLeft[A](l: List[A]) = foldLeft(l, List[A]())((x, xs) => xs :: x)

reverseListWithFoldLeft(List(1, 2, 3)) == List(3, 2, 1)

def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
  foldLeft(reverseListWithFoldLeft(l), z)((b, a) => f(a, b))

foldRightViaFoldLeft(List(1, 2, 3), 0)(_ + _) == 6

def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
  foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

foldLeftViaFoldRight(List(1, 2, 3), 0)(_ + _) == 6

def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = foldLeft(l.reverse, r)((acc, el) => el :: acc)

appendViaFoldLeft(List(1, 2, 3), List(4, 5, 6))

// foldLeft(List(3, 2, 1), List(4, 5, 6)((acc, el) => el :: acc)
// foldLeft(List(2, 1), List(3, 4, 5, 6)((acc, el) => el :: acc)
// foldLeft(List(1), List(2, 3, 4, 5, 6)((acc, el) => el :: acc)
// foldLeft(List(), List(1, 2, 3, 4, 5, 6)((acc, el) => el :: acc)
// List(1, 2, 3, 4, 5, 6)

// This version of flatten isn't good because each append method will occur in O(n) time, n number of times at worst.
// That puts this at O(n^2) time complexity. This is an inherent problem with foldLeft for this problem.
def poorFlattenListOfLists[A](l: List[List[A]]): List[A] = foldLeft(l, List[A]())((acc, el) => acc ++ el)

poorFlattenListOfLists(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4)

// foldLeft(List(List(1, 2), List(3, 4)), List())((acc, el) => acc ++ el)
// foldLeft(List(List(3, 4)), List(1, 2))((acc, el) => acc ++ el)
// foldLeft(List(), List(1, 2, 3, 4))((acc, el) => acc ++ el)
// List(1, 2, 3, 4)

def flattenListOfLists[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())((a, b) => a ++ b)

flattenListOfLists(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4)

// foldRight(List(List(1, 2), List(3, 4)), List())((a, b) => a ++ b)
// List(1, 2) ++ (foldRight(List(List(3, 4)), List())((a, b) => a ++ b))
// List(1, 2) ++ (List(3, 4) ++ (foldRight(List(), List())((a, b) => a ++ b)))
// List(1, 2) ++ (List(3, 4) ++ (List()))
// List(1, 2, 3, 4)

// The previous solution is in O(n) time, but our original foldRight implementation is not stack safe.
// Here, we use the reversal trick with foldLeft to make a stack-safe, O(n) implementation.
def stackSafeFlattenListOfLists[A](l: List[List[A]]): List[A] = foldLeft(l.reverse, List[A]())((acc, el) => el ++ acc)

stackSafeFlattenListOfLists(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4)

// foldLeft(List(List(3, 4), List(1, 2)), List())((acc, el) => el ++ acc)
// foldLeft(List(List(1, 2)), List(3, 4))((acc, el) => el ++ acc)
// foldLeft(List(), List(1, 2, 3, 4))((acc, el) => el ++ acc)
// List(1, 2, 3, 4)

def add1(l: List[Int]): List[Int] = foldLeft(l.reverse, List[Int]())((acc, el) => (el + 1) :: acc)

add1(List(1, 2, 3)) == List(2, 3, 4)

def doubleToString(l: List[Double]): List[String] = foldLeft(l.reverse, List[String]())((acc, el) => el.toString :: acc)

doubleToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0")

def map[A, B](as: List[A])(f: A => B): List[B] = foldLeft(as.reverse, List[B]())((acc, el) => f(el) :: acc)

map(List(1, 2, 3))(_ * 2) == List(2, 4, 6)

def filter[A](as: List[A])(f: A => Boolean): List[A] =
  foldLeft(as.reverse, List[A]())((acc, el) => if (f(el)) el :: acc else acc)

filter(List(1, 2, 3, 4))(el => el % 2 == 0) == List(2, 4)

def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
  foldLeft(as.reverse, List[B]())((acc, el) => f(el) ++ acc)

flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3)

def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] =
  stackSafeFlattenListOfLists(map(as)(f))

flatMap2(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3)

def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(el => if (f(el)) List(el) else Nil)

filterViaFlatMap(List(1, 2, 3, 4))(el => el % 2 == 0) == List(2, 4)

def zipAdd(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (x :: xs, y :: ys) => (x + y) :: zipAdd(xs, ys)
}

zipAdd(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9)

def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
}

zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9)

// This solution below is O(nm). A trie would be more appropriate for performance (O(n(log(m)))).
@annotation.tailrec
def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
  case (_, Nil) => true
  case (x :: xs, y :: ys) if x == y => startsWith(xs, ys)
  case _ => false
}

@annotation.tailrec
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
  case Nil => sub == Nil
  case _ if startsWith(sup, sub) => true
  case x :: xs => hasSubsequence(xs, sub)
}

val supList = List(1, 2, 3, 4)

hasSubsequence(supList, List()) == true
hasSubsequence(supList, List(2, 3)) == true
hasSubsequence(supList, List(4)) == true