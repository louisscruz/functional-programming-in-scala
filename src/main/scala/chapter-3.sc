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