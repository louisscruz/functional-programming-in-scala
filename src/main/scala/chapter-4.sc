import scala.{Option => _, Either => _, Some => _, None => _}

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(x) => MySome(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(x) => x
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    map(f) getOrElse MyNone

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case MyNone => ob
    case MySome(_) => this
  }

  def filter(f: A => Boolean): MyOption[A] = this match {
    case MySome(x) if f(x) => this
    case _ => MyNone
  }
}
case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

val some = MySome(get = "something")
val none = MyNone

val addExclamation = (el: String) => el + "!"

some.map(addExclamation) == MySome("something!")
none.map(addExclamation) == MyNone

some.getOrElse("default") == "something"
none.getOrElse("default") == "default"

some.flatMap(el => MySome(addExclamation(el))) == MySome("something!")
none.flatMap(el => MySome(addExclamation(el))) == MyNone

some.orElse(MySome("default")) == MySome("something")
none.orElse(MySome("default")) == MySome("default")

some.filter(el => el == "something") == MySome("something")
some.filter(el => el == "other") == MyNone
none.filter(el => el == "something") == MyNone

def mean(xs: Seq[Double]): MyOption[Double] =
  if (xs.isEmpty) MyNone else MySome(xs.sum / xs.length)

def variance[A](xs: Seq[Double]) =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

variance(List(1, 2, 3, 4)) == MySome(1.25)
variance(List()) == MyNone

def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = (a, b) match {
  case (MyNone, _) | (_, MyNone) => MyNone
  case (MySome(x), MySome(y)) => MySome(f(x, y))
}

map2(MySome("a"), MySome("b"))(_ + _) == MySome("ab")
map2(MyNone, MySome("a"))((a: String, b: String) => a + b) == MyNone

def map3[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
  a.flatMap(aa => b.map(bb => f(aa, bb)))

map3(MySome("a"), MySome("b"))(_ + _) == MySome("ab")
map3(MyNone, MySome("a"))((a: String, b: String) => a + b) == MyNone

def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = a match {
  case Nil => MySome(Nil)
  case x :: xs => x.flatMap(xx => sequence(xs).map(xx :: _))
}

sequence(List(MySome(1), MySome(2))) == MySome(List(1, 2))
sequence(List(MySome(1), MyNone)) == MyNone

def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = a match {
  case Nil => MySome(Nil)
  case x :: xs => map3(f(x), traverse(xs)(f))(_ :: _)
}

def allNumsAreEven(list: List[Int]) =
  traverse(list)(num => if (num % 2 == 0) MySome(num) else MyNone)

allNumsAreEven(List(1, 2, 3)) == MyNone

def sequenceViaTraverse[A](a: List[MyOption[A]]): MyOption[List[A]] = traverse(a)(_.orElse(MyNone))

sequenceViaTraverse(List(MySome(1), MySome(2))) == MySome(List(1, 2))
sequenceViaTraverse(List(MySome(1), MyNone)) == MyNone

//
// EITHER
//

sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyRight(x) => MyRight(f(x))
    case MyLeft(x) => MyLeft(x)
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(x) => MyLeft(x)
    case MyRight(x) => f(x)
  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(_) => b
    case MyRight(_) => this
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    flatMap(aa => b.map(bb => f(aa, bb)))
}
case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A]

val left = MyLeft("left")
val right = MyRight("right")

left.map((el: String) => el + "!") == MyLeft("left")
right.map((el: String) => el + "!") == MyRight("right!")

right.flatMap(first => MyRight("other").map(other => other + first)) == MyRight("otherright")
right.flatMap(first => MyLeft("other").map((other: String) => other + first)) == MyLeft("other")

left.orElse(MyLeft("hit or else")) == MyLeft("hit or else")
left.orElse(MyRight("hit or else")) == MyRight("hit or else")

right.orElse(MyLeft("hit or else")) == MyRight("right")
right.orElse(MyRight("hit or else")) == MyRight("right")

left.map2(right)((a: String, b: String) => a + b) == MyLeft("left")
right.map2(MyRight("other"))((a: String, b: String) => a + b) == MyRight("rightother")

def eTraverse[E, A, B](a: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = a match {
  case Nil => MyRight(Nil)
  case x :: xs => f(x).map2(eTraverse(xs)(f))(_ :: _)
}

eTraverse(List(1, 2, 3))((el: Int) => if (el % 2 == 0) MyRight(el.toString) else MyLeft("ah!")) == MyLeft("ah!")

//def sequence[E, A](a: List[MyEither[E, A]]): MyEither[E, List[A]] = a match {
//  case Nil => MyRight(Nil)
//  case x :: xs => x.flatMap(xx => sequence(xs).map(yy => xx :: yy))
//}
def sequence[E, A](a: List[MyEither[E, A]]): MyEither[E, List[A]] = eTraverse(a)(x => x)

sequence(List(MyRight("a"), MyRight("b"))) == MyRight(List("a", "b"))
sequence(List(MyLeft("a"), MyRight("b"))) == MyLeft("a")