sealed trait MyStream[+A] {
  def headOption: Option[A] = this match {
    case MyEmpty => None
    case MyCons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case MyEmpty => List()
    case MyCons(h, t) => h() :: t().toList
  }

  def stackSafeToList: List[A] = {
    val buffer = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def listAcc(stream: MyStream[A]): List[A] = stream match {
      case MyCons(h, t) => {
        buffer += h()
        listAcc(t())
      }
      case _ => buffer.toList
    }
    listAcc(this)
  }

  def exists(f: A => Boolean): Boolean = this match {
    case MyCons(h, t) => f(h()) || t().exists(f)
    case _ => false
  }

  def take(n: Int): MyStream[A] = this match {
    case MyCons(h, t) if n > 1 => MyStream.cons(h(), t().take(n - 1))
    case MyCons(h, _) if n == 1 => MyStream.cons(h(), MyStream.empty)
    case _ => MyEmpty
  }

  def drop(n: Int): MyStream[A] = this match {
    case MyCons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): MyStream[A] = this match {
    case MyCons(h, t) if f(h()) => MyStream.cons(h(), t().takeWhile(f))
    case _ => MyStream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case MyCons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(predicate: A => Boolean): Boolean =
    foldRight(true)((a, b) => predicate(a) && b)

  def takeWhileViaFoldRight(f: A => Boolean): MyStream[A] =
    foldRight(MyStream.empty[A])((h, t) =>
      if (f(h)) MyStream.cons(h, t) else MyStream.empty
    )

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def mapViaFoldRight[B](f: A => B): MyStream[B] =
    foldRight(MyStream.empty[B])((a, b) => MyStream.cons(f(a), b))

  def map[B](f: A => B) = mapViaFoldRight(f)

  def filterViaFoldRight(f: A => Boolean) =
    foldRight(MyStream.empty[A])((a, b) => if (f(a)) MyStream.cons(a, b) else b)

  def append[B >: A](s: => MyStream[B]) =
    foldRight(s)((h, t) => MyStream.cons(h, t))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(MyStream.empty[B])((h, t) => f(h).append(t))

  def mapViaUnfold[B](f: A => B): MyStream[B] =
    MyStream.unfold(this)({
      case MyCons(h, t) => Some(f(h()), t())
      case _ => None
    })

  def takeViaUnfold(n: Int): MyStream[A] =
    MyStream.unfold((this, n))({
      case (MyCons(h, t), 1) => Some(h(), (t(), 0))
      case (MyCons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _ => None
    })

  def takeWhileViaUnfold(f: A => Boolean): MyStream[A] =
    MyStream.unfold(this)({
      case MyCons(h, t) => if (f(h())) Some(h(), t()) else None
      case _ => None
    })

  def zipWithViaUnfold[B, C](other: MyStream[B])(f: (A, B) => C): MyStream[C] =
    MyStream.unfold((this, other))({
      case (MyCons(x, xs), MyCons(y, ys)) => Some(f(x(), y()), (xs(), ys()))
      case _ => None
    })

  def zipAll[B](other: MyStream[B]): MyStream[(Option[A], Option[B])] =
    MyStream.unfold((this, other))({
      case (MyCons(x, xs), MyCons(y, ys)) => Some((Some(x()), Some(y())), (xs(), ys()))
      case (MyCons(x, xs), MyEmpty) => Some((Some(x()), None), (xs(), MyEmpty))
      case (MyEmpty, MyCons(y, ys)) => Some((None, Some(y())), (MyEmpty, ys()))
      case (MyEmpty, MyEmpty) => None
    })

  def startsWith[B](other: MyStream[B]): Boolean =
    this.zipAll(other).takeWhile(!_._2.isEmpty).forAll({
      case (x, y) => x == y
    })

  def tails: MyStream[MyStream[A]] =
    MyStream.unfold(this)({
      case MyEmpty => None
      case stream => Some((stream, stream.drop(1)))
    }).append(MyStream(MyStream.empty))

  def scanRight[B](seed: B)(f: (A, => B) => B): MyStream[B] =
    foldRight((seed, MyStream(seed)))((a, acc) => {
      lazy val cachedAcc = acc
      val nextA = f(a, cachedAcc._1)
      (nextA, MyStream.cons(nextA, cachedAcc._2))
    })._2
}

case object MyEmpty extends MyStream[Nothing]
case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    MyCons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = MyEmpty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A) = {
    lazy val tail: MyStream[A] = MyCons(() => a, () => tail)
    tail
  }

  def from(n: Int): MyStream[Int] = MyStream.cons(n, from(n + 1))

  def fibs = {
    def fibAcc(a: Int, b: Int): MyStream[Int] = MyStream.cons(a, fibAcc(b, a + b))

    fibAcc(0, 1)
  }

  def unfold[A, B](z: B)(f: B => Option[(A, B)]): MyStream[A] = f(z) match {
    case Some((current, next)) => MyStream.cons(current, unfold(next)(f))
    case None => MyEmpty
  }

  def fibsViaUnfold =
    unfold((0, 1))({ case (a, b) => Some((a, (b, a + b)))})

  def fromViaUnfold(n: Int): MyStream[Int] =
    unfold(n)(el => Some(el, (el + 1)))
}

val myStream = MyStream(0, 1.1, 2.2, 3.3)

myStream.headOption == Some(0)

myStream.toList == List(0, 1.1, 2.2, 3.3)

myStream.stackSafeToList == List(0, 1.1, 2.2, 3.3)

myStream.take(2).stackSafeToList == List(0, 1.1)

myStream.drop(2).stackSafeToList == List(2.2, 3.3)

myStream.takeWhile(_ < 3).stackSafeToList == List(0, 1.1, 2.2)

myStream.foldRight(0D)(_ + _) == 6.6

myStream.forAll(_ < 4) == true

// foldRight(true)((a, b) => predicate(a) && b)
// foldRight(true)(f)
// (0, MyStream(1.1, 2.2, 3.3) => f(0, MyStream(1.1, 2.2, 3.3)
// (0, MyStream(1.1, 2.2, 3.3) => true && f(1.1, MyStream(2.2, 3.3))
// (0, MyStream(1.1, 2.2, 3.3) => true && (true && f(2.2, MyStream(3.3)))
// (0, MyStream(1.1, 2.2, 3.3) => true && (true && (true && f(3.3, MyStream())))
// (0, MyStream(1.1, 2.2, 3.3) => true && (true && (true && (true && f(MyEmpty))))
// true

myStream.takeWhile(_ < 3).stackSafeToList == List(0, 1.1, 2.2)

myStream.headOptionViaFoldRight == Option(0)

myStream.mapViaFoldRight(_ + 1).toList == List(1.0, 2.1, 3.2, 4.3)

myStream.filterViaFoldRight(_ > 0).toList == List(1.1, 2.2, 3.3)

myStream.append(MyStream(4.4)).toList == List(0, 1.1, 2.2, 3.3, 4.4)

myStream.flatMap(el => if (el < 2) MyStream.empty else MyStream(el)).toList == List(2.2, 3.3)

val ones: MyStream[Int] = MyStream.cons(1, ones)

ones.take(5).toList == List(1, 1, 1, 1, 1)

ones.exists(_ == 1) == true

ones.map(_ + 1).exists(_ % 2 == 0) == true

// This returns a stream. Calling toList produces a stack overflow.
// ones.takeWhile(_ == 1)

ones.forAll(_ != 1) == false

val fromTwo = MyStream.from(2)

fromTwo.takeWhile(_ < 5).toList == List(2, 3, 4)

MyStream.fibs.take(5).toList == List(0, 1, 1, 2, 3)

val evenNumbers = MyStream.unfold(0)({
  case x if x % 2 == 0 => Some((x, x + 2))
  case _ => None
}).take(5).toList

evenNumbers == List(0, 2, 4, 6, 8)

MyStream.fibsViaUnfold.take(5).toList == List(0, 1, 1, 2, 3)

MyStream.from(10).take(3).toList == List(10, 11, 12)

val onesViaUnfold = MyStream.unfold(1)(el => Some((el, 1)))

onesViaUnfold.take(3).toList == List(1, 1, 1)

myStream.mapViaUnfold(_ + 1).toList == List(1, 2.1, 3.2, 4.3)

myStream.takeViaUnfold(3).toList == List(0, 1.1, 2.2)

myStream.takeWhileViaUnfold(_ < 2).toList == List(0, 1.1)

val upStream = MyStream(1, 2, 3)
val downStream = MyStream(3, 2, 1)

upStream.zipWithViaUnfold(downStream)(_ + _).toList == List(4, 4, 4)

upStream.zipAll(downStream.take(1)).toList == List((Some(1), Some(3)), (Some(2), None), (Some(3), None))

val allInts = MyStream.from(1)
val favoriteNumbers = MyStream(1, 2, 3)

allInts.startsWith(favoriteNumbers) == true

favoriteNumbers.tails.map(_.toList).toList == List(List(1, 2, 3), List(2, 3), List(3), List())

favoriteNumbers.scanRight(0)(_ + _).toList == List(6, 5, 3, 0)