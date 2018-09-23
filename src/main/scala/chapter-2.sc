def fibs(n: Int) = {
  def fibsAcc(current: Int, previous: Int, remaining: Int): Int =
    if (remaining == 0) current else fibsAcc(current + previous, current, remaining - 1)

  fibsAcc(0, 1, n)
}

fibs(0) == 0
fibs(1) == 1
fibs(2) == 1
fibs(3) == 2

def isSorted[A](as: Array[A], comparator: (A, A) => Boolean): Boolean = {
  def isSortedIterator(n: Int, maxLength: Int): Boolean = {
    if (maxLength > n) {
      if (comparator(as(n - 1), as(n))) isSortedIterator(n + 1, maxLength) else false
    } else {
      true
    }
  }

  val length = as.length

  if (length == 1) true else isSortedIterator(1, length)
}

isSorted(Array(1, 2, 3), (a: Int, b: Int) => a <= b) == true
isSorted(Array(1, 1, 2, 2, 3, 3), (a: Int, b: Int) => a <= b) == true
isSorted(Array(1, 1, 0), (a: Int, b: Int) => a <= b) == false

def curry[A, B, C](f: (A, B) => C): A => B => C = {
  firstArg: A => secondArg: B => f(firstArg, secondArg)
}

def myFunction(text: String, num: Int) = s"$text $num"

val curriedMyFunction = curry(myFunction)

curriedMyFunction("My favorite number is")(42) == "My favorite number is 42"

def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (firstArg: A, secondArg: B) => f(firstArg)(secondArg)
}

val uncurriedMyFunction = uncurry(curriedMyFunction)

uncurriedMyFunction("My favorite number is", 42) == "My favorite number is 42"

def compose[A, B, C](f: B => C, g: A => B): A => C = {
  arg: A => f(g(arg))
}

def firstFunction(anyText: String) = 42

def secondFunction(anyInt: Int) = anyInt.toFloat

val composedFunction = compose(secondFunction, firstFunction)

composedFunction("Any text") == 42.0