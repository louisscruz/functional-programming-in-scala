import java.util.concurrent._

type Par[A] = ExecutorService => Future[A]

def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

def unit[A](a: A): Par[A] = (s: ExecutorService) => UnitFuture(a)

case class UnitFuture[A](get: A) extends Future[A] {
  def isDone = true
  def get(timeout: Long, units: TimeUnit) = get
  def isCancelled = false
  def cancel(evenIfRunning: Boolean): Boolean = false
}

def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (s: ExecutorService) => {
  val af = a(s)
  val bf = b(s)
  UnitFuture(f(af.get, bf.get))
}

map2(unit(1), unit(2))(_ + _)(Executors.newFixedThreadPool(32)).get() == 3

def fork[A](a: => Par[A]): Par[A] = s => s.submit(new Callable[A] {
  def call = a(s).get
})

def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

map(unit(1))(_ + 1)(Executors.newFixedThreadPool(32)).get() == 2

def sortPar(parList: Par[List[Int]]) = map(parList)(list => list.sorted)

sortPar(unit(List(3, 2, 1)))(Executors.newFixedThreadPool(32)).get() == List(1, 2, 3)

def equal[A](s: ExecutorService)(a: Par[A], b: Par[A]) = a(s).get() == b(s).get()

equal(Executors.newFixedThreadPool(32))(unit(5), unit(5)) == true

def delay[A](a: => Par[A]): Par[A] = s => a(s)

run(Executors.newFixedThreadPool(32))(delay(unit(1))).get() == 1

delay(unit(1))(Executors.newFixedThreadPool(32)).get() == 1

def choice[A](cond: Par[Boolean])(a: Par[A], b: Par[A]): Par[A] = s =>
  if (run(s)(cond).get) a(s)
  else b(s)

choice(unit(false))(unit(1), unit(2))(Executors.newFixedThreadPool(32)).get() == 2