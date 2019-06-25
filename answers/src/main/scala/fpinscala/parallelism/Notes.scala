package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

//FPinScala
//Chapter 7

object Par { // container for an unevaluated A, allows for retrieval of the value of computed A
  type Par[A] = ExecutorService => Future[A]
  //Problem 1
  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a) //Creates a computation, Par, (that immediately results in the value a)

  private case class UnitFuture[A](get: A) extends (Future[A]) {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  // 7.1
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // Combines the results of two parallel computations with a binary function
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =  //the given Par should be run in a separate logical thread
    es => es.submit(new Callable[A] {
      override def call(): A = {
        def call = a(es).get
      }})
  def lazyUnit[A](a: => A): par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s) //Fully evaluates a given Par, spawning parallel computations as requested by fork and extracting the resulting value

  def sum(ints: IndexSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else
      Par.get(Par.unit(sumL) + Par.get(Par.unit(sumL) // won’t run concurrently
  //^ Functions in Scala are strictly evaluated from L to R. Par.get() will get evaluated before Par.unit(sumL) and the calling order will effectively look like:
  //Par.get() -> Par.unit(sumL)…then we move onto the next Par.get() -> Par.unit(sumR)
  //Unit only has a side effect with regards to get. We want to avoid calling get until the very end.
  //Our goal is to combine asynchronous operations w/o waiting for them to finish

  // Solution 1
  // Let's now have sums return a Par[Int] object so that we no longer have to make a call to Par.get()
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else
      val (l,r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
  // this also doesn't help, because map2 is strict. We will strictly construct the left tree before we even look at the right side
  // the let still gets executed first, not much parallelism. walk thru example of IndexedSeq(1, 2, 3, 4)

 // Problem 2
  // Explicit forking, so that
//  def fork[A](a: => Par[A]): Par[A] //the given Par should be run in a separate logical thread

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else
  val (l,r) = ints.splitAt(ints.length / 2)
  Par.map2(sum(l), sum(r))(_ + _)
}




//strict vs lazy evaluation
//strict will completely evaluated when the code is called ex) listA.filter(bool) //will filter based on the bool
//does not evaluate the expression until the object is “demanded” or needed ex) listB.withFilter(bool) //won’t filter until listB is accessed
//- map2 is initially written as strict
