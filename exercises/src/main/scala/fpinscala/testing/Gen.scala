package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

/* 8.1
sum of first half of list and second half of list should equal sum of list
sum of empty list should be 0
 */

/* 8.2
max of list of size 1 is that element
max of reverse list should be same as max of original
max of the max of first half and max of second half should be same as max of original
 */


trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

/*
8.3
 */
  def &&(p: Prop): Prop = new Prop{
    def check = Prop.this.check && p.check
  }

}

object Prop {
  type SuccessCount: Int
  type FailedCase = String
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

case class Gen[A](sample: State[RNG, A]) {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  }

  def unit[A](a: => A): Gen[A] = Gen(State(unit(a)))

  def boolean: Gen[Boolean] = Gen(State.boolean)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample))
}