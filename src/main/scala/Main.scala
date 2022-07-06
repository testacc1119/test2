package dyhas.bo.lab1

import scala.annotation.tailrec


def foldLeft[A, B](xs: Seq[A], z: B)(op: (B, A) => B): B = 
  @tailrec
  def f(xs: Seq[A], acc: B): B = xs match {
    case Seq()   => acc
    case x +: xs => f(xs, op(acc, x))
  }
  f(xs, z)


@main def hello: Unit = 
  val l = List(1, 2, 3, 4)
  println(foldLeft(l,0)(_ + _))