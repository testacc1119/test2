package dyhas.bo.lab1

import scala.annotation.tailrec

enum List[+A] {
  case Nil
  case Cons(hd: A, tl: List[A])

  def foldLeft[A, B](xs: List[A], z: B)(op: (B, A) => B): B = {
    @tailrec
    def f(xs: List[A], acc: B): B = {
    xs match {
      case Nil => acc
      case Cons(xh, xt) => f(xt, op(acc, xh))
    }
  }
    f(xs, z)
}
}

@main def hello: Unit = 
  println("I work")