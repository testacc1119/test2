package dyhas.bo.lab1

import scala.annotation.tailrec

enum List[+A] {
  case Nil
  case Cons(hd: A, tl: List[A])

  def reverse: List[A] = {
    @tailrec
    def f(list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case Cons(head, tail) => 
          f(tail, Cons(head, acc))
      }
    }
    f(this, Nil)
  }

  def add_left[B >: A](z: B) : List[B] = {
    val temp: List[B] = List(z)
    def f(xs: List[B], acc: List[B]) : List[B] = {
      xs match {
        case Nil => acc
        case Cons(head, tail) => f(tail, acc)
      }
    }
    f(this, temp)
  }

  def add_right[B >: A](z: B) : List[B] = {
    var rev = this.reverse
    return (rev.add_left(z)).reverse
  }

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
  def grouped[B >: A](xs: List[B], window: Int): List[List[B]] = {
    @tailrec
    def f(xs: List[B], window: Int, acc: List[List[B]], temp: List[B], count: Int): List[List[B]] = {
      xs match {
        case Nil if count == 0 => {
          return acc
        }
        case Nil if count > 0 => {
          return acc.add_right(temp)
        }
        case Cons(head, tail) if count < window => {
          var buf = temp.add_right(head)
          var i = count + 1
          f(tail, window, acc, temp, i)
        }
        case Cons(head, tail) if count == window => {
          acc.add_right(temp)
          f(tail, window, acc, Nil, 0)
        }
      }
    }
    f(xs, window, Nil, Nil, 0)
  }

}

import List.*
object List:
  def apply[A](xs: A*): List[A] = of(xs*)
  def of[A](xs: A*): List[A] =
    xs.foldRight(Nil: List[A]) { case (x, acc) => Cons(x, acc) }

@main def hello: Unit = 
  println("I work!")