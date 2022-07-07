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
    return Cons(z, this)
  }

  def add_right[B >: A](z: B) : List[B] = {
    return (Cons(z, this.reverse)).reverse
  }

  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    @tailrec
    def f(xs: List[A], acc: B): B = {
    xs match {
      case Nil => acc
      case Cons(xh, xt) => f(xt, op(acc, xh))
    }
  }
    f(this, z)
}

  def grouped[B >: A](window: Int): List[List[B]] = {
    if (window == 0)
      this
    @tailrec
    def f(xs: List[B], window: Int, acc: List[List[B]], temp: List[B], count: Int): List[List[B]] = {
      this match {
        case Nil => {
          if count == 1
            then return acc // if there's no sublist(temp), just return listoflists
          else return acc.add_right(temp) // if there's a sublist (temp), push it in acc and then return
        }

        case Cons(head, tail) => {
          if count < window
            then f(tail, window, acc, temp.add_right(head), count + 1) // if (count < window) -> push element into sublist and count++
          else f(tail, window, acc.add_right(temp), Nil, 1) // if (count == window) -> push sublist (temp) in acc and empty temp
        }
      }
    }
    f(this, window, Nil, Nil, 1)
  }
/*
  def sliding[B >: A](window: Int): List[List[A]] = {
    @tailrec
    def()
    this match{
      case Nil => 
    }
  }

  def windowed[B >: A](step: Int, window: Int)
  */
}

import List.*
object List:
  def apply[A](xs: A*): List[A] = of(xs*)
  def of[A](xs: A*): List[A] =
    xs.foldRight(Nil: List[A]) { case (x, acc) => Cons(x, acc) }

@main def hello: Unit = 
  println("I work!")