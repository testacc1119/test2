package dyhas.bo.lab1

import scala.annotation.tailrec
import scala.collection.mutable

enum List[+A] {
  case Nil
  case Cons(hd: A, tl: List[A])

  override def toString: String = {
    @tailrec
    def go(sb: mutable.StringBuilder, as: List[A]): String = {
      as match {
        case Nil => sb.result
        case Cons(h, t) => go(sb.append(h).append(if t == Nil then "]" else ", "), t)
      }
    }
    go(new mutable.StringBuilder("["), this)
  }

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
    Cons(z, this)
  }

  def add_right[B >: A](z: B) : List[B] = {
    (Cons(z, this.reverse)).reverse
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
      List(this)
    @tailrec
    def f(xs: List[B], window: Int, acc: List[List[B]], temp: List[B], count: Int): List[List[B]] = {
      xs match {
        case Nil => {
          if count == 0
            then acc // if there's no sublist(temp), just return listoflists
          else acc.add_right(temp) // if there's a sublist (temp), push it in acc and then return
        }

        case Cons(head, tail) => {
          if count < window - 1
            then f(tail, window, acc, temp.add_right(head), count + 1) // if (count < window) -> push element into sublist and count++
          else f(tail, window, acc.add_right(temp.add_right(head)), Nil, 0) // if (count == window) -> push sublist (temp) in acc and empty temp
        }
      }
    }
    f(this, window, Nil, Nil, 0)
  }

  def sliding[B >: A](window: Int): List[List[B]] = {
    @tailrec
    def f(xs: List[B], window: Int, acc: List[List[B]]) : List[List[B]] = {

      @tailrec
      def into(xs: List[B], window: Int, acc: List[List[B]], temp: List[B], count: Int) : List[List[B]] ={
        xs match 
          case Nil => 
            if count == window // if there's no sublist(temp) or if there's a sublist (temp), which lenght is less than window, just return acc
              then acc.add_right(temp)
            else acc
          case Cons(head, tail) => {
            if count < window 
              then into (tail, window, acc, temp.add_right(head), count + 1) // if (count < window) -> push element into sublist and count++
            else acc.add_right(temp) // if (count == window) -> push sublist (temp) in acc and return acc
          }
      }

      xs match{
        case Nil => acc
        case Cons(head, tail) => f(tail, window, into(xs, window, acc, Nil, 0))
      }
    }
    f(this, window, Nil)
  }

//  def windowed[B >: A](step: Int, window: Int)

}

import List.*
object List:
  def apply[A](xs: A*): List[A] = of(xs*)
  def of[A](xs: A*): List[A] =
    xs.foldRight(Nil: List[A]) { case (x, acc) => Cons(x, acc) }

@main def hello: Unit = 
  val expected = List(List(1, 2, 3), List(2, 3, 4))
  var temp = List(1, 2, 3, 4)
  val actual = temp.sliding(3)
  println(expected.toString)
  println("\n")
  println(actual.toString)