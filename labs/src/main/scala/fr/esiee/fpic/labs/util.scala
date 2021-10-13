package fr.esiee.fpic.labs:

  import scala.annotation.tailrec
  @tailrec
  def loop(n: Int)(body: => Any): Unit =
    if n > 0 then
      val _ = body
      loop(n-1)(body)

  def lsum(xs: List[Int]): Int = xs match
    case y::ys => y+lsum(ys)
    // Nil is the same as List(), but more idiomatic
    case Nil => 0


  def lmul(xs: List[Int]): Int = xs match
    case y::ys => y*lmul(ys)
    case Nil => 1


  def reverse(xs: List[Int]): List[Int] = xs match
    case Nil => Nil
    case y::ys => reverse(ys) ::: List(y)

  
  def reversel(ys: List[Int]) = 
    ys.foldLeft(List[Int]())((xs: List[Int], x: Int) => x :: xs)

  def reverser(zs: List[Int]) = 
    zs.foldRight(List[Int]())((x: Int, xs: List[Int]) => xs ::: List(x) )

  def reverse_l_tr(xs: List[Int]) = 
    @tailrec
    def arev(rev: List[Int], rest: List[Int]): List[Int] = rest match
      case Nil => rev
      case x::xs => arev(x::rev, xs)
    arev(Nil, xs)