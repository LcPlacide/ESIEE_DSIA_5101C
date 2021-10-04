package fr.esiee.fpic.labs:

    import scala.annotation.tailrec

    def factorial(n: BigInt): BigInt = 
        if n == 0 then 1
        else n*factorial(n-1)

    def factorial2(n: BigInt): BigInt =
        n match
            case 0 => 1
            case _ => n*factorial2(n)

    def trFactorial(n: BigInt): BigInt =
        @tailrec
        def afact(a: BigInt, n: BigInt): BigInt =
            n match
                case 0 => a
                case _ => afact(n*a, n-1)
        afact(1, n)

    def gcd(a: Int, b: Int): Int =
        val (c, d) = if a > b then (a, b) else (b, a)
        val r = c % d
        if r == 0 then d 
        else gcd(d, r)        

    def max(xs: List[Int]): Option[Int] =
        xs match 
            case Nil => None
            case n::Nil => Some(n)
            case n::ns => max(ns).flatMap(x => Some(scala.math.max(x, n)))

    // Adding some Curry to Scala Functions... ;-)
    //def sum(a: Int)(b: Int): Int = a + b

    