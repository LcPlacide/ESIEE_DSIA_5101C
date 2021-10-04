package fr.esiee.fpic.labs:

  class Rational(a: Int, b: Int):
    assert(b != 0)
    val num = a / gcd(a, b)
    val den = b / gcd(a,b)

    def this(n: Int) = this(n, 1)

    def inverse(r: Rational) = Rational(r.den, r.num)

    def *(other: Rational) =
      val n = num * other.num
      val m = den * other.den
      val d = gcd(n, m)
      Rational(n/d, m/d)

    def unary_- = Rational(-this.num, -this.den)

    override def toString: String = s"($num/$den)"

    // Please define sum, subtraction, division and negation

    def +(other:Rational):Rational = 
      val n = num*other.den+other.num*den
      val m = den*other.den
      val d = gcd(n,m)
      Rational(n/d, m/d)

    
    def -(other:Rational):Rational = 
      Rational(num,den) + other.unary_-

    def /(other:Rational):Rational =
      Rational(num,den) * other.inverse(other)



