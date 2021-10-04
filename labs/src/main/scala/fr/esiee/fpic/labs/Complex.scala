package fr.esiee.fpic.labs:

  class Complex(val im: Float, val re: Float):
    def +(other: Complex) = Complex(re + other.re, im + other.im)

    def -(other: Complex) = Complex(re - other.re, im - other.im)

    override def toString: String = s"$re+i$im"
    // Please define:
    // - modulo
    // - multiplication
    // - division
    // - negation (hint: this is a unary op)

    def mod: Double = scala.math.sqrt(re*re+im*im)

    def *(other:Complex): Complex = Complex(re * other.re - im * other.im, im * other.re + other.im * re)

    def neg:Complex = Complex(-re,-im)

    def /(other:Complex): Complex =
      val n=Complex(re,im)*other.neg
      val d= other * other.neg
      Complex(n.re/d.re, n.im/d.re) 


