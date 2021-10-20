package fr.esiee.fpic.labs:

// 42
// 42 + 7 
// (1+2)*(5+7)
// (a + b)
    enum Expression:
        case Val(v: Int)
        case Sum(a: Expression, b: Expression)
        case Mul(a: Expression, b: Expression)

        override def toString: String = 
            import Expression.*
            this match
            case Val(v) => v.toString
            case Sum(a, b) => "(" + a.toString + " + " + b.toString + ")"
            case Mul(a, b) => "(" + a.toString + " * " + b.toString + ")"


    def eval(e: Expression): Int =
     import Expression.*
     e match    
         case Val(v) => v
         case Sum(a, b) => eval(a) + eval(b)               
         case Mul(a, b) => eval(a) * eval(b)               

    def test_expr() = 
        import Expression.*
        val e = Sum(Mul(Val(2), Val(3)), Sum(Val(7),Val(8)))
        println(e)
        println(s"eval($e) = " + eval(e))
    