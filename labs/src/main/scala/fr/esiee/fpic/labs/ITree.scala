package fr.esiee.fpic.labs:

    enum ITree:
        case Empty
        case Leaf(v: Int)
        case Node(left: ITree, right: ITree)

        override def toString: String = this match
            case ITree.Empty => ""
            case ITree.Leaf(v) => v.toString
            case ITree.Node(l, r) => "(" + l + ", " + r + ")"

        def height:Int= this match {
            case ITree.Empty => 0
            case ITree.Leaf(_) => 1
            case ITree.Node(l,r) => 1 + scala.math.max(l.height,r.height)
        }

        def sum:Int = this match {
            case ITree.Empty => 0
            case ITree.Leaf(v) => v
            case ITree.Node(l,r) => l.sum + r.sum
        }

        def fold(zero:Int)(f:(x: Int, y: Int) => Int):Int= this match {
            case ITree.Empty => zero
            case ITree.Leaf(v) => v
            case ITree.Node(l,r) => f(l.fold(zero)(f), r.fold(zero)(f))  
        }
