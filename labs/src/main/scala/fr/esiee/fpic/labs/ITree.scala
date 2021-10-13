package fr.esiee.fpic.labs

    enum ITree:
        case Empty
        case Leaf(v: Int)
        case Node(left: ITree, right: ITree)

        override def toString: String = this match
            case ITree.Empty => ""
            case ITree.Leaf(v) => v.toString
            case ITree.Node(l, r) => "(" + l + ", " + r + ")"
