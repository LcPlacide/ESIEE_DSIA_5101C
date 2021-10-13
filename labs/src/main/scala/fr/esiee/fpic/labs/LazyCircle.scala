package fr.esiee.fpic.labs:

    class LazyCircle(val radius: Float) extends LazyShape:

        def scale(a: Float): LazyShape = new LazyCircle(a * radius)
        def show(): Unit = print(s"Circle($radius)")

        lazy val area: Float = Math.PI.toFloat*radius*radius
        lazy val perimeter: Float = 2*Math.PI.toFloat*radius


