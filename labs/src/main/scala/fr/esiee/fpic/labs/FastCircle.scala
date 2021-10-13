package fr.esiee.fpic.labs:

    class FastCircle(r: Int) extends LazyCircle(r.toFloat):
        override lazy val perimeter: Float = Math.PI.floatValue * (r << 1)
        override def show(): Unit = print(s"O($radius")        

