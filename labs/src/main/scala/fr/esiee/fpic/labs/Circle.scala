package fr.esiee.fpic.labs

class Circle(val radius: Float) extends Shape:

  def scale(a: Float): Shape = new Circle(a * radius)

  def show(): Unit = print(s"Circle($radius)")

  def area: Float = Math.PI.toFloat*radius*radius

  // This shows that you can implement def with vals
  val perimeter: Float = 2*Math.PI.toFloat*radius
