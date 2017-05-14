case class Point(val x: Int, val y: Int) {
  def +(that: Point) = Point(x + that.x, y + that.y)
}
