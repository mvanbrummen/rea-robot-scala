trait Surface {
  def withinBounds(p: Point): Boolean
}
case class Table(val min: Point, val max: Point) extends Surface {
  require(min.x < max.x && min.y < max.y, "Min point must be less than max point")

  def withinBounds(p: Point): Boolean = (min.y to max.y contains p.y) && (min.x to max.x contains p.x)
}
