object Direction extends Enumeration {
  type Direction = Value
  val North, East, South, West = Value

  def rotateRight(dir: Direction) = dir match {
    case North => East
    case East => South
    case South => West
    case West => North
  }

  def rotateLeft(dir: Direction) = dir match {
    case North => West
    case West => South
    case South => East
    case East => North
  }

  def translation(dir: Direction) = dir match {
    case North => Point(0, 1)
    case East => Point(1, 0)
    case South => Point(0, -1)
    case West => Point(-1, 0)
  }
}
