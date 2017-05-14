import Direction._

abstract sealed class Robot
case class PlacedRobot(val position: Point, val facing: Direction) extends Robot
case class UnplacedRobot() extends Robot

class ToyRobotSimulation(val table: Surface) {
  var robot: Robot = UnplacedRobot()

  def place(position: Point, facing: Direction) = robot = if (table withinBounds position) PlacedRobot(position, facing) else UnplacedRobot()

  def report() = robot match {
      case PlacedRobot(position, facing) => println(s"${position.x},${position.y},${facing.toString()}")
      case UnplacedRobot() => // remain silent
  }

  def move() = robot = robot match {
    case PlacedRobot(pos, dir) if table withinBounds pos + Direction.translation(dir) => PlacedRobot(pos + Direction.translation(dir), dir)
    case r @ _ => r
  }

  def rotateRight() = robot = robot match {
    case PlacedRobot(position, direction) => PlacedRobot(position, Direction.rotateRight(direction))
    case r @ UnplacedRobot() => r
  }

  def rotateLeft() = robot = robot match {
    case PlacedRobot(position, direction) => PlacedRobot(position, Direction.rotateLeft(direction))
    case r @ UnplacedRobot() => r
  }

}
