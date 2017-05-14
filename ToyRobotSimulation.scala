import Direction._

abstract sealed class Robot
case class PlacedRobot(val position: Point, val facing: Direction) extends Robot
case class UnplacedRobot() extends Robot

class ToyRobotSimulation(val table: Surface) {
  var robot: Robot = UnplacedRobot()
  var running = true

  def run() = {
    val PROMPT = "> "

    while(running) {
      print(PROMPT)
      robot = scala.io.StdIn.readLine() match {
        case PlaceCommand(position, facing) => place(position, facing)
        case "MOVE" => move()
        case "LEFT" => rotateLeft()
        case "RIGHT" => rotateRight()
        case "REPORT" => report()
        case "QUIT" | ":Q" => quit()
        case _ => unknownCommand()
      }
    }
  }

  object PlaceCommand {
    implicit class Regex(sc: StringContext) {
      def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }

    def unapply(s: String): Option[(Point, Direction)] = s match {
      case r"PLACE (\d+)${x}\,(\d+)${y}\,(NORTH|EAST|SOUTH|WEST)${direction}" => Some(Point(x.toInt, y.toInt), Direction withName direction.toLowerCase.capitalize)
      case _ => None
    }
  }

  def quit() = {
    running = false
    UnplacedRobot()
  }

  def unknownCommand() = {
    println("What you talkin' 'bout Willis?!")
    robot
  }

  def place(position: Point, facing: Direction) = if (table withinBounds position) PlacedRobot(position, facing) else UnplacedRobot()

  def report() = {
    robot match {
      case PlacedRobot(position, facing) => println(s"${position.x},${position.y},${facing.toString()}")
      case UnplacedRobot() => // remain silent
    }
    robot
  }

  def move() = robot match {
    case PlacedRobot(pos, dir) if table withinBounds pos + Direction.translation(dir) => PlacedRobot(pos + Direction.translation(dir), dir)
    case r @ _ => r
  }

  def rotateRight() = robot match {
    case PlacedRobot(position, direction) => PlacedRobot(position, Direction.rotateRight(direction))
    case r @ UnplacedRobot() => r
  }

  def rotateLeft() = robot match {
    case PlacedRobot(position, direction) => PlacedRobot(position, Direction.rotateLeft(direction))
    case r @ UnplacedRobot() => r
  }

}
