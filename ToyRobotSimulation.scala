object ToyRobotSimulation {
  def main(args: Array[String]) = {
    val PROMPT = "> "
    var running = true

    while(running) {
      print(PROMPT)
      scala.io.StdIn.readLine() match {
        case PlaceCommand(position, facing) => place(position, facing)
        case "MOVE" => move()
        case "LEFT" => rotateLeft()
        case "RIGHT" => rotateRight()
        case "REPORT" => report()
        case "QUIT" | ":Q" => running = false
        case _ => println("What you talkin' 'bout Willis?!")
      }
    }
  }

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
  import Direction._

  case class Point(val x: Int, val y: Int) {
    def +(that: Point) = Point(x + that.x, y + that.y)
  }

  trait Surface {
    def withinBounds(p: Point): Boolean
  }
  case class Table(val min: Point, val max: Point) extends Surface {
    require(min.x < max.x && min.y < max.y, "Min point must be less than max point")

    def withinBounds(p: Point): Boolean = (min.y to max.y contains p.y) && (min.x to max.x contains p.x)
  }

  abstract sealed class Robot
  case class PlacedRobot(val position: Point, val facing: Direction) extends Robot
  case class UnplacedRobot() extends Robot

  object PlaceCommand {
    implicit class Regex(sc: StringContext) {
      def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }

    def unapply(s: String): Option[(Point, Direction)] = s match {
      case r"PLACE (\d+)${x}\,(\d+)${y}\,(NORTH|EAST|SOUTH|WEST)${direction}" => Some(Point(x.toInt, y.toInt), Direction withName direction.toLowerCase.capitalize)
      case _ => None
    }
  }

  var robot: Robot = UnplacedRobot()
  val table: Surface = Table(Point(0, 0), Point(4, 4))

  def place(position: Point, facing: Direction) = if (table.withinBounds(position)) robot = PlacedRobot(position, facing)

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
