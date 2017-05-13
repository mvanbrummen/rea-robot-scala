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
        case "QUIT" => running = false
        case _ => println("What you talkin' 'bout Willis?!")
      }
    }
  }

  object Direction extends Enumeration {
    type Direction = Value
    val North, East, South, West = Value
  }
  import Direction._

  case class Point(val x: Int, val y: Int)

  trait Surface {
    def withinBounds(p: Point): Boolean
  }
  case class Table(val min: Point, val max: Point) extends Surface {
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
      case r"PLACE (\d+)${x}\,(\d+)${y}\,(North|East|South|West)${direction}" => Some(Point(x.toInt, y.toInt), Direction withName direction)
      case _ => None
    }
  }

  var robot: Robot = PlacedRobot(Point(0,0), North)
  val table: Surface = Table(Point(0, 0), Point(4, 4))

  def place(position: Point, facing: Direction) = if (table.withinBounds(position)) robot = PlacedRobot(position, facing)

  def report() = robot match {
    case PlacedRobot(position, facing) => println(s"${position.x},${position.y},${facing.toString()}")
    case UnplacedRobot() => // remain silent
  }

  def move() = robot = robot match {
    case PlacedRobot(position, North) => PlacedRobot(Point(position.x, position.y + 1), North)
    case PlacedRobot(position, East) => PlacedRobot(Point(position.x + 1, position.y), East)
    case PlacedRobot(position, South) => PlacedRobot(Point(position.x, position.y - 1), South)
    case PlacedRobot(position, West) => PlacedRobot(Point(position.x - 1, position.y), West)
    case r @ UnplacedRobot() => r
  }

  def rotateRight() = robot = robot match {
    case PlacedRobot(position, North) => PlacedRobot(position, East)
    case PlacedRobot(position, East) => PlacedRobot(position, South)
    case PlacedRobot(position, South) => PlacedRobot(position, West)
    case PlacedRobot(position, West) => PlacedRobot(position, North)
    case r @ UnplacedRobot() => r
  }

  def rotateLeft() = robot = robot match {
    case PlacedRobot(position, North) => PlacedRobot(position, West)
    case PlacedRobot(position, West) => PlacedRobot(position, South)
    case PlacedRobot(position, South) => PlacedRobot(position, East)
    case PlacedRobot(position, East) => PlacedRobot(position, North)
    case r @ UnplacedRobot() => r
  }

}
