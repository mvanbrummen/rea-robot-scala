// sealed trait Direction {
//   def productPrefix: String
// }
// case object North extends Direction
// case object East extends Direction
// case object South extends Direction
// case object West extends Direction

object Direction extends Enumeration {
  type Direction = Value
  val North, East, South, West = Value
}

case class Point(val x: Int, val y: Int)

abstract sealed class Robot
case class PlacedRobot(val position: Point, val facing: Direction) extends Robot
case class UnplacedRobot() extends Robot

object PlaceCommand {
  def unapply(s: String): Option[(Point, Direction)] = s match {
    case r"^PLACE \d+\,\d+\,(NORTH|EAST|SOUTH|WEST)$" => {
      val tokens = s.replace("PLACE ", "").split(",")
      Some(Point(tokens(0).toInt, tokens(1).toInt), )
    }
    case _ => None
  }
}

object ReaRobot {
  var robot: Robot = PlacedRobot(Point(0,0), North)

  def main(args: Array[String]) = {
    val PROMPT = "> "
    var running = true

    // val board = (p: Point, min: Point, max: Point) => (p.x > min.x && p.y > min.y) && (p.x < max.x && p.y < max.y)

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

  def place(position: Point, facing: Direction) = {

  }

  def report() = robot match {
    case PlacedRobot(position, facing) => println(s"${position.x},${position.y},${facing.productPrefix}")
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
