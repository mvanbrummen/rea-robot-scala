object Runner extends App {
  val robotSim = new ToyRobotSimulation(Table(Point(0,0), Point(4,4)))
  var running = true
  val PROMPT = "> "

  while(running) {
    print(PROMPT)
    scala.io.StdIn.readLine() match {
      case PlaceCommand(position, facing) => robotSim.place(position, facing)
      case "MOVE" => robotSim.move()
      case "LEFT" => robotSim.rotateLeft()
      case "RIGHT" => robotSim.rotateRight()
      case "REPORT" => robotSim.report()
      case "QUIT" | ":Q" => running = false
      case _ => println("Whachu' talkin' 'bout Willis?!")
    }
  }

  object PlaceCommand {
    import Direction._

    implicit class Regex(sc: StringContext) {
      def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }

    def unapply(s: String): Option[(Point, Direction)] = s match {
      case r"PLACE (\d+)${x}\,(\d+)${y}\,(NORTH|EAST|SOUTH|WEST)${direction}" => Some(Point(x.toInt, y.toInt), Direction withName direction.toLowerCase.capitalize)
      case _ => None
    }
  }
}
