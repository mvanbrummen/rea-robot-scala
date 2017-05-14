object Runner {
  def main(args: Array[String]) = {
    val robotSim = new ToyRobotSimulation(UnplacedRobot(), Table(Point(0,0), Point(4,4)))
    robotSim.run()
  }
}
