package fftools

import com.typesafe.config.ConfigFactory
import utils.Logger._
import model._

object PlayoffProbabilitiesApp extends App {

  private val conf = ConfigFactory.load.getConfig("playoff_probabilities")

  log("Starting app...")

  val currentStandings = Standings.parseFrom(conf.getString("current_standings_file"))
  log(currentStandings.toString)
}
