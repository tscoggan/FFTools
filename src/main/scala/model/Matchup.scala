package fftools
package model

import utils.StringUtils._
import utils.FileUtils
import com.typesafe.config.ConfigFactory

case class Matchup(week: Int, team1: Team, team2: Team) {

  override def toString: String = s"Week ${week}: $team1 vs $team2"

}

object Matchup {

  def parseFrom(fileName: String): List[Matchup] = FileUtils.readFile(fileName).tail
    .map(_.trim)
    .filter(_.nonEmpty)
    .map {
      nextLine =>
        val Array(week, team1, team2) = nextLine.splitCSV()
        Matchup(week.toInt, Teams.get(team1), Teams.get(team2))
    }

}

case class MatchupResult(matchup: Matchup, winner: Team, loser: Team) {

  override def toString: String = s"Week ${matchup.week}: $winner defeats $loser"

}

object Matchups {

  private val conf = ConfigFactory.load.getConfig("playoff_probabilities")

  val remainingMatchups: List[Matchup] = {
    val matchups = Matchups.parseFrom(conf.getString("remaining_matchups_file"))
    assert({
      val numberOfWeeks: Int = matchups.map(_.week).distinct.length
      matchups.flatMap(m => List(m.team1, m.team2)).groupBy(_.name).forall { case (_, matchups) => matchups.length == numberOfWeeks }
    }, "Invalid remaining matchups --- each team must have 1 matchup per week")
    matchups
  }

  def parseFrom(fileName: String): List[Matchup] = {
    Matchup.parseFrom(fileName)
  }

}
