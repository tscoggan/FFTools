package fftools
package model

import utils.StringUtils._
import utils.FileUtils
import utils.FloatUtils._
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

case class MatchupResult(matchup: Matchup, team1Score: Float, team2Score: Float) {

  val winner: Team = if (team1Score > team2Score) matchup.team1 else matchup.team2 // no need to simulate ties since they're so rare

  val loser: Team = if (winner == matchup.team1) matchup.team2 else matchup.team1

  def pointsScoredBy(team: Team): Float = team match {
    case matchup.team1 => team1Score
    case matchup.team2 => team2Score
    case _ => 0f
  }

  override def toString: String = s"Week ${matchup.week}: $winner (${math.max(team1Score, team2Score).rounded(2)}) defeats $loser (${math.min(team1Score, team2Score).rounded(2)})"

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
