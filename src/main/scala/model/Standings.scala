package fftools
package model

import utils.StringUtils._
import utils.FloatUtils._
import utils.FileUtils
import com.typesafe.config.ConfigFactory
import model.CustomTypes._

case class Standings(rows: List[StandingsRow]) {

  def get(team: Team): StandingsRow = rows.find(_.teamName == team.name) match {
    case Some(row) => row
    case None => throw new Exception(s"Team $team not found in current standings")
  }

  /*
    What place in the standings (1st, 2nd, etc) the specified team finished
   */
  def getRank(team: Team): Rank = get(team).rank

  def allRanks: Map[Rank, Team] = Teams.allTeams.map(t => (getRank(t), t)).toMap

  def playoffSeeds: Map[Rank, Team] = {
    val conf = ConfigFactory.load.getConfig("playoff_probabilities")
    rows.filter(_.rank <= conf.getInt("number_of_playoff_spots")).map(r => (r.rank,Teams.get(r.teamName))).toMap
  }

  def tiebreakerWinner(team1: Team, team2: Team): Team = {
    val team1Points = get(team1).pointsFor
    val team2Points = get(team2).pointsFor
    if (team1Points > team2Points) team1 else team2
  }

  def teamsWithSameRecordAs(team: Team): List[Team] = {
    val t: StandingsRow = get(team)
    rows.filter(r => r.teamName != team.name && r.wins == t.wins && r.losses == t.losses && r.ties == t.ties).map(r => Teams.get(r.teamName))
  }

  override def toString: String = rows.map(_.toString).mkString("\n")

}


object Standings {

  private val conf = ConfigFactory.load.getConfig("playoff_probabilities")

  val currentStandings: Standings = Standings.parseFrom(conf.getString("current_standings_file"))

  def parseFrom(fileName: String): Standings = {
    val rows = StandingsRow.parseFrom(fileName)
    Standings(rows)
  }

}

case class StandingsRow(
                         rank: Int,
                         teamName: String,
                         wins: Int,
                         losses: Int,
                         ties: Int,
                         pointsFor: Float
                       ) {

  lazy val winPercentage: Float = (wins + ties / 2f) / (wins + losses + ties)

  override def toString: String = s"${rank}) $teamName $wins-$losses-$ties (Win %: ${winPercentage.rounded(3)}) PF: $pointsFor"

}

object StandingsRow {

  def parseFrom(fileName: String): List[StandingsRow] = FileUtils.readFile(fileName).tail
    .map(_.trim)
    .filter(_.nonEmpty)
    .map {
      nextLine =>

        val Array(rank, team, wlt, ptsFor, other: _*) = nextLine.split("\t").map(_.trim)

        val Array(wins, losses, ties) = wlt.split("-").map(_.toInt)

        StandingsRow(rank.toInt,
          team.substringAfter("logo ").trim,
          wins,
          losses,
          ties,
          ptsFor.toFloat)
    }
}
