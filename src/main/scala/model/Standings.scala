package fftools
package model

import utils.StringUtils._
import utils.FloatUtils._
import utils.FileUtils
import com.typesafe.config.ConfigFactory

case class Standings(rows: List[StandingsRow]) {

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
                         pointsFor: Float,
                         pointsAgainst: Float,
                         streak: String,
                         waiver: Int,
                         moves: Int
                       ) {

  lazy val winPercentage: Float = (wins + ties/2f) / (wins + losses + ties)

  override def toString: String = s"${rank}) $teamName $wins-$losses-$ties (Win %: ${winPercentage.rounded(3)}) PF: $pointsFor PA: $pointsAgainst $streak $waiver $moves"

}

object StandingsRow {

  def parseFrom(fileName: String): List[StandingsRow] = FileUtils.readFile(fileName).tail
    .map(_.trim)
    .filter(_.nonEmpty)
    .map {
      nextLine =>

        val Array(rank, team, wlt, ptsFor, ptsAgainst, streak, waiver, moves) = nextLine.split("\t").map(_.trim)

        val Array(wins, losses, ties) = wlt.split("-").map(_.toInt)

        StandingsRow(rank.toInt,
          team.substringAfter("logo ").trim,
          wins,
          losses,
          ties,
          ptsFor.toFloat,
          ptsAgainst.toFloat,
          streak,
          waiver.toInt,
          moves.toInt
        )
    }
}
