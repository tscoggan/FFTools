package fftools
package model

import com.typesafe.config.ConfigFactory
import utils.FileUtils
import utils.StringUtils._
import utils.FloatUtils._
import utils.MathUtils._
import utils.Logger._

case class Team(id: Int, name: String) {

  override def toString: String = name

  val (meanScore: Float, stdDevScore: Float) = {
    val conf = ConfigFactory.load.getConfig("playoff_probabilities")
    val scores: List[Double] = {
      val fileName = conf.getString("past_scores_file")
      FileUtils.readFile(fileName).tail
        .map(_.trim)
        .find(_.startsWith(name)) match {
        case None => throw new Exception(s"Past scores CSV file has no entry for $name")
        case Some(line) => line.splitCSV().toList.tail.map(_.toDouble)
      }
    }
    (mean(scores).toFloat, stdDev(scores).toFloat)
  }

  logDebug(s"TEAM $name --- ID: $id, Mean Score: ${meanScore.rounded(2)}, Std Dev: ${stdDevScore.rounded(2)}")

}

object Teams {

  val allTeams: List[Team] = Standings.currentStandings.rows.map { row => Team(row.rank, row.teamName) }.distinct

  private val teamsByName: Map[String, Team] = allTeams.map{ t => (t.name, t)}.toMap

  def get(name: String): Team = teamsByName(name)
}
