package fftools

import utils.Logger._
import utils.FileUtils
import utils.FloatUtils._
import utils.MathUtils._
import com.typesafe.config.ConfigFactory
import model._

import scala.annotation.tailrec

object PlayoffProbabilitiesApp extends App {

  private val conf = ConfigFactory.load.getConfig("playoff_probabilities")

  import Standings._
  import Matchups._
  import Teams._

  log(s"\n### Current Standings: ###\n" + currentStandings.toString)
  log(s"\n### Remaining Matchups: ###\n" + remainingMatchups.sortBy(_.week).mkString("\n"))

  /*
    Represents a single possible outcome for the remainder of the season.  I.e. with a specific
    result for each remaining matchup.
   */
  case class Scenario(results: List[MatchupResult]) {

    def add(result: MatchupResult): Scenario = this.copy(results = result :: results)

    /*
      Update the current standings with this scenario
     */
    def applyToStandings: Standings = {
      case class TeamRecord(team: Team, wins: Int, losses: Int, ties: Int, pointsFor: Float)

      val updatedRecords: Map[String, TeamRecord] = allTeams.map { t =>
        val currentRecord: StandingsRow = currentStandings.get(t)
        val updatedWins: Int = currentRecord.wins + results.count(_.winner == t)
        val updatedLosses: Int = currentRecord.losses + results.count(_.loser == t)
        val updatedPointsFor: Float = currentRecord.pointsFor + results.map(_.pointsScoredBy(t)).sum
        (t.name, TeamRecord(t, updatedWins, updatedLosses, currentRecord.ties, updatedPointsFor))
      }.toMap

      val newStandingsRows: List[StandingsRow] = currentStandings.rows.map { row =>
        val newRecord: TeamRecord = updatedRecords(row.teamName)
        row.copy(wins = newRecord.wins, losses = newRecord.losses, ties = newRecord.ties, pointsFor = newRecord.pointsFor)
      }.sortBy(r => (r.winPercentage, r.pointsFor)).reverse.zipWithIndex // descending sort by win % with current total points as tiebreaker
        .map { case (row, index) => row.copy(rank = index + 1) }

      Standings(newStandingsRows)
    }

    override def toString: String = s"Scenario:\n\t" + results.sortBy(_.matchup.week).mkString("\n\t")
  }

  // use Monte Carlo sim to perform N simulations of remaining matchups:

  val allScenarios: Seq[Scenario] = (1 to conf.getInt("number_of_sim_iterations")) map {
    ???
  }

  val numberOfScenarios: Int = allScenarios.length

  log(s"Generated $numberOfScenarios scenarios")

  type PlayoffSeed = Int
  type Probability = Float

  val playoffSpots: Int = conf.getInt("number_of_playoff_spots")
  val playoffByes: Int = conf.getInt("number_of_playoff_byes")

  case class PlayoffProbabilities(team: Team, probabilitiesBySeed: Map[PlayoffSeed, Probability], avgRank: Float) {

    val playoffProbability: Probability = probabilitiesBySeed.values.sum // odds of making playoffs

    val playoffByeProbability: Probability = playoffByes match {
      case 0 => 0
      case _ => (1 to playoffByes).map(seed => probabilitiesBySeed(seed)).sum
    }

    def toCSVString: String = s"$team,${avgRank.rounded(2)},${playoffProbability.toPercent(2)}%,${playoffByeProbability.toPercent(2)}%,"+
      (1 to playoffSpots).map { seed => probabilitiesBySeed.getOrElse(seed, 0.0f)}.map(_.toPercent(2)+"%").mkString(",")
  }

  val allScenarioStandings: Seq[Standings] = allScenarios.map(_.applyToStandings)

  val playoffProbabilities: List[PlayoffProbabilities] = allTeams.map { team =>
    type Rank = Int // which place this team finished
    type NumberOfScenarios = Int // number of scenarios in which this team finished in the corresponding place
    val rankCounts: Map[Rank, NumberOfScenarios] = allScenarioStandings
      .groupBy(_.getRank(team))
      .map { case (rank, scenarios) => (rank, scenarios.length) }

    logDebug(s"rankCounts for $team:\n"+rankCounts.mkString("\n"))

    val avgRank: Float = weightedAvg(rankCounts.toSeq: _*).toFloat

    val probabilities: Map[PlayoffSeed, Probability] = (1 to playoffSpots).map { seed =>
      val probability: Probability = rankCounts.getOrElse(seed, 0) / numberOfScenarios
      (seed, probability)
    }.toMap
    PlayoffProbabilities(team, probabilities, avgRank)
  }.sortBy(p => (p.avgRank, p.playoffProbability)).reverse

  log("Finished generating playoff probabilities")

  // dump results to CSV file:
  val outputFile: String = conf.getString("output_file_path")
  val output: List[String] = s"Team,Avg Rank,Playoffs (6 spots),Bye (2 spots), 1st Seed, 2nd Seed, 3rd Seed, 4th Seed, 5th Seed, 6th Seed" :: playoffProbabilities.map(_.toCSVString)
  FileUtils.writeLinesToFile(output, outputFile, overwrite = true)

  log(s"Done!  Results written to $outputFile")
}
