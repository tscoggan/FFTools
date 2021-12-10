package fftools

import utils.Logger._
import utils.FileUtils
import utils.FloatUtils._
import utils.MathUtils._
import com.typesafe.config.ConfigFactory
import model._
import model.CustomTypes._

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

  val allScenarios: Seq[Scenario] = (1 to conf.getInt("number_of_sim_iterations")) map { _ =>
    val matchupResults: List[MatchupResult] = remainingMatchups.map{ matchup =>
      val team1Score: Float = randomNumber(matchup.team1.meanScore, matchup.team1.stdDevScore).toFloat
      val team2Score: Float = randomNumber(matchup.team2.meanScore, matchup.team2.stdDevScore).toFloat
      MatchupResult(matchup, team1Score, team2Score)
    }
    Scenario(matchupResults)
  }

  val numberOfScenarios: Int = allScenarios.length

  log(s"Generated $numberOfScenarios allScenarios")

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

  case class ScenarioStandings(scenario: Scenario, standings: Standings) {
    def teamRanks: Map[Rank, Team] = standings.allRanks
    def playoffSeeds: Map[Rank, Team] = standings.playoffSeeds
  }

  val allScenarioStandings: Seq[ScenarioStandings] = allScenarios.map(s => ScenarioStandings(s, s.applyToStandings))

  val playoffProbabilities: List[PlayoffProbabilities] = allTeams.map { team =>
    type NumberOfScenarios = Int // number of allScenarios in which this team finished in the corresponding place
    val rankCounts: Map[Rank, NumberOfScenarios] = allScenarioStandings.map(_.standings)
      .groupBy(_.getRank(team))
      .map { case (rank, scenarios) => (rank, scenarios.length) }

    logDebug(s"rankCounts for $team:\n"+rankCounts.mkString("\n"))

    val avgRank: Float = weightedAvg(rankCounts.toSeq: _*).toFloat

    val probabilities: Map[PlayoffSeed, Probability] = (1 to playoffSpots).map { seed =>
      val probability: Probability = rankCounts.getOrElse(seed, 0).toFloat / numberOfScenarios
      (seed, probability)
    }.toMap
    PlayoffProbabilities(team, probabilities, avgRank)
  }.sortBy(_.avgRank)

  log("Finished generating playoff probabilities")

  val eliminatedTeams: List[Team] = playoffProbabilities.filter(_.playoffProbability == 0).map(_.team)

  log("Eliminated teams: \n\t"+eliminatedTeams.mkString("\n\t"))

  // dump results to CSV file:
  val outputFile: String = conf.getString("output_file_path")
  val output: List[String] = s"Team,Avg Finish,Playoffs (6 spots),Bye (2 spots), 1st Seed, 2nd Seed, 3rd Seed, 4th Seed, 5th Seed, 6th Seed" :: playoffProbabilities.map(_.toCSVString)
  FileUtils.writeLinesToFile(output, outputFile, overwrite = true)

  log(s"Done!  Results written to $outputFile")

  val report = reports.PathsToPlayoffsReport(currentStandings, allScenarioStandings, eliminatedTeams)
  report.createFor(allTeams.find(_.name == "Show Me Your TDs").get, 6)
}
