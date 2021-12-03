package fftools

import utils.Logger._
import model._

import scala.annotation.tailrec

object PlayoffProbabilitiesApp extends App {

  import Standings._
  import Matchups._

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
    def updateStandings: Standings = {
      ???
    }

    override def toString: String = s"Scenario:\n\t" + results.sortBy(_.matchup.week).mkString("\n\t")
  }

  // generate all possible matchup outcomes:

  @tailrec
  def simulateMatchups(remainingMatchups: List[Matchup], scenarios: List[Scenario] = Nil): List[Scenario] = {
    remainingMatchups.headOption match {
      case Some(matchup: Matchup) => {
        val team1Wins = MatchupResult(matchup, matchup.team1, matchup.team2)
        val team2Wins = MatchupResult(matchup, matchup.team2, matchup.team1)

        val newScenarios: List[Scenario] = scenarios match {
          case Nil => List(Scenario(List(team1Wins)), Scenario(List(team2Wins)))
          case _ => scenarios.flatMap(s => List(s.add(team1Wins), s.add(team2Wins)))
        }

        logDebug(s"...${newScenarios.length} scenarios")
        simulateMatchups(remainingMatchups.tail, newScenarios)
      }
      case None => scenarios
    }
  }

  val allScenarios: Seq[Scenario] = simulateMatchups(remainingMatchups)

  log(s"Generated ${allScenarios.length} scenarios")
}
