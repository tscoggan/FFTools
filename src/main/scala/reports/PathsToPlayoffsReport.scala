package fftools
package reports

import model._
import PlayoffProbabilitiesApp.ScenarioStandings
import utils.Logger._

case class PathsToPlayoffsReport(currentStandings: Standings, allScenarios: Seq[ScenarioStandings]) {

  trait Event {
    def toString: String
  }

  type EventSet = Set[Event]

  case class MatchupWin(winner: Team, loser: Team) extends Event {
    override def toString: String = s"$winner defeats $loser"
  }

  object MatchupWin {
    def from(result: MatchupResult): MatchupWin = MatchupWin(result.winner, result.loser)
  }

  case class TiebreakerSwap(winner: Team, loser: Team) extends Event {
    override def toString: String = s"$winner wins tiebreaker over $loser"
  }

  case class Path(events: EventSet) {
    override def toString: String = events.mkString(" AND ")
  }

  def compareTiebreaker(oldStandings: Standings, newStandings: Standings): Seq[TiebreakerSwap] = {
    Teams.allTeams.combinations(2).toSeq.flatMap { case List(t1, t2) =>
      val oldWinner: Team = oldStandings.tiebreakerWinner(t1, t2)
      val newWinner: Team = newStandings.tiebreakerWinner(t1, t2)
      if (oldWinner != newWinner) Some(TiebreakerSwap(newWinner, oldWinner)) else None
    }
  }

  /**
   * Create report for the specified team --- shows the unique ways that this team can finish in the top N places in the standings
   *
   * @param places N (i.e. the team must finish in the top N places)
   * @return list of playoff paths (in order from most to least likely), or Nil if the team cannot finish in the top N
   */
  def createFor(team: Team, places: Int): List[Path] = {
    log(s"Creating 'paths to playoffs' report for $team to finish in top $places...")
    allScenarios.filter(_.standings.getRank(team) <= places) // scenarios in which specified team finishes in top N in standings
      .groupBy(_.teamRanks) // groups of scenarios that all produce the same standings
      .toList.sortBy { case (standings, scenarios) => scenarios.length }.reverse // start with most likely scenarios
      .map { case (standings, scenarios) =>
        // find events that are common across all scenarios that produce the same standings
        val commonEvents: EventSet = {
          val scenarioEventSets: Seq[EventSet] = scenarios.map { ss =>
            val wins: Seq[MatchupWin] = ss.scenario.results.map(MatchupWin.from(_))
            val tiebreakerSwaps: Seq[TiebreakerSwap] = compareTiebreaker(currentStandings, ss.standings)
            (wins ++ tiebreakerSwaps).toSet
          }
          scenarioEventSets.reduceLeft { (s1, s2) => s1.intersect(s2) }
        }
        logDebug("STANDINGS: \n\t" + standings.toList.sortBy(_._1).mkString("\n\t"))
        logDebug(s"${scenarios.length} SCENARIOS --- COMMON EVENTS: \n\t" + commonEvents.mkString("\n\t"))

        Path(commonEvents)
      }.toList
  }

}
