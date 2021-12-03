package fftools
package model

case class Matchup(week: Int, team1: String, team2: String)

case class MatchupResult(matchup: Matchup, winner: Team, loser: Team)
