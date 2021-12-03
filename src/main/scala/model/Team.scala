package fftools
package model

case class Team(id: Int, name: String) {

  override def toString: String = name

}

object Teams {

  val allTeams: List[Team] = Standings.currentStandings.rows.map { row => Team(row.rank, row.teamName) }.distinct

  private val teamsByName: Map[String, Team] = allTeams.map{ t => (t.name, t)}.toMap

  def get(name: String): Team = teamsByName(name)
}
