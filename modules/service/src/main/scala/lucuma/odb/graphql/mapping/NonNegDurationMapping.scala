// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import edu.gemini.grackle.skunk.SkunkMapping
import io.circe.Encoder
import lucuma.core.model.Program
import lucuma.odb.graphql.table.AllocationTable
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.graphql.table.ProposalTable

import java.time.Duration

trait NonNegDurationMapping[F[_]]
  extends AllocationTable[F]
     with ProgramTable[F]
     with ProposalTable[F] { this: SkunkMapping[F] =>

  lazy val NonNegDurationType = schema.ref("NonNegDuration")

  lazy val NonNegDurationMapping =
    PrefixedMapping(
      tpe = NonNegDurationType,
      mappings = List(
        // Program
        List("plannedTime", "pi")        -> nonNegDurationMapping(ProgramTable.PlannedTime.Pi)(ProgramTable.Id),
        List("plannedTime", "uncharged") -> nonNegDurationMapping(ProgramTable.PlannedTime.Uncharged)(ProgramTable.Id),
        List("plannedTime", "execution") -> nonNegDurationMapping(ProgramTable.PlannedTime.Execution)(ProgramTable.Id),
        // Proposal
        List("totalTime") -> nonNegDurationMapping(ProposalTable.TotalTime)(ProposalTable.ProgramId),
        // Allocation
        List("duration") -> nonNegDurationMapping(AllocationTable.Duration)(AllocationTable.ProgramId, AllocationTable.Partner),
      ),
    )

  private def valueAs[A: io.circe.Encoder](name: String)(f: Duration => A): CursorField[A] =
    CursorField[A](name, c => c.fieldAs[Duration]("value").map(f), List("value"))

  private def nonNegDurationMapping(data: ColumnRef)(keys: ColumnRef*): ObjectMapping =
    ObjectMapping(
      tpe = NonNegDurationType,
      fieldMappings =
        keyFields(keys: _*) ++ List(
        SqlField("value", data, hidden = true),
        valueAs("microseconds")(d => d.toMillis * 1000L),
        valueAs("milliseconds")(d => BigDecimal(d.toMillis)),
        valueAs("seconds")(d => BigDecimal(d.toMillis) / BigDecimal(1000)),
        valueAs("minutes")(d => BigDecimal(d.toMillis) / BigDecimal(1000 * 60)),
        valueAs("hours")(d => BigDecimal(d.toMillis) / BigDecimal(1000 * 60 * 60)),
        valueAs("iso")(d => d.toString),
      ),
    )

  private def keyFields(keys: ColumnRef*): List[FieldMapping] =
    keys.toList.zipWithIndex.map { (col, n) =>
      SqlField(s"key_$n", col, key = true, hidden = true)
    }

}