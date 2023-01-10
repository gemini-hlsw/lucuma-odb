// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.data.Ior
import edu.gemini.grackle.Result
import edu.gemini.grackle.skunk.SkunkMapping
import io.circe.Encoder
import lucuma.core.model.Program
import lucuma.odb.graphql.table.AllocationTable
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.graphql.table.ProposalTable
import lucuma.odb.graphql.util.MappingExtras

import java.time.Duration

trait NonNegDurationMapping[F[_]] extends AllocationTable[F] with ProgramTable[F] with ProposalTable[F] {

  lazy val NonNegDurationMapping: TypeMapping =
    SwitchMapping(
      NonNegDurationType,
      List(
        (PlannedTimeSummaryType, "pi",        nonNegDurationMapping(ProgramTable.PlannedTime.Pi)(ProgramTable.Id)),
        (PlannedTimeSummaryType, "uncharged", nonNegDurationMapping(ProgramTable.PlannedTime.Uncharged)(ProgramTable.Id)),
        (PlannedTimeSummaryType, "execution", nonNegDurationMapping(ProgramTable.PlannedTime.Execution)(ProgramTable.Id)),
        (IntensiveType,          "totalTime", nonNegDurationMapping(ProposalTable.TotalTime)(ProposalTable.ProgramId)),
        (LargeProgramType,       "totalTime", nonNegDurationMapping(ProposalTable.TotalTime)(ProposalTable.ProgramId)),
        (AllocationType,         "duration",  nonNegDurationMapping(AllocationTable.Duration)(AllocationTable.ProgramId, AllocationTable.Partner)),
      ),
    )

  private def valueAs[A: io.circe.Encoder](name: String)(f: Duration => A): CursorField[A] =
    FieldRef("value").as(name, f)

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