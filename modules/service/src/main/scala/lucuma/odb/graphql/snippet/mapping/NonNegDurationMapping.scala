// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet.mapping

import edu.gemini.grackle.skunk.SkunkMapping
import io.circe.Encoder
import java.time.Duration
import lucuma.core.model.Program
import lucuma.odb.graphql.snippet.table.ProgramTable

trait NonNegDurationMapping[F[_]]
  extends ProgramTable[F] { this: SkunkMapping[F] =>

  lazy val NonNegDurationType = schema.ref("NonNegDuration")

  lazy val NonNegDurationMapping =
    PrefixedMapping(
      tpe = NonNegDurationType,
      mappings = List(
        // Program
        List("plannedTime", "pi")        -> nonNegDurationMapping(ProgramTable.Id, ProgramTable.PlannedTime.Pi),
        List("plannedTime", "uncharged") -> nonNegDurationMapping(ProgramTable.Id, ProgramTable.PlannedTime.Uncharged),
        List("plannedTime", "execution") -> nonNegDurationMapping(ProgramTable.Id, ProgramTable.PlannedTime.Execution),
      ),
    )

  private def valueAs[A: io.circe.Encoder](name: String)(f: Duration => A): CursorField[A] =
    CursorField[A](name, c => c.fieldAs[Duration]("value").map(f), List("value"))

  private def nonNegDurationMapping(key: ColumnRef, data: ColumnRef): ObjectMapping =
    ObjectMapping(
      tpe = NonNegDurationType,
      fieldMappings = List(
        SqlField("id", ProgramTable.Id, key = true, hidden = true),
        SqlField("value", data, hidden = true),
        valueAs("microseconds")(d => d.toMillis * 1000L),
        valueAs("milliseconds")(d => BigDecimal(d.toMillis)),
        valueAs("seconds")(d => BigDecimal(d.toMillis) / BigDecimal(1000)),
        valueAs("minutes")(d => BigDecimal(d.toMillis) / BigDecimal(1000 * 60)),
        valueAs("hours")(d => BigDecimal(d.toMillis) / BigDecimal(1000 * 60 * 60)),
        valueAs("iso")(d => d.toString),
      ),
    )

}