// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.core.optics.Format
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.table.AllocationTable
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.graphql.table.ProposalTable
import lucuma.odb.graphql.table.TimingWindowView

trait TimeSpanMapping[F[_]] extends AllocationTable[F] with ProgramTable[F] with ProposalTable[F] with ObservationView[F] with GroupView[F] with TimingWindowView[F] {

  lazy val TimeSpanMapping: TypeMapping =
    SwitchMapping(
      TimeSpanType,
      List(
        ProgramType / "plannedTime" / "pi"            -> timeSpanMapping(ProgramTable.PlannedTime.Pi)(ProgramTable.Id),
        ProgramType / "plannedTime" / "uncharged"     -> timeSpanMapping(ProgramTable.PlannedTime.Uncharged)(ProgramTable.Id),
        ProgramType / "plannedTime" / "execution"     -> timeSpanMapping(ProgramTable.PlannedTime.Execution)(ProgramTable.Id),
        ObservationType / "plannedTime" / "pi"        -> timeSpanMapping(ObservationView.PlannedTime.Pi)(ObservationView.Id),
        ObservationType / "plannedTime" / "uncharged" -> timeSpanMapping(ObservationView.PlannedTime.Uncharged)(ObservationView.Id),
        ObservationType / "plannedTime" / "execution" -> timeSpanMapping(ObservationView.PlannedTime.Execution)(ObservationView.Id),
        IntensiveType / "totalTime"                   -> timeSpanMapping(ProposalTable.TotalTime)(ProposalTable.ProgramId),
        LargeProgramType / "totalTime"                -> timeSpanMapping(ProposalTable.TotalTime)(ProposalTable.ProgramId),
        AllocationType / "duration"                   -> timeSpanMapping(AllocationTable.Duration)(AllocationTable.ProgramId, AllocationTable.Partner),
        GroupType / "minimumInterval"                 -> timeSpanMapping(GroupView.MinInterval)(GroupView.MinIntervalId),
        GroupType / "maximumInterval"                 -> timeSpanMapping(GroupView.MaxInterval)(GroupView.MaxIntervalId),
        TimingWindowEndAfterType / "duration"         -> timeSpanMapping(TimingWindowView.End.After)(TimingWindowView.End.SyntheticId),
        TimingWindowRepeatType / "period"             -> timeSpanMapping(TimingWindowView.End.Repeat.Period)(TimingWindowView.End.SyntheticId)
      )
    )

  private def valueAs[A: io.circe.Encoder](name: String)(f: Format[A, TimeSpan]): CursorField[A] =
    FieldRef("value").as(name, f.reverseGet)

  private def timeSpanMapping(data: ColumnRef)(keys: ColumnRef*): ObjectMapping =
    ObjectMapping(
      tpe = TimeSpanType,
      fieldMappings =
        keyFields(keys: _*) ++ List(
        SqlField("value", data, hidden = true),
        valueAs("microseconds")(Format.fromPrism(TimeSpan.FromMicroseconds)),
        valueAs("milliseconds")(TimeSpan.FromMilliseconds),
        valueAs("seconds")(TimeSpan.FromSeconds),
        valueAs("minutes")(TimeSpan.FromMinutes),
        valueAs("hours")(TimeSpan.FromHours),
        valueAs("iso")(TimeSpan.FromString)
      )
    )

  private def keyFields(keys: ColumnRef*): List[FieldMapping] =
    keys.toList.zipWithIndex.map { (col, n) =>
      SqlField(s"key_$n", col, key = true, hidden = true)
    }

}
