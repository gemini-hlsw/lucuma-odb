// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.Path
import lucuma.core.optics.Format
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.table.AllocationTable
import lucuma.odb.graphql.table.ChronConditionsEntryView
import lucuma.odb.graphql.table.GmosDynamicTables
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.graphql.table.ProposalView
import lucuma.odb.graphql.table.StepRecordView
import lucuma.odb.graphql.table.TimeChargeCorrectionTable
import lucuma.odb.graphql.table.TimeChargeDiscountTable
import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.graphql.table.VisitTable

trait TimeSpanMapping[F[_]] extends AllocationTable[F]
                               with GmosDynamicTables[F]
                               with ProgramTable[F]
                               with ProposalView[F]
                               with ObservationView[F]
                               with GroupView[F]
                               with StepRecordView[F]
                               with TimeChargeCorrectionTable[F]
                               with TimeChargeDiscountTable[F]
                               with TimingWindowView[F]
                               with VisitTable[F]
                               with ChronConditionsEntryView[F] {

  lazy val TimeSpanMappings: List[TypeMapping] =
    List(
      timeSpanMappingAtPath(AllocationType / "duration", AllocationTable.Duration)(AllocationTable.ProgramId, AllocationTable.Partner),
      timeSpanMappingAtPath(ConditionsExpectationType / "timeframe", ChronConditionsEntryView.Intuition.Expectation.Timespan)(ChronConditionsEntryView.Intuition.Expectation.SyntheticId),
      timeSpanMappingAtPath(GroupType / "maximumInterval", GroupView.MaxInterval)(GroupView.MaxIntervalId),
      timeSpanMappingAtPath(GroupType / "minimumInterval", GroupView.MinInterval)(GroupView.MinIntervalId),
      timeSpanMappingAtPath(CallPropertiesLargeProgramType / "totalTime", ProposalView.LargeProgram.TotalTime)(ProposalView.ProgramId),
      timeSpanMappingAtPath(StepRecordType / "estimate", StepRecordView.TimeEstimate)(StepRecordView.Id),
      timeSpanMappingAtPath(StepRecordType / "gmosNorth" / "exposure", GmosNorthDynamicTable.ExposureTime)(GmosNorthDynamicTable.Id),
      timeSpanMappingAtPath(StepRecordType / "gmosSouth" / "exposure", GmosSouthDynamicTable.ExposureTime)(GmosSouthDynamicTable.Id),
      timeSpanMappingAtPath(TimeChargeCorrectionType / "amount", TimeChargeCorrectionTable.Amount)(TimeChargeCorrectionTable.Id),
      timeSpanMappingAtPath(TimeChargeDiscountType / "partner", TimeChargeDiscountTable.Partner)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeDiscountType / "program", TimeChargeDiscountTable.Program)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeDaylightDiscountType / "partner", TimeChargeDiscountTable.Partner)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeDaylightDiscountType / "program", TimeChargeDiscountTable.Program)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeQaDiscountType / "partner", TimeChargeDiscountTable.Partner)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeQaDiscountType / "program", TimeChargeDiscountTable.Program)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeInvoiceType / "executionTime" / "nonCharged", VisitTable.Raw.NonChargedTime)(VisitTable.Id),
      timeSpanMappingAtPath(TimeChargeInvoiceType / "executionTime" / "partner", VisitTable.Raw.PartnerTime)(VisitTable.Id),
      timeSpanMappingAtPath(TimeChargeInvoiceType / "executionTime" / "program", VisitTable.Raw.ProgramTime)(VisitTable.Id),
      timeSpanMappingAtPath(TimeChargeInvoiceType / "finalCharge" / "nonCharged", VisitTable.Final.NonChargedTime)(VisitTable.Id),
      timeSpanMappingAtPath(TimeChargeInvoiceType / "finalCharge" / "partner", VisitTable.Final.PartnerTime)(VisitTable.Id),
      timeSpanMappingAtPath(TimeChargeInvoiceType / "finalCharge" / "program", VisitTable.Final.ProgramTime)(VisitTable.Id),
      timeSpanMappingAtPath(TimingWindowEndAfterType / "after", TimingWindowView.End.After)(TimingWindowView.End.SyntheticId),
      timeSpanMappingAtPath(TimingWindowRepeatType / "period", TimingWindowView.End.Repeat.Period)(TimingWindowView.End.SyntheticId)
    )

  private def valueAs[A: io.circe.Encoder](name: String)(f: Format[A, TimeSpan]): CursorField[A] =
    FieldRef("value").as(name, f.reverseGet)

  private def timeSpanMappingAtPath(path: Path, data: ColumnRef)(keys: ColumnRef*): ObjectMapping =
    ObjectMapping(path)(
      keyFields(keys*) ++ List(
      SqlField("value", data, hidden = true),
      valueAs("microseconds")(Format.fromPrism(TimeSpan.FromMicroseconds)),
      valueAs("milliseconds")(TimeSpan.FromMilliseconds),
      valueAs("seconds")(TimeSpan.FromSeconds),
      valueAs("minutes")(TimeSpan.FromMinutes),
      valueAs("hours")(TimeSpan.FromHours),
      valueAs("iso")(TimeSpan.FromString)
    )*)

  private def keyFields(keys: ColumnRef*): List[FieldMapping] =
    keys.toList.zipWithIndex.map { (col, n) =>
      SqlField(s"key_$n", col, key = true, hidden = true)
    }

}
