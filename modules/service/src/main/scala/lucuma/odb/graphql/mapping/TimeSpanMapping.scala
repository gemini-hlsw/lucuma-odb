// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.core.optics.Format
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.table.AllocationTable
import lucuma.odb.graphql.table.ChronConditionsEntryView
import lucuma.odb.graphql.table.GmosDynamicTables
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.graphql.table.ProposalTable
import lucuma.odb.graphql.table.StepRecordView
import lucuma.odb.graphql.table.TimeChargeCorrectionTable
import lucuma.odb.graphql.table.TimeChargeDiscountTable
import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.graphql.table.VisitTable

trait TimeSpanMapping[F[_]] extends AllocationTable[F]
                               with GmosDynamicTables[F]
                               with ProgramTable[F]
                               with ProposalTable[F]
                               with ObservationView[F]
                               with GroupView[F]
                               with StepRecordView[F]
                               with TimeChargeCorrectionTable[F]
                               with TimeChargeDiscountTable[F]
                               with TimingWindowView[F]
                               with VisitTable[F]
                               with ChronConditionsEntryView[F] {

  lazy val TimeSpanMapping: TypeMapping =
    SwitchMapping(
      TimeSpanType,
      List(
        AllocationType / "duration"                               -> timeSpanMapping(AllocationTable.Duration)(AllocationTable.ProgramId, AllocationTable.Partner),
        ConditionsExpectationType / "timeframe"                   -> timeSpanMapping(ChronConditionsEntryView.Intuition.Expectation.Timespan)(ChronConditionsEntryView.Intuition.Expectation.SyntheticId),
        GmosNorthStepRecordType / "estimate"                      -> timeSpanMapping(StepRecordView.TimeEstimate)(StepRecordView.Id),
        GmosNorthStepRecordType / "instrumentConfig" / "exposure" -> timeSpanMapping(GmosNorthDynamicTable.ExposureTime)(GmosNorthDynamicTable.Id),
        GmosSouthStepRecordType / "estimate"                      -> timeSpanMapping(StepRecordView.TimeEstimate)(StepRecordView.Id),
        GmosSouthStepRecordType / "instrumentConfig" / "exposure" -> timeSpanMapping(GmosSouthDynamicTable.ExposureTime)(GmosSouthDynamicTable.Id),
        GroupType / "maximumInterval"                             -> timeSpanMapping(GroupView.MaxInterval)(GroupView.MaxIntervalId),
        GroupType / "minimumInterval"                             -> timeSpanMapping(GroupView.MinInterval)(GroupView.MinIntervalId),
        IntensiveType / "totalTime"                               -> timeSpanMapping(ProposalTable.TotalTime)(ProposalTable.ProgramId),
        LargeProgramType / "totalTime"                            -> timeSpanMapping(ProposalTable.TotalTime)(ProposalTable.ProgramId),
        StepRecordType / "estimate"                               -> timeSpanMapping(StepRecordView.TimeEstimate)(StepRecordView.Id),
        TimeChargeCorrectionType / "amount"                       -> timeSpanMapping(TimeChargeCorrectionTable.Amount)(TimeChargeCorrectionTable.Id),
        TimeChargeDiscountType / "partner"                        -> timeSpanMapping(TimeChargeDiscountTable.Partner)(TimeChargeDiscountTable.VisitId),
        TimeChargeDiscountType / "program"                        -> timeSpanMapping(TimeChargeDiscountTable.Program)(TimeChargeDiscountTable.VisitId),
        TimeChargeDaylightDiscountType / "partner"                -> timeSpanMapping(TimeChargeDiscountTable.Partner)(TimeChargeDiscountTable.VisitId),
        TimeChargeDaylightDiscountType / "program"                -> timeSpanMapping(TimeChargeDiscountTable.Program)(TimeChargeDiscountTable.VisitId),
        TimeChargeQaDiscountType / "partner"                      -> timeSpanMapping(TimeChargeDiscountTable.Partner)(TimeChargeDiscountTable.VisitId),
        TimeChargeQaDiscountType / "program"                      -> timeSpanMapping(TimeChargeDiscountTable.Program)(TimeChargeDiscountTable.VisitId),
        TimeChargeInvoiceType / "executionTime" / "nonCharged"    -> timeSpanMapping(VisitTable.Raw.NonChargedTime)(VisitTable.Id),
        TimeChargeInvoiceType / "executionTime" / "partner"       -> timeSpanMapping(VisitTable.Raw.PartnerTime)(VisitTable.Id),
        TimeChargeInvoiceType / "executionTime" / "program"       -> timeSpanMapping(VisitTable.Raw.ProgramTime)(VisitTable.Id),
        TimeChargeInvoiceType / "finalCharge" / "nonCharged"      -> timeSpanMapping(VisitTable.Final.NonChargedTime)(VisitTable.Id),
        TimeChargeInvoiceType / "finalCharge" / "partner"         -> timeSpanMapping(VisitTable.Final.PartnerTime)(VisitTable.Id),
        TimeChargeInvoiceType / "finalCharge" / "program"         -> timeSpanMapping(VisitTable.Final.ProgramTime)(VisitTable.Id),
        TimingWindowEndAfterType / "after"                        -> timeSpanMapping(TimingWindowView.End.After)(TimingWindowView.End.SyntheticId),
        TimingWindowRepeatType / "period"                         -> timeSpanMapping(TimingWindowView.End.Repeat.Period)(TimingWindowView.End.SyntheticId)
      )
    )

  private def valueAs[A: io.circe.Encoder](name: String)(f: Format[A, TimeSpan]): CursorField[A] =
    FieldRef("value").as(name, f.reverseGet)

  private def timeSpanMapping(data: ColumnRef)(keys: ColumnRef*): ObjectMapping =
    ObjectMapping(
      tpe = TimeSpanType,
      fieldMappings =
        keyFields(keys*) ++ List(
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
