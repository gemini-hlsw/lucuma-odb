// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.Path
import lucuma.core.optics.Format
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.table.AllocationTable
import lucuma.odb.graphql.table.ChronConditionsEntryView
import lucuma.odb.graphql.table.Flamingos2DynamicView
import lucuma.odb.graphql.table.GmosDynamicTables
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ObscalcView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.graphql.table.ProposalView
import lucuma.odb.graphql.table.StepRecordView
import lucuma.odb.graphql.table.TimeChargeCorrectionTable
import lucuma.odb.graphql.table.TimeChargeDiscountTable
import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.graphql.table.VisitTable

trait TimeSpanMapping[F[_]] extends AllocationTable[F]
                               with Flamingos2DynamicView[F]
                               with GmosDynamicTables[F]
                               with ProgramTable[F]
                               with ProposalView[F]
                               with ObscalcView[F]
                               with ObservationView[F]
                               with GroupView[F]
                               with StepRecordView[F]
                               with TimeChargeCorrectionTable[F]
                               with TimeChargeDiscountTable[F]
                               with TimingWindowView[F]
                               with VisitTable[F]
                               with ChronConditionsEntryView[F] {

  import ObservationView.ScienceRequirements.Spectroscopy.ExposureTimeMode

  lazy val TimeSpanMappings: List[TypeMapping] =
    List(
      timeSpanMappingAtPath(AllocationType / "duration", AllocationTable.Duration)(AllocationTable.ProgramId, AllocationTable.Category, AllocationTable.ScienceBand),
      timeSpanMappingAtPath(ConditionsExpectationType / "timeframe", ChronConditionsEntryView.Intuition.Expectation.Timespan)(ChronConditionsEntryView.Intuition.Expectation.SyntheticId),
      timeSpanMappingAtPath(GroupType / "maximumInterval", GroupView.MaxInterval)(GroupView.MaxIntervalId),
      timeSpanMappingAtPath(GroupType / "minimumInterval", GroupView.MinInterval)(GroupView.MinIntervalId),
      timeSpanMappingAtPath(LargeProgramType / "totalTime", ProposalView.LargeProgram.TotalTime)(ProposalView.ProgramId),
      timeSpanMappingAtPath(StepRecordType / "estimate", StepRecordView.TimeEstimate)(StepRecordView.Id),
      timeSpanMappingAtPath(StepRecordType / "flamingos2" / "exposure", Flamingos2DynamicView.ExposureTime)(Flamingos2DynamicView.Id),
      timeSpanMappingAtPath(StepRecordType / "gmosNorth" / "exposure", GmosNorthDynamicTable.ExposureTime)(GmosNorthDynamicTable.Id),
      timeSpanMappingAtPath(StepRecordType / "gmosSouth" / "exposure", GmosSouthDynamicTable.ExposureTime)(GmosSouthDynamicTable.Id),
      timeSpanMappingAtPath(TimeAndCountExposureTimeModeType / "time", ExposureTimeMode.TimeAndCount.Time)(ExposureTimeMode.SyntheticId),
      timeSpanMappingAtPath(TimeChargeCorrectionType / "amount", TimeChargeCorrectionTable.Amount)(TimeChargeCorrectionTable.Id),
      timeSpanMappingAtPath(TimeChargeDiscountType / "program", TimeChargeDiscountTable.Amount)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeDaylightDiscountType / "program", TimeChargeDiscountTable.Amount)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeNoDataDiscountType / "program", TimeChargeDiscountTable.Amount)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeOverlapDiscountType / "program", TimeChargeDiscountTable.Amount)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeQaDiscountType / "program", TimeChargeDiscountTable.Amount)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeDiscountType / "amount", TimeChargeDiscountTable.Amount)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeDaylightDiscountType / "amount", TimeChargeDiscountTable.Amount)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeNoDataDiscountType / "amount", TimeChargeDiscountTable.Amount)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeOverlapDiscountType / "amount", TimeChargeDiscountTable.Amount)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeQaDiscountType / "amount", TimeChargeDiscountTable.Amount)(TimeChargeDiscountTable.VisitId),
      timeSpanMappingAtPath(TimeChargeInvoiceType / "executionTime" / "nonCharged", VisitTable.Raw.NonChargedTime)(VisitTable.Id),
      timeSpanMappingAtPath(TimeChargeInvoiceType / "executionTime" / "program", VisitTable.Raw.ProgramTime)(VisitTable.Id),
      timeSpanMappingAtPath(TimeChargeInvoiceType / "finalCharge" / "nonCharged", VisitTable.Final.NonChargedTime)(VisitTable.Id),
      timeSpanMappingAtPath(TimeChargeInvoiceType / "finalCharge" / "program", VisitTable.Final.ProgramTime)(VisitTable.Id),
      timeSpanMappingAtPath(TimingWindowEndAfterType / "after", TimingWindowView.End.After)(TimingWindowView.End.SyntheticId),
      timeSpanMappingAtPath(TimingWindowRepeatType / "period", TimingWindowView.End.Repeat.Period)(TimingWindowView.End.SyntheticId),
      timeSpanMappingAtPath(ObservationType / "observationDuration", ObservationView.ObservationDuration.ObservationDuration)(ObservationView.ObservationDuration.SyntheticId),
      timeSpanMappingAtPath(SetupTimeType / "full", ObscalcView.Digest.FullSetupTime)(ObscalcView.ObservationId),
      timeSpanMappingAtPath(SetupTimeType / "reacquisition", ObscalcView.Digest.ReacqSetupTime)(ObscalcView.ObservationId),
      timeSpanMappingAtPath(ExecutionDigestType / "acquisition" / "timeEstimate" / "nonCharged", ObscalcView.Digest.Acquisition.NonChargedTime)(ObscalcView.ObservationId),
      timeSpanMappingAtPath(ExecutionDigestType / "acquisition" / "timeEstimate" / "program", ObscalcView.Digest.Acquisition.ProgramTime)(ObscalcView.ObservationId),
      timeSpanMappingAtPath(ExecutionDigestType / "science" / "timeEstimate" / "nonCharged", ObscalcView.Digest.Science.NonChargedTime)(ObscalcView.ObservationId),
      timeSpanMappingAtPath(ExecutionDigestType / "science" / "timeEstimate" / "program", ObscalcView.Digest.Science.ProgramTime)(ObscalcView.ObservationId),
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
