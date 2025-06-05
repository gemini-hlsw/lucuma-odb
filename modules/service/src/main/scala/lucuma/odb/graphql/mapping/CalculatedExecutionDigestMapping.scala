// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import lucuma.odb.graphql.table.ObscalcTable

trait CalculatedExecutionDigestMapping[F[_]] extends ObscalcTable[F]:

  private lazy val DigestPath: Path =
    CalculatedExecutionDigestType / "value"

  private lazy val SetupTimeMapping: TypeMapping =
    ObjectMapping(DigestPath / "setup")(
      SqlField("synthetic_id", ObscalcTable.ObservationId, hidden = true, key = true),
      SqlObject("full"),
      SqlObject("reacquisition")
    )

  private def sequenceDigestMapping(
    fieldName: String,
    cols:      ObscalcTable.Digest.SequenceDigest
  ): TypeMapping =
    ObjectMapping(DigestPath / fieldName)(
      SqlField("synthetic_id", ObscalcTable.ObservationId, hidden = true, key = true),
      SqlField("observeClass", cols.ObsClass),
      SqlObject("timeEstimate"),
      SqlField("offsets", cols.Offsets),
      SqlField("atomCount", cols.AtomCount),
      SqlField("executionState", cols.ExecutionState)
    )

  private lazy val AcquisitionSequenceDigestMapping: TypeMapping =
    sequenceDigestMapping("acquisition", ObscalcTable.Digest.Acquisition)

  private lazy val ScienceSequenceDigestMapping: TypeMapping =
    sequenceDigestMapping("science", ObscalcTable.Digest.Science)

  private lazy val ExecutionDigestMapping: TypeMapping =
    ObjectMapping(DigestPath)(
      SqlField("synthetic_id",     ObscalcTable.ObservationId, hidden = true, key = true),
      SqlObject("setup"),
      SqlObject("acquisition"),
      SqlObject("science")
    )

  private lazy val CalculatedExecutionDigestMapping: TypeMapping =
    ObjectMapping(CalculatedExecutionDigestType)(
      SqlField("synthetic_id",     ObscalcTable.ObservationId, hidden = true, key = true),
      SqlField("state",            ObscalcTable.CalculationState),
      SqlObject("value")
    )

  lazy val CalculatedExecutionDigestMappings: List[TypeMapping] = List(
      SetupTimeMapping,
      AcquisitionSequenceDigestMapping,
      ScienceSequenceDigestMapping,
      ExecutionDigestMapping,
      CalculatedExecutionDigestMapping
    )