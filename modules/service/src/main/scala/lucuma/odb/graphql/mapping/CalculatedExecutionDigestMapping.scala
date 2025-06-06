// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import io.circe.syntax.*
import lucuma.core.math.Offset
import lucuma.odb.graphql.table.ObscalcView
import lucuma.odb.json.offset.query.given

trait CalculatedExecutionDigestMapping[F[_]] extends ObscalcView[F]:

  private lazy val SetupTimeMapping: TypeMapping =
    ObjectMapping(SetupTimeType)(
      SqlField("synthetic_id", ObscalcView.Digest.Id, hidden = true, key = true),
      SqlObject("full"),
      SqlObject("reacquisition")
    )

  private def sequenceDigestMapping(
    fieldName: String,
    cols:      ObscalcView.Digest.SequenceDigest
  ): TypeMapping =
    ObjectMapping(ExecutionDigestType / fieldName)(
      SqlField("synthetic_id", cols.Id, hidden = true, key = true),
      SqlField("observeClass", cols.ObsClass),
      SqlObject("timeEstimate"),
      SqlField("_offsets", cols.Offsets, hidden = true),
      CursorFieldJson("offsets", c =>
        c.fieldAs[List[Long]]("_offsets")
         .map: os =>
           os.sliding(2, 2)
             .collect:
               case List(p, q) => Offset.signedMicroarcseconds.reverseGet((p, q))
             .toList
         .map(_.asJson),
        List("_offsets")
      ),
      SqlField("atomCount", cols.AtomCount),
      SqlField("executionState", cols.ExecutionState)
    )

  private lazy val AcquisitionSequenceDigestMapping: TypeMapping =
    sequenceDigestMapping("acquisition", ObscalcView.Digest.Acquisition)

  private lazy val ScienceSequenceDigestMapping: TypeMapping =
    sequenceDigestMapping("science", ObscalcView.Digest.Science)

  private lazy val ExecutionDigestMapping: TypeMapping =
    ObjectMapping(ExecutionDigestType)(
      SqlField("synthetic_id",     ObscalcView.Digest.Id, hidden = true, key = true),
      SqlObject("setup"),
      SqlObject("acquisition"),
      SqlObject("science")
    )

  private lazy val CalculatedExecutionDigestMapping: TypeMapping =
    ObjectMapping(CalculatedExecutionDigestType)(
      SqlField("synthetic_id",     ObscalcView.ObservationId, hidden = true, key = true),
      SqlField("state",            ObscalcView.CalculationState),
      SqlObject("value")
    )

  lazy val CalculatedExecutionDigestMappings: List[TypeMapping] = List(
      SetupTimeMapping,
      AcquisitionSequenceDigestMapping,
      ScienceSequenceDigestMapping,
      ExecutionDigestMapping,
      CalculatedExecutionDigestMapping
    )