// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.refined.given
import lucuma.odb.graphql.table.ObservationView

trait ObservationTimeEstimateMapping[F[_]] extends ObservationView[F]:

  import ObservationView.OriginalEstimate

  lazy val ObservationTimeEstimateMappings: List[TypeMapping] =
    List(
      ObjectMapping(ExecutionType / "originalEstimate")(
        SqlField("id", OriginalEstimate.SyntheticId, key = true, hidden = true),
        SqlField("existingCount", OriginalEstimate.ExistCalCount, hidden = true),
        SqlField("expectedCount", OriginalEstimate.ExpCalCount, hidden = true),
        SqlObject("setup"),
        SqlField("setupCount", OriginalEstimate.SetupCount),
        SqlField("reacquisitionCount", OriginalEstimate.ReacquisitionCount),
        SqlObject("calibrations"),
        calibrationCount("calibrationCount"),
        SqlObject("science"),
        SqlObject("total")
      ),

      ObjectMapping(ExecutionType / "originalEstimate" / "calibrations")(
        SqlField("id", OriginalEstimate.SyntheticId, key = true, hidden = true),
        SqlField("existingCount", OriginalEstimate.ExistCalCount, hidden = true),
        SqlField("expectedCount", OriginalEstimate.ExpCalCount, hidden = true),
        calibrationCount("count"),
        SqlObject("existing"),
        SqlObject("expected")
      ),

      ObjectMapping(ExecutionType / "originalEstimate" / "calibrations" / "existing")(
        SqlField("id", OriginalEstimate.SyntheticId, key = true, hidden = true),
        SqlField("count", OriginalEstimate.ExistCalCount),
        SqlObject("time")
      ),

      ObjectMapping(ExecutionType / "originalEstimate" / "calibrations" / "expected")(
        SqlField("id", OriginalEstimate.SyntheticId, key = true, hidden = true),
        SqlField("count", OriginalEstimate.ExpCalCount),
        SqlObject("time")
      ),

      ObjectMapping(ExecutionType / "originalEstimate" / "setup")(
        SqlField("id", OriginalEstimate.SyntheticId, key = true, hidden = true),
        SqlObject("full"),
        SqlObject("reacquisition")
      ),

      // `ObservationTimeEstimate` and `SetupTime` also appear inside the
      // `ExecutionDigest`, which is served as a single JSON blob by the
      // `digest` EffectField (a subtree).  We must NOT SQL-map those JSON
      // occurrences, but we do have to declare them: Grackle applies a type's
      // sole `ObjectMapping` unconditionally (a type with exactly one mapping
      // is indexed without consulting its path predicate), so the SQL mappings
      // above would otherwise leak into the JSON digest subtree and break the
      // subtree exemption for the nested `CategorizedTime` fields.  Declaring
      // these empty, path-scoped mappings forces both types into Grackle's
      // predicated index; their fields are then resolved from the digest JSON
      // as normal.
      ObjectMapping(ExecutionType / "digest" / "value" / "estimate")(),
      ObjectMapping(ExecutionType / "digest" / "value" / "estimate" / "setup")(),
      ObjectMapping(ExecutionType / "digest" / "value" / "estimate" / "calibrations")(),
      ObjectMapping(ExecutionType / "digest" / "value" / "estimate" / "calibrations" / "existing")(),
      ObjectMapping(ExecutionType / "digest" / "value" / "estimate" / "calibrations" / "expected")()
    )

  // The count is not stored: it is the existing plus the expected calibrations.
  private def calibrationCount(name: String): CursorField[NonNegInt] =
    CursorField(
      name,
      cursor =>
        for
          i <- cursor.fieldAs[NonNegInt]("existingCount")
          p <- cursor.fieldAs[NonNegInt]("expectedCount")
        yield NonNegInt.unsafeFrom(i.value + p.value),
      List("existingCount", "expectedCount")
    )
