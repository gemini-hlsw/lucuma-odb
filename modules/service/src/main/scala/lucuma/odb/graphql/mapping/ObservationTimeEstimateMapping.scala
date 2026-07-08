// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ObservationView

trait ObservationTimeEstimateMapping[F[_]] extends ObservationView[F]:

  import ObservationView.OriginalEstimate

  lazy val ObservationTimeEstimateMappings: List[TypeMapping] =
    List(
      ObjectMapping(ExecutionType / "originalEstimate")(
        SqlField("id", OriginalEstimate.SyntheticId, key = true, hidden = true),
        SqlObject("setup"),
        SqlField("setupCount", OriginalEstimate.SetupCount),
        SqlObject("science"),
        SqlObject("total")
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
      ObjectMapping(ExecutionType / "digest" / "value" / "estimate" / "setup")()
    )
