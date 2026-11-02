// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.ObservationQaState
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ObservationQaStateBinding
import lucuma.odb.graphql.binding.WhereOptionEq

object WhereObservationQa:

  def binding(path: Path): Matcher[Predicate] =
    val WhereState    = WhereOptionEq.binding[ObservationQaState](path / "state",    ObservationQaStateBinding)
    val WhereComputed = WhereOptionEq.binding[ObservationQaState](path / "computed", ObservationQaStateBinding)
    val WhereOverride = WhereOptionEq.binding[ObservationQaState](path / "override", ObservationQaStateBinding)
    val WhereDatasets = WhereDatasetQaCounts.binding(path / "datasets")

    ObjectFieldsBinding.rmap:
      case List(
        WhereState.Option("state", rState),
        WhereComputed.Option("computed", rComputed),
        WhereOverride.Option("override", rOverride),
        WhereDatasets.Option("datasets", rDatasets)
      ) => (rState, rComputed, rOverride, rDatasets).parMapN: (state, computed, over, datasets) =>
        and(List(state, computed, over, datasets).flatten)
