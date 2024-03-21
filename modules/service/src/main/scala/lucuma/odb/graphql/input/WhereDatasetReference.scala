// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PosIntBinding

object WhereDatasetReference {

  def binding(path: Path): Matcher[Predicate] = {
    // match dataset reference labels on the String value so we can do
    // comparisons like 'LIKE: "G-2024A-0001-Q-0001-%"'.
    val WhereLabel          = WhereString.binding(path / "labelString")
    val WhereObservationRef = WhereObservationReference.binding(path / "observation")
    val WhereStepIndex      = WhereOrder.binding[PosInt](path / "stepIndex", PosIntBinding)
    val WhereExposureIndex  = WhereOrder.binding[PosInt](path / "exposureIndex", PosIntBinding)

    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        WhereLabel.Option("label", rRef),
        WhereObservationRef.Option("observation", rObservation),
        WhereStepIndex.Option("stepIndex", rStepIndex),
        WhereExposureIndex.Option("exposureIndex", rExposureIndex),
      ) => (rIsNull, rRef, rObservation, rStepIndex, rExposureIndex).parMapN {
        (isNull, ref, observation, stepIndex, exposureIndex) =>
          and(List(
            isNull.map(IsNull(path / "id", _)),
            ref,
            observation,
            stepIndex,
            exposureIndex
          ).flatten)
      }
    }
  }

}
