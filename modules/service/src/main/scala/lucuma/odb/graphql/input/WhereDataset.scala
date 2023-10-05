// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel._
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosShort
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.sequence.Step
import lucuma.odb.graphql.binding._

object WhereDataset {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereObservationBinding  = WhereObservation.binding(path / "observation")
    val WhereEqStepIdBinding     = WhereEq.binding[Step.Id](path / "id" / "stepId", StepIdBinding)
    val WhereOrderIndexBinding   = WhereOrder.binding[PosShort](path / "id" / "index", PosShortBinding)
    val QaStateBinding           = WhereOptionEq.binding[DatasetQaState](path / "qaState", enumeratedBinding[DatasetQaState])

    lazy val WhereDatasetBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereDatasetBinding.List.Option("AND", rAND),
        WhereDatasetBinding.List.Option("OR", rOR),
        WhereDatasetBinding.Option("NOT", rNOT),

        WhereObservationBinding.Option("observation", rObs),
        WhereEqStepIdBinding.Option("stepId", rStepId),
        WhereOrderIndexBinding.Option("index", rIndex),
        QaStateBinding.Option("qaState", rQa)
      ) =>
        (rAND, rOR, rNOT, rObs, rStepId, rIndex, rQa).parMapN { (AND, OR, NOT, obs, sid, index, qa) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            obs,
            sid,
            index,
            qa
          ).flatten)
        }
    }
  }

}

