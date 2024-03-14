// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel._
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.odb.graphql.binding._

object WhereDataset {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderDatasetIdBinding = WhereOrder.binding[Dataset.Id](path / "id", DatasetIdBinding)
    val WhereObservationBinding    = WhereObservation.binding(path / "observation")
    val WhereEqStepIdBinding       = WhereEq.binding[Step.Id](path / "step" / "id", StepIdBinding)
    val WhereOrderIndexBinding     = WhereOrder.binding[PosInt](path / "index", PosIntBinding)
    val WhereFilenameBinding       = WhereString.binding(path / "filename")
    val QaStateBinding             = WhereOptionEq.binding[DatasetQaState](path / "qaState", enumeratedBinding[DatasetQaState])

    lazy val WhereDatasetBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereDatasetBinding.List.Option("AND", rAND),
        WhereDatasetBinding.List.Option("OR", rOR),
        WhereDatasetBinding.Option("NOT", rNOT),

        WhereOrderDatasetIdBinding.Option("id", rId),
        WhereObservationBinding.Option("observation", rObs),
        WhereEqStepIdBinding.Option("stepId", rStepId),
        WhereOrderIndexBinding.Option("index", rIndex),
        WhereFilenameBinding.Option("filename", rFile),
        QaStateBinding.Option("qaState", rQa)
      ) =>
        (rAND, rOR, rNOT, rId, rObs, rStepId, rIndex, rFile, rQa).parMapN { (AND, OR, NOT, id, obs, sid, index, file, qa) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            obs,
            sid,
            index,
            file,
            qa
          ).flatten)
        }
    }
  }

}

