// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.odb.graphql.binding.*

object WhereDataset {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderDatasetIdBinding = WhereOrder.binding[Dataset.Id](path / "id", DatasetIdBinding)
    val WhereObservationBinding    = WhereObservation.binding(path / "observation")
    val WhereReferenceBinding      = WhereDatasetReference.binding(path / "reference")
    val WhereEqStepIdBinding       = WhereEq.binding[Step.Id](path / "step" / "id", StepIdBinding)
    val WhereOrderIndexBinding     = WhereOrder.binding[PosInt](path / "index", PosIntBinding)
    val WhereFilenameBinding       = WhereString.binding(path / "filename")
    val QaStateBinding             = WhereOptionEq.binding[DatasetQaState](path / "qaState", enumeratedBinding[DatasetQaState])
    val CommentBinding             = WhereOptionString.binding(path / "comment")
    val IsWrittenBinding           = ObjectFieldsBinding.rmap {
      case List(BooleanBinding.Option("EQ", rEQ)) =>
        rEQ.map(_.fold(Predicate.True)(b => IsNull(path / "end", !b)))
    }


    WhereBoolean.binding(path / "isWritten", BooleanBinding)

    lazy val WhereDatasetBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereDatasetBinding.List.Option("AND", rAND),
        WhereDatasetBinding.List.Option("OR", rOR),
        WhereDatasetBinding.Option("NOT", rNOT),

        WhereOrderDatasetIdBinding.Option("id", rId),
        WhereReferenceBinding.Option("reference", rRef),
        WhereObservationBinding.Option("observation", rObs),
        WhereEqStepIdBinding.Option("stepId", rStepId),
        WhereOrderIndexBinding.Option("index", rIndex),
        WhereFilenameBinding.Option("filename", rFile),
        QaStateBinding.Option("qaState", rQa),
        CommentBinding.Option("comment", rComment),
        IsWrittenBinding.Option("isWritten", rIsWritten)
      ) =>
        (rAND, rOR, rNOT, rId, rRef, rObs, rStepId, rIndex, rFile, rQa, rComment, rIsWritten).parMapN {
          (AND, OR, NOT, id, ref, obs, sid, index, file, qa, comment, isWritten) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              ref,
              obs,
              sid,
              index,
              file,
              qa,
              comment,
              isWritten
            ).flatten)
        }
    }
  }

}

