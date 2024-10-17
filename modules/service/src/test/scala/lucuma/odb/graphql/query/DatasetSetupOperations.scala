// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step

trait DatasetSetupOperations extends DatabaseOperations { this: OdbSuite =>

  def recordDatasets(
    mode: ObservingModeType,
    user: User,
    service: User,
    offset: Int = 0,
    stepCount: Int = 3,
    datasetsPerStep: Int = 2
  ):IO[(Observation.Id, List[(Step.Id, List[Dataset.Id])])] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(service, mode.instrument, oid)
      aid <- recordAtomAs(service, mode.instrument, vid)
      ids <- (0 until stepCount).toList.traverse { x =>
        recordStepAs(service, mode.instrument, aid).flatMap { sid =>
          (0 until datasetsPerStep).toList.traverse { y =>
            recordDatasetAs(service, sid, f"N18630101S${offset + x * datasetsPerStep + y + 1}%04d.fits")
          }.tupleLeft(sid)
        }
      }
    } yield (oid, ids)

}
