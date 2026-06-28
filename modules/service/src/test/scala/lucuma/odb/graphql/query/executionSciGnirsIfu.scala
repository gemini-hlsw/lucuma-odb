// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import lucuma.core.model.Observation

/**
 * GNIRS IFU science sequence generation.  This is the regression test for the
 * "missing Smart GCAL mapping" failure: an IFU observation must resolve its
 * nighttime flat/arc smart gcal (keyed by the IFU FPU) just like the long slit.
 * The IFU smart gcal rows are seeded by ExecutionTestSupportForGnirs.
 */
class executionSciGnirsIfu extends ExecutionTestSupportForGnirs:

  private def gnirsIfuObs: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGnirsIfuObservationAs(pi, p, t)
    yield o

  test("[gnirs ifu] science sequence generates and resolves smart gcal"):
    gnirsIfuObs.flatMap: oid =>
      query(pi, gnirsScienceQuery(oid)).map: js =>
        val science =
          js.hcursor
            .downField("executionConfig")
            .downField("gnirs")
            .downField("science")
        val nextAtomSteps =
          science.downField("nextAtom").downField("steps").as[List[Json]].toOption.orEmpty
        // A successful generation yields science steps; a missing smart gcal
        // mapping would instead surface as a GraphQL error (failing `query`).
        assert(nextAtomSteps.nonEmpty, s"expected science steps, got: $js")
