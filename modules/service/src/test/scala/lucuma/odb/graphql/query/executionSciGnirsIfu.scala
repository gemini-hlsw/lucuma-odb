// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import lucuma.core.model.Observation

/**
 * GNIRS IFU science sequence generation.  Regression test for two IFU-specific
 * failures:
 *   - "missing Smart GCAL mapping": an IFU observation must resolve its nighttime
 *     flat/arc smart gcal (keyed by the IFU FPU), seeded by ExecutionTestSupportForGnirs.
 *   - the generated-sequence JSON must carry `fpuIfu` (the GnirsDynamicConfig encoder),
 *     otherwise the Grackle circe cursor falls back to the SQL mapping and the query
 *     fails with "Unhandled mapping of type SqlField for field 'fpuIfu'".  The shared
 *     gnirsScienceQuery selects fpuIfu, so this test exercises that path.
 */
class executionSciGnirsIfu extends ExecutionTestSupportForGnirs:

  private def gnirsIfuObs: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGnirsIfuObservationAs(pi, p, t)
    yield o

  test("[gnirs ifu] science sequence generates, resolves smart gcal, and carries fpuIfu"):
    gnirsIfuObs.flatMap: oid =>
      query(pi, gnirsScienceQuery(oid)).map: js =>
        val steps =
          js.hcursor
            .downField("executionConfig")
            .downField("gnirs")
            .downField("science")
            .downField("nextAtom")
            .downField("steps")
            .as[List[Json]]
            .toOption
            .orEmpty
        // A successful generation yields science steps; a missing smart gcal mapping
        // would instead surface as a GraphQL error (failing `query`).
        assert(steps.nonEmpty, s"expected science steps, got: $js")
        // The science step's FPU must round-trip as fpuIfu (with fpuSlit/fpuOther null).
        val fpuIfus =
          steps.flatMap(_.hcursor.downField("instrumentConfig").downField("fpuIfu").as[String].toOption)
        assert(
          fpuIfus.contains("LOW_RESOLUTION"),
          s"expected a science step with fpuIfu=LOW_RESOLUTION, got: $js"
        )
