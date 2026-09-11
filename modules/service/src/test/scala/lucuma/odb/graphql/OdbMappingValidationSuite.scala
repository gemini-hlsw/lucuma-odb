// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import lucuma.core.model.User
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

/**
 * Validates the OdbMapping type mappings once. Every other suite builds its mappings unchecked,
 * so this is the only place a stale or unused mapping is caught.
 */
class OdbMappingValidationSuite extends OdbSuite:

  val validUsers: List[User] = Nil

  test("type mappings validate without failures"):
    given Tracer[IO]         = Tracer.noop
    given TracerProvider[IO] = TracerProvider.noop
    given MeterProvider[IO]  = MeterProvider.noop
    mapping.use(OdbMapping.validate[IO])
