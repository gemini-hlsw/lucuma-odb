// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import lucuma.core.model.Observation
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.model.sequence.gmos.longslit.DefaultSlitTelescopeConfigs
import lucuma.core.model.sequence.gmos.mos.DefaultTelescopeConfigs
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.slit_offset_mode
import skunk.Query
import skunk.codec.text.text
import skunk.syntax.all.*

/**
 * The GMOS views hold a literal copy of lucuma-core's defaults, so a core change would
 * otherwise drift from the database silently.
 */
class gmosTelescopeConfigsDefaults extends OdbSuite with ObservingModeSetupOperations:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers: List[User] = List(pi)

  private def readLongSlitDefault(oid: Observation.Id): IO[SlitTelescopeConfigs] =
    val q: Query[Observation.Id, (lucuma.core.enums.SlitOffsetMode, String)] =
      sql"""
        SELECT c_slit_offset_mode_default, c_telescope_configs_default
        FROM v_gmos_north_long_slit
        WHERE c_observation_id = $observation_id
      """.query(slit_offset_mode *: text)
    withSession(_.unique(q)(oid)).map: stored =>
      SlitTelescopeConfigsFormat.getOption(stored).getOrElse(sys.error(s"Could not parse '$stored'."))

  private def readMosDefault(oid: Observation.Id): IO[String] =
    val q: Query[Observation.Id, String] =
      sql"""
        SELECT c_telescope_configs_default
        FROM v_gmos_north_mos
        WHERE c_observation_id = $observation_id
      """.query(text)
    withSession(_.unique(q)(oid))

  test("the stored long slit default matches lucuma-core's nod along slit"):
    for
      pid    <- createProgramAs(pi)
      tid    <- createTargetAs(pi, pid, "Biff")
      oid    <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      stored <- readLongSlitDefault(oid)
      _      <- IO(assertEquals(stored, DefaultSlitTelescopeConfigs))
    yield ()

  test("the stored MOS default matches lucuma-core's single guided position"):
    for
      pid    <- createProgramAs(pi)
      tid    <- createTargetAs(pi, pid, "Biff")
      oid    <- createGmosNorthMosObservationAs(pi, pid, List(tid))
      stored <- readMosDefault(oid)
      _      <- IO(assertEquals(ToSkyFormat.getOption(stored), Some(DefaultTelescopeConfigs)))
    yield ()
