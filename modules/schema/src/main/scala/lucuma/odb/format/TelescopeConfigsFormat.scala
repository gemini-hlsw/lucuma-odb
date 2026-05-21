// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.format

import cats.data.NonEmptyList
import io.circe.syntax.*
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.optics.Format

/**
 * Serialization layer between `SlitTelescopeConfigs` (the lucuma-core domain type) and the two
 * columns used to persist it in the DB: a discriminant tag (e.g. `c_slit_offset_mode`) and a
 * JSON blob (column name varies by instrument, e.g. `c_telescope_configs` for GNIRS).
 */
trait TelescopeConfigsFormat:
  import lucuma.odb.json.offset.transport.given
  import lucuma.odb.json.stepconfig.given
  import lucuma.odb.json.telescopeConfigAlongSlit.transport.given

  val AlongSlitFormat: Format[String, NonEmptyList[TelescopeConfigAlongSlit]] =
    Format(
      s => io.circe.parser.decode[List[TelescopeConfigAlongSlit]](s).toOption.flatMap(NonEmptyList.fromList),
      cs => cs.toList.asJson.noSpaces
    )

  val ToSkyFormat: Format[String, NonEmptyList[TelescopeConfig]] =
    Format(
      s => io.circe.parser.decode[List[TelescopeConfig]](s).toOption.flatMap(NonEmptyList.fromList),
      cs => cs.toList.asJson.noSpaces
    )

  val SlitTelescopeConfigsFormat: Format[(SlitOffsetMode, String), SlitTelescopeConfigs] =
    Format(
      { case (mode, s) =>
        mode match
          case SlitOffsetMode.NodAlongSlit => AlongSlitFormat.getOption(s).map(SlitTelescopeConfigs.AlongSlit(_))
          case SlitOffsetMode.NodToSky     => ToSkyFormat.getOption(s).map(SlitTelescopeConfigs.ToSky(_))
      },
      {
        case SlitTelescopeConfigs.AlongSlit(cs) => (SlitOffsetMode.NodAlongSlit, AlongSlitFormat.reverseGet(cs))
        case SlitTelescopeConfigs.ToSky(cs)     => (SlitOffsetMode.NodToSky,     ToSkyFormat.reverseGet(cs))
      }
    )

object telescopeConfigs extends TelescopeConfigsFormat
