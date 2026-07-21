// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.format

import cats.data.NonEmptyList
import io.circe.parser.decode
import io.circe.syntax.*
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.optics.Format

/**
 * The two columns a `SlitTelescopeConfigs` persists
 */
final case class StoredSlitTelescopeConfigs(slitOffsetMode: SlitOffsetMode, telescopeConfigs: String)

/**
 * Serialization layer between `SlitTelescopeConfigs` and the two columns every long-slit table
 * persists it as:
 * - the discriminant tag (`c_slit_offset_mode`) and
 * - the JSON blob (`c_telescope_configs`).
 */
trait TelescopeConfigsFormat:
  import lucuma.odb.json.offset.transport.given
  import lucuma.odb.json.stepconfig.given
  import lucuma.odb.json.telescopeConfigAlongSlit.transport.given

  val AlongSlitFormat: Format[String, NonEmptyList[TelescopeConfigAlongSlit]] =
    Format(
      s => decode[List[TelescopeConfigAlongSlit]](s).toOption.flatMap(NonEmptyList.fromList),
      cs => cs.toList.asJson.noSpaces
    )

  val ToSkyFormat: Format[String, NonEmptyList[TelescopeConfig]] =
    Format(
      s => decode[List[TelescopeConfig]](s).toOption.flatMap(NonEmptyList.fromList),
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

  /** Splits a `SlitTelescopeConfigs` into its two persisted columns. */
  val storedSlitTelescopeConfigs: SlitTelescopeConfigs => StoredSlitTelescopeConfigs =
    stc =>
      val (slitOffsetMode, telescopeConfigs) = SlitTelescopeConfigsFormat.reverseGet(stc)
      StoredSlitTelescopeConfigs(slitOffsetMode, telescopeConfigs)

object telescopeConfigs extends TelescopeConfigsFormat
