// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.apply.*
import lucuma.core.enums.Flamingos2AcquisitionType
import lucuma.core.math.Offset
import lucuma.odb.data.Nullable
import lucuma.odb.sequence.flamingos2.spectroscopy.AcquisitionConfig
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import skunk.*
import skunk.implicits.*

/**
 * Acquisition SQL shared by the Flamingos 2 long slit and MOS mode tables, which
 * name their acquisition columns alike.
 */
object Flamingos2AcquisitionStatements:

  /**
   * Decodes the acquisition exposure time mode columns followed by
   * c_acquisition_filter_default, c_acquisition_filter, c_acquisition_type,
   * c_acquisition_sky_offset_p and c_acquisition_sky_offset_q.
   */
  val acquisition_config: Decoder[AcquisitionConfig] =
    (exposure_time_mode               *:
     flamingos_2_filter               *:
     flamingos_2_filter.opt           *:
     flamingos_2_acquisition_type.opt *:
     angle_µas.opt                    *:
     angle_µas.opt
    ).map: (exposureTimeMode, defaultFilter, explicitFilter, acquisitionType, skyOffsetP, skyOffsetQ) =>
      AcquisitionConfig(
        exposureTimeMode,
        defaultFilter,
        explicitFilter,
        AcquisitionConfig.explicitMode(
          acquisitionType,
          (skyOffsetP, skyOffsetQ).mapN((p, q) => Offset(Offset.P(p), Offset.Q(q)))
        )
      )

  /**
   * The type and its sky offset are written together: input validation guarantees
   * an offset is present exactly when the explicit type is Faint, so changing the
   * type always replaces the offset. An absent type leaves both alone.
   */
  def updates(
    explicitAcquisitionType: Nullable[Flamingos2AcquisitionType],
    skyOffset:               Option[Offset]
  ): List[AppliedFragment] =
    val upType: Fragment[Option[Flamingos2AcquisitionType]] =
      sql"c_acquisition_type = ${flamingos_2_acquisition_type.opt}"
    val upSkyOffsetP = sql"c_acquisition_sky_offset_p = ${angle_µas.opt}"
    val upSkyOffsetQ = sql"c_acquisition_sky_offset_q = ${angle_µas.opt}"

    explicitAcquisitionType.toOptionOption.toList.flatMap: acquisitionType =>
      List(
        upType(acquisitionType),
        upSkyOffsetP(skyOffset.map(_.p.toAngle)),
        upSkyOffsetQ(skyOffset.map(_.q.toAngle))
      )
