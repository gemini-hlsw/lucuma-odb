// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.enums

import cats.Functor
import cats.data.NonEmptyList
import cats.syntax.functor.*
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import skunk.Session

/**
 * Enums whose values are loaded from the database on startup.  Ordinary enums
 * are static: a Scala enum in lucuma-core, a GraphQL enum in `OdbSchema.graphql`
 * and a database lookup table, kept in sync and verified by `StartupDiagnostics`.
 * The enums here are the exception -- their members are known at compile time but
 * their associated metadata lives in the database.
 *
 * @param enumMeta metadata for the enums defined here
 */
final class Enums(
  enumMeta: Enums.Meta
) {

  // The enums defined in this class appear to be lazily constructed.  Since they
  // reference metadata which comes from the database, we want them to fail
  // immediately if the associated meta data was not found.  For this reason they
  // must be explicitly "used" in the construction of `Enums`. Add any new enums
  // to the list below.
  Enumerated[TimeEstimate]

  /**
   * Time estimates for config changes, etc.  Because this is internal to the
   * ODB, it is not added to the schema.
   */
  enum TimeEstimate(val tag: String) {
    private val meta = enumMeta.timeEstimate.getOrElse(tag, sys.error(s"t_time_estimate missing tag `$tag`"))

    def name: String                   = meta.name
    def description: String            = meta.description
    def instrument: Option[Instrument] = meta.instrument
    def time: TimeSpan                 = meta.time

    case Flamingos2BrightReadout    extends TimeEstimate("f2_bright_readout")
    case Flamingos2Disperser        extends TimeEstimate("f2_disperser")
    case Flamingos2FaintReadout     extends TimeEstimate("f2_faint_readout")
    case Flamingos2Filter           extends TimeEstimate("f2_filter")
    case Flamingos2Fpu              extends TimeEstimate("f2_fpu")
    case Flamingos2LongslitSetup    extends TimeEstimate("f2_longslit_setup")
    case Flamingos2LongslitMaxVisit extends TimeEstimate("f2_longslit_max_visit")
    case Flamingos2MediumReadout    extends TimeEstimate("f2_medium_readout")
    case Flamingos2Reacquisition    extends TimeEstimate("f2_reacquisition")
    case Flamingos2Write            extends TimeEstimate("f2_write")

    case GcalDiffuser               extends TimeEstimate("gcal_diffuser")
    case GcalFilter                 extends TimeEstimate("gcal_filter")
    case GcalShutter                extends TimeEstimate("gcal_shutter")

    case GhostIfuMaxVisit           extends TimeEstimate("ghost_ifu_max_visit")
    case GhostIfuSetup              extends TimeEstimate("ghost_ifu_setup")
    case GhostReacquisition         extends TimeEstimate("ghost_reacquisition")
    case GhostWrite                 extends TimeEstimate("ghost_write")

    case GmosNorthDisperser         extends TimeEstimate("gmos_north_disperser")
    case GmosNorthFilter            extends TimeEstimate("gmos_north_filter")
    case GmosNorthFpu               extends TimeEstimate("gmos_north_fpu")
    case GmosNorthImagingMaxVisit   extends TimeEstimate("gmos_north_imaging_max_visit")
    case GmosNorthImagingSetup      extends TimeEstimate("gmos_north_imaging_setup")
    case GmosNorthLongslitMaxVisit  extends TimeEstimate("gmos_north_longslit_max_visit")
    case GmosNorthLongslitSetup     extends TimeEstimate("gmos_north_longslit_setup")
    case GmosNorthMosMaxVisit       extends TimeEstimate("gmos_north_mos_max_visit")
    case GmosNorthMosSetup          extends TimeEstimate("gmos_north_mos_setup")
    case GmosNorthNod               extends TimeEstimate("gmos_north_nod")
    case GmosNorthNodEOffset        extends TimeEstimate("gmos_north_nod_e_offset")
    case GmosNorthReacquisition     extends TimeEstimate("gmos_north_reacquisition")
    case GmosNorthWrite             extends TimeEstimate("gmos_north_write")

    case GmosSouthDisperser         extends TimeEstimate("gmos_south_disperser")
    case GmosSouthFilter            extends TimeEstimate("gmos_south_filter")
    case GmosSouthFpu               extends TimeEstimate("gmos_south_fpu")
    case GmosSouthImagingMaxVisit   extends TimeEstimate("gmos_south_imaging_max_visit")
    case GmosSouthImagingSetup      extends TimeEstimate("gmos_south_imaging_setup")
    case GmosSouthLongslitMaxVisit  extends TimeEstimate("gmos_south_longslit_max_visit")
    case GmosSouthLongslitSetup     extends TimeEstimate("gmos_south_longslit_setup")
    case GmosSouthMosMaxVisit       extends TimeEstimate("gmos_south_mos_max_visit")
    case GmosSouthMosSetup          extends TimeEstimate("gmos_south_mos_setup")
    case GmosSouthNod               extends TimeEstimate("gmos_south_nod")
    case GmosSouthNodEOffset        extends TimeEstimate("gmos_south_nod_e_offset")
    case GmosSouthReacquisition     extends TimeEstimate("gmos_south_reacquisition")
    case GmosSouthWrite             extends TimeEstimate("gmos_south_write")

    case GnirsLongslitMaxVisit      extends TimeEstimate("gnirs_longslit_max_visit")
    case GnirsLongslitSetup         extends TimeEstimate("gnirs_longslit_setup")
    case GnirsReacquisition         extends TimeEstimate("gnirs_reacquisition")
    case GnirsWavelength            extends TimeEstimate("gnirs_wavelength")
    case GnirsWrite                 extends TimeEstimate("gnirs_write")

    case Igrins2LongslitMaxVisit    extends TimeEstimate("igrins2_longslit_max_visit")
    case Igrins2LongslitSetup       extends TimeEstimate("igrins2_longslit_setup")
    case Igrins2Reacquisition       extends TimeEstimate("igrins2_reacquisition")
    case Igrins2Write               extends TimeEstimate("igrins2_write")

    case OffsetConstant             extends TimeEstimate("offset_constant")
    case OffsetDistance             extends TimeEstimate("offset_distance")
    case ScienceFold                extends TimeEstimate("science_fold")

    // For resident visitors
    case AlopekeSpeckleSetup        extends TimeEstimate("alopeke_speckle_setup")
    case AlopekeSpeckleReadout      extends TimeEstimate("alopeke_speckle_readout")
    case AlopekeWideFieldSetup      extends TimeEstimate("alopeke_wide_field_setup")
    case AlopekeWideFieldReadout    extends TimeEstimate("alopeke_wide_field_readout")
    case ZorroSpeckleSetup          extends TimeEstimate("zorro_speckle_setup")
    case ZorroSpeckleReadout        extends TimeEstimate("zorro_speckle_readout")
    case ZorroWideFieldSetup        extends TimeEstimate("zorro_wide_field_setup")
    case ZorroWideFieldReadout      extends TimeEstimate("zorro_wide_field_readout")
    case MaroonXSetup               extends TimeEstimate("maroon_x_setup")
    case MaroonXReadout             extends TimeEstimate("maroon_x_readout")

    // Used to test that undefined values in the database produce immediate failure on startup.
    // case FooBar              extends TimeEstimate("foo_bar")
  }

  object TimeEstimate {

    given Enumerated[TimeEstimate] =
      Enumerated.fromNEL(NonEmptyList.fromListUnsafe(values.toList)).withTag(_.tag)

  }

}

object Enums {

  case class Meta(
    timeEstimate: Map[String, TimeEstimateMeta]
  )

  def load[F[_]: Functor](s: Session[F]): F[Enums] =
    TimeEstimateMeta.select(s).map(te => Enums(Meta(te)))

}
