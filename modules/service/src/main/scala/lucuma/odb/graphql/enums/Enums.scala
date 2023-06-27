// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.enums

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import edu.gemini.grackle.Directive
import edu.gemini.grackle.EnumType
import edu.gemini.grackle.NamedType
import edu.gemini.grackle.Schema
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import org.tpolecat.sourcepos.SourcePos
import org.typelevel.log4cats.Logger
import skunk.Session

/**
 * Enums loaded from the database on startup.  These fall into two categories:
 * "referenced" enums, those for which we need to reference individual values
 * in code and "unreferenced" enums, those which are simply a listing of
 * elements to add to the schema.
 *
 * @param enumMeta metadata for referenced enums
 */
final class Enums(
  enumMeta: Enums.Meta
) {

  // The referenced enums defined in this class appear to be lazily constructed.
  // Since they reference metadata which comes from the database, we want them
  // to fail immediately if the associated meta data was not found.  For this
  // reason they must be explicitly "used" in the construction of `Enums`. Add
  // any new enums to the list below.
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

    case GcalDiffuser           extends TimeEstimate("gcal_diffuser")
    case GcalFilter             extends TimeEstimate("gcal_filter")
    case GcalShutter            extends TimeEstimate("gcal_shutter")
    case GmosNorthDisperser     extends TimeEstimate("gmos_north_disperser")
    case GmosNorthFilter        extends TimeEstimate("gmos_north_filter")
    case GmosNorthFpu           extends TimeEstimate("gmos_north_fpu")
    case GmosNorthLongslitSetup extends TimeEstimate("gmos_north_longslit_setup")
    case GmosNorthNod           extends TimeEstimate("gmos_north_nod")
    case GmosNorthNodEOffset    extends TimeEstimate("gmos_north_nod_e_offset")
    case GmosNorthReacquisition extends TimeEstimate("gmos_north_reacquisition")
    case GmosNorthWrite         extends TimeEstimate("gmos_north_write")
    case GmosSouthDisperser     extends TimeEstimate("gmos_south_disperser")
    case GmosSouthFilter        extends TimeEstimate("gmos_south_filter")
    case GmosSouthFpu           extends TimeEstimate("gmos_south_fpu")
    case GmosSouthLongslitSetup extends TimeEstimate("gmos_south_longslit_setup")
    case GmosSouthNod           extends TimeEstimate("gmos_south_nod")
    case GmosSouthNodEOffset    extends TimeEstimate("gmos_south_nod_e_offset")
    case GmosSouthReacquisition extends TimeEstimate("gmos_south_reacquisition")
    case GmosSouthWrite         extends TimeEstimate("gmos_south_write")
    case OffsetConstant         extends TimeEstimate("offset_constant")
    case OffsetDistance         extends TimeEstimate("offset_distance")
    case ScienceFold            extends TimeEstimate("science_fold")

    // Used to test that undefined values in the database produce immediate failure on startup.
    // case FooBar              extends TimeEstimate("foo_bar")
  }

  object TimeEstimate {

    given Enumerated[TimeEstimate] =
      Enumerated.from(values.head, values.tail*).withTag(_.tag)

  }

  val schema: Schema =
    new Schema {
      def pos:        SourcePos       = SourcePos.instance
      def types:      List[NamedType] = enumMeta.unreferencedTypes
      def directives: List[Directive] = Nil
    }

}

object Enums {

  case class Meta(
    timeEstimate:      Map[String, TimeEstimateMeta],
    unreferencedTypes: List[EnumType]
  )

  def load[F[_]: Monad: Logger](s: Session[F]): F[Enums] =
    for {
      te  <- TimeEstimateMeta.select(s)
      un  <- List(
               // "Unreferenced" types -- those for which we do not need to refer
               // to individual instance in ODB code.
               ObsAttachmentTypeEnumType.fetch(s),
               ProposalAttachmentTypeEnumType.fetch(s),
               FilterTypeEnumType.fetch(s),
               PartnerEnumType.fetch(s),
               ConditionsExpectationTypeEnumType.fetch(s),
               ConditionsMeasurementSourceEnumType.fetch(s),
               SeeingTrendEnumType.fetch(s),
             ).sequence
    } yield Enums(Meta(te, un))

}
