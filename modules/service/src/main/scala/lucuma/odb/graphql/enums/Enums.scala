// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.enums

import cats.Monad
import cats.MonadThrow
import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.DirectiveDef
import grackle.EnumType
import grackle.NamedType
import grackle.Schema
import grackle.SchemaExtension
import grackle.TypeExtension
import lucuma.core.enums.Instrument
import lucuma.core.enums.ToOActivation
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import org.tpolecat.sourcepos.SourcePos
import org.typelevel.log4cats.Logger
import skunk.Codec
import skunk.Fragment
import skunk.Session
import skunk.data.Arr
import skunk.data.Type

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
  Enumerated[ProposalStatus]

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
      Enumerated.fromNEL(NonEmptyList.fromListUnsafe(values.toList)).withTag(_.tag)

  }

  enum ProposalStatus(val tag: String, ordinal: Short) {
    private val meta = enumMeta.proposalStatus.getOrElse((tag, ordinal), sys.error(s"t_proposal_status missing tag `$tag`, ordinal `$ordinal`"))

    def name: String = meta.name

    // Note: The order here controls the Order of the Enumerated in the ODB, so keep them in order. 
    case NotSubmitted   extends ProposalStatus("not_submitted", 0)
    case Submitted      extends ProposalStatus("submitted", 1)
    case Accepted       extends ProposalStatus("accepted", 2)
    case NotAccepted    extends ProposalStatus("not_accepted", 3)

    // Used to test that undefined values in the database produce immediate failure on startup.
    // case NotGood     extends ProposalStatus("not_submitted", 1)
    // case UhOh        extends ProposalStatus("uh_oh", 0)
  }

  object ProposalStatus {
    given Enumerated[ProposalStatus] =
      Enumerated.fromNEL(NonEmptyList.fromListUnsafe(values.toList)).withTag(_.tag)
  }

  val schema: Schema =
    new Schema {
      def pos: SourcePos = SourcePos.instance
      def baseTypes: List[NamedType] = 
        Enumerated[ProposalStatus].toEnumType("ProposalStatus", "Enumerated type of ProposalStatus")(_.name) ::
          enumMeta.unreferencedTypes ++ enumMeta.scalaEnumTypes
      def directives: List[DirectiveDef] = Nil
      def schemaExtensions: List[SchemaExtension] = Nil
      def typeExtensions: List[TypeExtension] = Nil
    }

}

object Enums {
  case class ScalaEnum(
    postgresName: String,
    enumType: EnumType
  )

  // enums defined in scala for which we want to validate the postgres enums and add to the schema
  val scalaEnums: List[ScalaEnum] =
    List(
      ScalaEnum("e_too_activation", Enumerated[ToOActivation].toEnumType("ToOActivation", "Target of Opportunity Activation")(_.label))
    )

  case class Meta(
    timeEstimate:      Map[String, TimeEstimateMeta],
    proposalStatus:    Map[(String, Short), ProposalStatusMeta],
    unreferencedTypes: List[EnumType],
    scalaEnumTypes:    List[EnumType]
  )

  def load[F[_]: MonadThrow: Logger](s: Session[F]): F[Enums] =
    for {
      _   <- scalaEnums.traverse(se => validateScalaEnums(s, se))
      te  <- TimeEstimateMeta.select(s)
      ps  <- ProposalStatusMeta.select(s)
      un  <- List(
               // "Unreferenced" types -- those for which we do not need to refer
               // to individual instance in ODB code.
               FilterTypeEnumType.fetch(s),
               ConditionsExpectationTypeEnumType.fetch(s),
               ConditionsMeasurementSourceEnumType.fetch(s),
               SeeingTrendEnumType.fetch(s),
             ).sequence
    } yield Enums(Meta(te, ps, un, scalaEnums.map(_.enumType)))

  def validateScalaEnums[F[_]: MonadThrow: Logger](s: Session[F], scalaEnum: ScalaEnum): F[Unit] =
      val _text_enum: Codec[List[String]] = 
        Codec.array[String](_.toString, _.asRight[String], Type(s"_${scalaEnum.postgresName}", List(Type(scalaEnum.postgresName))))
          .imap(_.toList)(l => Arr.fromFoldable(l))
      // This is probably abusing skunk - is there a better way?
      val query = Fragment(List(Left(s"select enum_range(null::${scalaEnum.postgresName})")), skunk.Void.codec, skunk.util.Origin.unknown)
      val scalaValues = scalaEnum.enumType.enumValues.map(_.name.toLowerCase)
      s.unique(query.query(_text_enum)).flatMap(l =>
        // May need to allow for different ordering and conversions
        if (l === scalaValues) Monad[F].unit
        else MonadThrow[F].raiseError(Exception(s"Enumeration ${scalaEnum.postgresName} with values $l does not match Scala Enumerated with values $scalaValues"))
      )

}
