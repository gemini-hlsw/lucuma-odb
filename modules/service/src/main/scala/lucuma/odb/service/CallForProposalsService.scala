// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceSubtype
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.CallForProposals
import lucuma.core.model.Semester
import lucuma.odb.data.ExchangePartner
import lucuma.odb.data.Existence
import lucuma.odb.data.KeckInstrument
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.SubaruInstrument
import lucuma.odb.data.SubaruProposalType
import lucuma.odb.graphql.input.CallForProposalsPartnerInput
import lucuma.odb.graphql.input.CallForProposalsPropertiesInput
import lucuma.odb.graphql.input.CoordinateLimitsInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.syntax.scienceSubtype.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.temporal.date
import skunk.implicits.*

import Services.Syntax.*

trait CallForProposalsService[F[_]] {

  def selectProperties(
    cid: CallForProposals.Id
  )(using Transaction[F]): F[Option[CallForProposalsService.CfpProperties]]

  def createCallForProposals(
    input: AccessControl.Checked[CallForProposalsPropertiesInput.Create]
  )(using Transaction[F]): F[Result[CallForProposals.Id]]

  def updateCallsForProposals(
    input: AccessControl.Checked[CallForProposalsPropertiesInput.Edit]
  )(using Transaction[F]): F[Result[List[CallForProposals.Id]]]

}

object CallForProposalsService {

  case class CfpProperties(
    cid:         CallForProposals.Id,
    callType:    CallForProposalsType,
    semester:    Semester,
    proprietary: NonNegInt
  ):
    def validateSubtype(sub: ScienceSubtype): Result[Unit] =
      CfpProperties.mismatchedCfp(cid, callType, sub)
        .asFailure
        .unlessA(sub.isCompatibleWith(callType))

  object CfpProperties:
    def mismatchedCfp(cid:  CallForProposals.Id, cfpType: CallForProposalsType, sub:  ScienceSubtype): OdbError =
      OdbError.InvalidArgument(
        s"The Call for Proposals $cid is a ${cfpType.title} call and cannot be used with a ${sub.title} proposal.".some
      )

  def instantiate[F[_]: Concurrent](using Services[F]): CallForProposalsService[F] =
    new CallForProposalsService[F] {

      override def selectProperties(
        cid: CallForProposals.Id
      )(using Transaction[F]): F[Option[CfpProperties]] =
        session.option(Statements.SelectProperties)(cid)

      def createCallForProposals(
        input: AccessControl.Checked[CallForProposalsPropertiesInput.Create]
      )(using Transaction[F]): F[Result[CallForProposals.Id]] = {
        input.fold(OdbError.InvalidArgument().asFailureF): (SET, _) =>

          val insertCfp: F[Result[CallForProposals.Id]] =
            session.unique(Statements.InsertCallForProposals)(SET)
              .map(_.success)
              .recoverWith:
                case SqlState.CheckViolation(ex) if ex.getMessage.indexOf("d_semester_check") >= 0 =>
                  OdbError.InvalidArgument(s"The maximum semester is capped at the current year +1 (${SET.semester} specified).".some).asFailureF

          // Only Gemini calls list their instruments in `t_cfp_instrument`;
          // the exchange observatories store theirs in array columns directly.
          val instruments: List[Instrument] =
            SET.properties match
              case CallForProposalsPropertiesInput.Create.Properties.Gemini(g) => g.instruments
              case _                                                           => Nil

          case class UsingCid(cid: CallForProposals.Id):
            val cids = List(cid)

            val insertPartnersDefault: F[Unit] =
              session
              .prepareR(Statements.InsertDefaultPartners(cids))
              .use(_.execute(cids))
              .void

            val insertPartners: F[Unit] =
              SET.partners.fold(insertPartnersDefault): partners =>
                session
                  .prepareR(Statements.InsertPartners(cids, partners))
                  .use(_.execute(cids, partners))
                  .whenA(partners.nonEmpty)

            val insertInstruments: F[Unit] =
              session
                .prepareR(Statements.InsertInstruments(cids, instruments))
                .use(_.execute(cids, instruments))
                .whenA(instruments.nonEmpty)

          (for
            cid <- ResultT(insertCfp)
            usingCid = UsingCid(cid)
            _   <- ResultT.liftF(usingCid.insertPartners)
            _   <- ResultT.liftF(usingCid.insertInstruments)
          yield cid).value

      }

      private def updateCfpTable(
        SET:   CallForProposalsPropertiesInput.Edit,
        which: AppliedFragment
      ): F[Result[List[CallForProposals.Id]]] = {
        val af = Statements.UpdateCallsForProposals(SET, which)
        session.prepareR(af.fragment.query(cfp_id)).use { pq =>
          pq.stream(af.argument, chunkSize = 1024)
            .compile
            .toList
            .map(_.success)
            .recover {
              case SqlState.CheckViolation(_)  =>
                OdbError.InvalidArgument("Requested update to the active period is invalid: activeStart must come before activeEnd".some).asFailure
              case SqlState.RaiseException(ex) =>
                OdbError.UpdateFailed(ex.message.some).asFailure
            }
        }
      }

      private def updatePartners(
        cids:     List[CallForProposals.Id],
        partners: Nullable[List[CallForProposalsPartnerInput]]
      ): F[Result[Unit]] = {
        val delete =
          session.executeCommand(Statements.DeletePartners(cids)).void

        def insert(vals: List[CallForProposalsPartnerInput]) =
          session
            .prepareR(Statements.InsertPartners(cids, vals))
            .use(_.execute(cids, vals))
            .whenA(vals.nonEmpty)
            .void

        partners
          .fold(delete, Concurrent[F].unit, is => delete *> insert(is))
          .map(_.success)
      }

      private def updateInstruments(
        cids:        List[CallForProposals.Id],
        instruments: Nullable[List[Instrument]]
      ): F[Result[Unit]] = {
        val delete =
          session.executeCommand(Statements.DeleteInstruments(cids)).void

        def insert(vals: List[Instrument]) =
          session
            .prepareR(Statements.InsertInstruments(cids, vals))
            .use(_.execute(cids, vals))
            .whenA(vals.nonEmpty)
            .void

        instruments
          .fold(delete, Concurrent[F].unit, is => delete *> insert(is))
          .map(_.success)
      }

      // Changing the observatory of an existing CfP would require rewriting all
      // of the observatory-discriminated columns (and re-deriving coordinate
      // limit defaults per row).  That switch is not yet supported, so we reject
      // an edit whose observatory-specific properties target an observatory
      // different from any matched row's existing observatory.
      private def checkObservatory(
        SET:   CallForProposalsPropertiesInput.Edit,
        which: AppliedFragment
      ): F[Result[Unit]] =
        SET.observatory.fold(Result.unit.pure[F]): target =>
          val af = Statements.SelectObservatories(which)
          session.prepareR(af.fragment.query(observatory)).use { pq =>
            pq.stream(af.argument, chunkSize = 1024).compile.toList.map { existing =>
              OdbError.InvalidArgument(
                s"Cannot change the observatory of an existing Call for Proposals.".some
              ).asFailure.whenA(existing.exists(_ != target))
            }
          }

      def updateCallsForProposals(
        input: AccessControl.Checked[CallForProposalsPropertiesInput.Edit]
      )(using Transaction[F]): F[Result[List[CallForProposals.Id]]] =
        input.fold(OdbError.InvalidArgument().asFailureF): (SET, which) =>
          // Gemini instruments live in `t_cfp_instrument`; for the exchange
          // observatories the instrument set is updated in the main table.
          val geminiInstruments: Nullable[List[Instrument]] =
            SET.properties match
              case Some(CallForProposalsPropertiesInput.Edit.Properties.Gemini(g)) => g.instruments
              case _                                                               => Nullable.Absent

          (for {
            _    <- ResultT(checkObservatory(SET, which))
            cids <- ResultT(updateCfpTable(SET, which))
            _    <- ResultT(updatePartners(cids, SET.partners))
            _    <- ResultT(updateInstruments(cids, geminiInstruments))
          } yield cids).value

    }

  object Statements {

    val cfp_properties: Codec[CfpProperties] =
      (cfp_id *: cfp_type *: semester *: int4_nonneg).to[CfpProperties]

    // Exchange calls have no proprietary period; coalesce to 0 so the value
    // stays non-optional (it is unused until a proposal can target an exchange
    // call, which is not yet supported).
    val SelectProperties: Query[CallForProposals.Id, CfpProperties] =
      sql"""
        SELECT c_cfp_id, c_type, c_semester, COALESCE(c_proprietary, 0)
          FROM t_cfp
         WHERE c_cfp_id = $cfp_id AND c_existence = 'present'::e_existence
      """.query(cfp_properties)

    // The distinct observatories of the CfPs matched by the given id selection.
    def SelectObservatories(which: AppliedFragment): AppliedFragment =
      void"SELECT DISTINCT c_observatory FROM t_cfp WHERE c_cfp_id IN (" |+| which |+| void")"

    // The observatory-specific column values derived from a `Create` input.
    // Each variant supplies values for its own columns and leaves the others
    // null, in keeping with the `t_cfp` discriminant check constraints.
    private case class CreateCols(
      cfpType:     CallForProposalsType,
      north:       CoordinateLimitsInput.Create,
      south:       Option[CoordinateLimitsInput.Create],
      proprietary: Option[NonNegInt],
      exchange:    Option[List[ExchangePartner]],
      keck:        Option[List[KeckInstrument]],
      subaru:      Option[List[SubaruInstrument]]
    )

    // The Subaru proposal type is encoded directly in the call type.
    def subaruCallType(t: SubaruProposalType): CallForProposalsType =
      t match
        case SubaruProposalType.Normal    => CallForProposalsType.Subaru
        case SubaruProposalType.Intensive => CallForProposalsType.SubaruIntensive

    private def createCols(p: CallForProposalsPropertiesInput.Create.Properties): CreateCols =
      import CallForProposalsPropertiesInput.Create.Properties
      p match
        case Properties.Gemini(g) =>
          CreateCols(
            cfpType     = g.cfpType,
            north       = g.coordinateLimits.north,
            south       = g.coordinateLimits.south.some,
            proprietary = g.proprietary,
            exchange    = g.exchangePartners.some,
            keck        = none,
            subaru      = none
          )
        case Properties.Keck(k) =>
          CreateCols(
            cfpType     = CallForProposalsType.Keck,
            north       = k.coordinateLimits,
            south       = none,
            proprietary = none,
            exchange    = none,
            keck        = k.instruments.some,
            subaru      = none
          )
        case Properties.Subaru(s) =>
          CreateCols(
            cfpType     = subaruCallType(s.subaruType),
            north       = s.coordinateLimits,
            south       = none,
            proprietary = none,
            exchange    = none,
            keck        = none,
            subaru      = s.instruments.some
          )

    val InsertCallForProposals: Query[CallForProposalsPropertiesInput.Create, CallForProposals.Id] =
      sql"""
        INSERT INTO t_cfp (
          c_observatory,
          c_type,
          c_semester,
          c_title_override,
          c_north_ra_start,
          c_north_ra_end,
          c_north_dec_start,
          c_north_dec_end,
          c_south_ra_start,
          c_south_ra_end,
          c_south_dec_start,
          c_south_dec_end,
          c_deadline_default,
          c_active_start,
          c_active_end,
          c_proprietary,
          c_exchange_partners,
          c_keck_instruments,
          c_subaru_instruments,
          c_existence
        )
        SELECT
          $observatory,
          $cfp_type,
          $semester,
          ${text_nonempty.opt},
          ${right_ascension},
          ${right_ascension},
          ${declination},
          ${declination},
          ${right_ascension.opt},
          ${right_ascension.opt},
          ${declination.opt},
          ${declination.opt},
          ${core_timestamp.opt},
          $date,
          $date,
          COALESCE(${int4_nonneg.opt}, (SELECT c_proprietary FROM t_cfp_type WHERE c_type = $cfp_type)),
          ${_exchange_partner.opt},
          ${_keck_instrument.opt},
          ${_subaru_instrument.opt},
          $existence
        RETURNING
          c_cfp_id
      """.query(cfp_id).contramap { input =>
        val c = createCols(input.properties)
        (
          input.observatory,
          c.cfpType,
          input.semester,
          input.title,
          c.north.raStart,
          c.north.raEnd,
          c.north.decStart,
          c.north.decEnd,
          c.south.map(_.raStart),
          c.south.map(_.raEnd),
          c.south.map(_.decStart),
          c.south.map(_.decEnd),
          input.deadline,
          input.active.start,
          input.active.end,
          c.proprietary,
          c.cfpType,           // for the proprietary-default subquery
          c.exchange,
          c.keck,
          c.subaru,
          input.existence
        )
      }

    def InsertDefaultPartners(
      cids: List[CallForProposals.Id]
    ): Command[cids.type] =
      sql"""
        INSERT INTO t_cfp_partner (
          c_cfp_id,
          c_partner
        )
        SELECT
            cfps.id,
            p.c_tag
        FROM t_partner p CROSS JOIN (VALUES ${cfp_id.values.list(cids.length)}) AS cfps(id)
      """.command.contramap(_ => cids)

    def InsertPartners(
      cids:     List[CallForProposals.Id],
      partners: List[CallForProposalsPartnerInput]
    ): Command[(cids.type, partners.type)] =
      sql"""
        INSERT INTO t_cfp_partner (
          c_cfp_id,
          c_partner,
          c_deadline_override
        ) VALUES ${(
          cfp_id  *:
          partner *:
          core_timestamp.opt
        ).values.list(cids.length * partners.length)}
      """.command
         .contramap {
           case (cids, partners) => cids.flatMap { cid =>
             partners.map { p => (cid, p.partner, p.deadline) }
           }
         }

    def DeletePartners(cids: List[CallForProposals.Id]): AppliedFragment =
      sql"""
        DELETE FROM t_cfp_partner
          WHERE c_cfp_id IN ${cfp_id.list(cids.length).values}
      """.apply(cids)

    def InsertInstruments(
      cids:        List[CallForProposals.Id],
      instruments: List[Instrument]
    ): Command[(cids.type, instruments.type)] =
      sql"""
        INSERT INTO t_cfp_instrument (
          c_cfp_id,
          c_instrument
        ) VALUES ${(cfp_id *: instrument).values.list(cids.length * instruments.length)}
      """.command
         .contramap {
           case (cids, instruments) => cids.flatMap(instruments.tupleLeft(_))
         }

    def DeleteInstruments(cids: List[CallForProposals.Id]): AppliedFragment =
      sql"""
        DELETE FROM t_cfp_instrument
          WHERE c_cfp_id IN ${cfp_id.list(cids.length).values}
      """.apply(cids)

    def UpdateCallsForProposals(
      SET:   CallForProposalsPropertiesInput.Edit,
      which: AppliedFragment
    ): AppliedFragment = {
      val gnRaStart   = sql"c_north_ra_start  = $right_ascension"
      val gnRaEnd     = sql"c_north_ra_end    = $right_ascension"
      val gnDecStart  = sql"c_north_dec_start = $declination"
      val gnDecEnd    = sql"c_north_dec_end   = $declination"
      val gsRaStart   = sql"c_south_ra_start  = $right_ascension"
      val gsRaEnd     = sql"c_south_ra_end    = $right_ascension"
      val gsDecStart  = sql"c_south_dec_start = $declination"
      val gsDecEnd    = sql"c_south_dec_end   = $declination"
      val activeStart = sql"c_active_start    = $date"
      val activeEnd   = sql"c_active_end      = $date"

      val upExistence    = sql"c_existence            = $existence"
      val upSemester     = sql"c_semester             = $semester"
      val upType         = sql"c_type                 = $cfp_type"
      val upProprietary  = sql"c_proprietary          = $int4_nonneg"
      val upExchange     = sql"c_exchange_partners    = ${_exchange_partner}"
      val upKeckInstr    = sql"c_keck_instruments     = ${_keck_instrument}"
      val upSubaruInstr  = sql"c_subaru_instruments   = ${_subaru_instrument}"

      // Observatory-specific assignments.  Only the columns belonging to the
      // edited observatory are touched; supplying properties for the "wrong"
      // observatory would be rejected by the table's check constraints.
      val propUps: List[Option[AppliedFragment]] =
        SET.properties.toList.flatMap {
          case CallForProposalsPropertiesInput.Edit.Properties.Gemini(g) =>
            val north = g.coordinateLimits.flatMap(_.north)
            val south = g.coordinateLimits.flatMap(_.south)
            List(
              g.cfpType.map(upType),
              g.proprietary.map(upProprietary),
              // The Gemini discriminant requires a non-null array, so a null
              // reset is stored as the empty array.
              g.exchangePartners.fold(List.empty[ExchangePartner].some, none, _.some).map(upExchange),
              north.flatMap(_.raStart).map(gnRaStart),
              north.flatMap(_.raEnd).map(gnRaEnd),
              north.flatMap(_.decStart).map(gnDecStart),
              north.flatMap(_.decEnd).map(gnDecEnd),
              south.flatMap(_.raStart).map(gsRaStart),
              south.flatMap(_.raEnd).map(gsRaEnd),
              south.flatMap(_.decStart).map(gsDecStart),
              south.flatMap(_.decEnd).map(gsDecEnd)
            )

          case CallForProposalsPropertiesInput.Edit.Properties.Keck(k) =>
            val limits = k.coordinateLimits
            List(
              // Exchange instrument arrays are never null on their own row, so
              // a null reset becomes the empty array.
              k.instruments.fold(List.empty[KeckInstrument].some, none, _.some).map(upKeckInstr),
              limits.flatMap(_.raStart).map(gnRaStart),
              limits.flatMap(_.raEnd).map(gnRaEnd),
              limits.flatMap(_.decStart).map(gnDecStart),
              limits.flatMap(_.decEnd).map(gnDecEnd)
            )

          case CallForProposalsPropertiesInput.Edit.Properties.Subaru(s) =>
            val limits = s.coordinateLimits
            List(
              // The Subaru proposal type is encoded in the call type.
              s.subaruType.map(t => upType(subaruCallType(t))),
              s.instruments.fold(List.empty[SubaruInstrument].some, none, _.some).map(upSubaruInstr),
              limits.flatMap(_.raStart).map(gnRaStart),
              limits.flatMap(_.raEnd).map(gnRaEnd),
              limits.flatMap(_.decStart).map(gnDecStart),
              limits.flatMap(_.decEnd).map(gnDecEnd)
            )
        }

      val ups: Option[NonEmptyList[AppliedFragment]] =
        NonEmptyList.fromList(
          List(
            SET.title.foldPresent(sql"c_title_override = ${text_nonempty.opt}"),
            SET.deadline.foldPresent(sql"c_deadline_default = ${core_timestamp.opt}"),
            SET.active.flatMap(_.left).map(activeStart),
            SET.active.flatMap(_.right).map(activeEnd),
            SET.existence.map(upExistence),
            SET.semester.map(upSemester)
          ).flatten ++ propUps.flatten
        )

      def update(us: NonEmptyList[AppliedFragment]): AppliedFragment =
        void"UPDATE t_cfp "                                      |+|
          void"SET " |+| us.intercalate(void", ") |+| void" "    |+|
          void"WHERE t_cfp.c_cfp_id IN (" |+| which |+| void") " |+|
          void"RETURNING t_cfp.c_cfp_id"

       def selectOnly: AppliedFragment =
         void"SELECT c.c_cfp_id " |+|
           void"FROM t_cfp c "    |+|
           void"WHERE c.c_cfp_id IN (" |+| which |+| void")"

      ups.fold(selectOnly)(update)
    }
  }
}
