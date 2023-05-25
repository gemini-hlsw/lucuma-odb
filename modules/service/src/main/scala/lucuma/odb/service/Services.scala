// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.model.User
import skunk.Session
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import skunk.Transaction
import natchez.Trace
import cats.effect.Resource
import cats.effect.MonadCancelThrow
import lucuma.odb.logic.Itc
import lucuma.itc.client.ItcClient
import lucuma.odb.logic.Generator
import lucuma.odb.sequence.util.CommitHash

/** A collection of services, all bound to a single Session and User. */
trait Services[F[_]]:
  def session: Session[F]
  def user: User
  def transactionally[A](f: (Transaction[F]) ?=> F[A]): F[A]
  def allocationService: AllocationService[F]
  def asterismService: AsterismService[F]
  def generatorParamsService: GeneratorParamsService[F]
  def gmosLongSlitService: GmosLongSlitService[F]
  def groupService: GroupService[F]
  def obsAttachmentFileService(s3: S3FileService[F]): ObsAttachmentFileService[F]
  def obsAttachmentMetadataService: ObsAttachmentMetadataService[F]
  def observationService: ObservationService[F]
  def observingModeServices: ObservingModeServices[F]
  def partnerSplitsService: PartnerSplitsService[F]
  def programService: ProgramService[F]
  def proposalAttachmentFileService(s3: S3FileService[F]): ProposalAttachmentFileService[F]
  def proposalAttachmentMetadataService: ProposalAttachmentMetadataService[F]
  def proposalService: ProposalService[F]
  def smartGcalService: SmartGcalService[F]
  def targetService: TargetService[F]
  def timingWindowService: TimingWindowService[F]
  def itc(itcClient: ItcClient[F]): Itc[F]
  def generator(commitHash: CommitHash, itcClient: ItcClient[F]): Generator[F]

object Services:
  
  def forUser[F[_]: Concurrent: Trace: UUIDGen](u: User)(s: Session[F]): Services[F[_]] =
    new Services[F]:

      val user = u
      val session = s

      def transactionally[A](f: Transaction[F] ?=> F[A]): F[A] =
        session.transaction.use(xa => f(using xa))

      private given Services[F] = this

      lazy val allocationService = AllocationService.instantiate
      lazy val asterismService = AsterismService.instantiate
      lazy val generatorParamsService = GeneratorParamsService.instantiate
      lazy val gmosLongSlitService = GmosLongSlitService.instantiate
      lazy val groupService = GroupService.instantiate
      def obsAttachmentFileService(s3: S3FileService[F]) = ObsAttachmentFileService.instantiate(s3)
      lazy val obsAttachmentMetadataService = ObsAttachmentMetadataService.instantiate
      lazy val observationService = ObservationService.instantiate
      lazy val observingModeServices = ObservingModeServices.instantiate
      lazy val partnerSplitsService = PartnerSplitsService.instantiate
      lazy val programService = ProgramService.instantiate
      def proposalAttachmentFileService(s3: S3FileService[F]) = ProposalAttachmentFileService.instantiate(s3)
      lazy val proposalAttachmentMetadataService = ProposalAttachmentMetadataService.instantiate
      lazy val proposalService = ProposalService.instantiate
      lazy val smartGcalService = SmartGcalService.instantiate
      lazy val targetService = TargetService.instantiate
      lazy val timingWindowService = TimingWindowService.instantiate
      def itc(itcClient: ItcClient[F]) = Itc.instantiate(itcClient)
      def generator(commitHash: CommitHash, itcClient: ItcClient[F]) = Generator.instantiate(commitHash, itcClient)

  object Syntax:
    def transaction[F[_]](using Transaction[F]): Transaction[F] = summon
    def session[F[_]](using Services[F]): Session[F] = summon[Services[F]].session
    def user[F[_]](using Services[F]): User = summon[Services[F]].user
    def allocationService[F[_]](using Services[F]): AllocationService[F] = summon[Services[F]].allocationService
    def asterismService[F[_]](using Services[F]): AsterismService[F] = summon[Services[F]].asterismService
    def generatorParamsService[F[_]](using Services[F]): GeneratorParamsService[F] = summon[Services[F]].generatorParamsService
    def gmosLongSlitService[F[_]](using Services[F]): GmosLongSlitService[F] = summon[Services[F]].gmosLongSlitService
    def groupService[F[_]](using Services[F]): GroupService[F] = summon[Services[F]].groupService
    def obsAttachmentFileService[F[_]](s3: S3FileService[F])(using Services[F]): ObsAttachmentFileService[F] = summon[Services[F]].obsAttachmentFileService(s3)
    def obsAttachmentMetadataService[F[_]](using Services[F]): ObsAttachmentMetadataService[F] = summon[Services[F]].obsAttachmentMetadataService
    def observationService[F[_]](using Services[F]): ObservationService[F] = summon[Services[F]].observationService
    def observingModeServices[F[_]](using Services[F]): ObservingModeServices[F] = summon[Services[F]].observingModeServices
    def partnerSplitsService[F[_]](using Services[F]): PartnerSplitsService[F] = summon[Services[F]].partnerSplitsService
    def programService[F[_]](using Services[F]): ProgramService[F] = summon[Services[F]].programService
    def proposalAttachmentFileService[F[_]](s3: S3FileService[F])(using Services[F]): ProposalAttachmentFileService[F] = summon[Services[F]].proposalAttachmentFileService(s3)
    def proposalAttachmentMetadataService[F[_]](using Services[F]): ProposalAttachmentMetadataService[F] = summon[Services[F]].proposalAttachmentMetadataService
    def proposalService[F[_]](using Services[F]): ProposalService[F] = summon[Services[F]].proposalService
    def smartGcalService[F[_]](using Services[F]): SmartGcalService[F] = summon[Services[F]].smartGcalService
    def targetService[F[_]](using Services[F]): TargetService[F] = summon[Services[F]].targetService
    def timingWindowService[F[_]](using Services[F]): TimingWindowService[F] = summon[Services[F]].timingWindowService
    def itc[F[_]](client: ItcClient[F])(using Services[F]): Itc[F] = summon[Services[F]].itc(client)
    def generator[F[_]](commitHash: CommitHash, itcClient: ItcClient[F])(using Services[F]): Generator[F] = summon[Services[F]].generator(commitHash, itcClient)

    extension [F[_]: MonadCancelThrow, A](s: Resource[F, Services[F]]) def useTransactionally(fa: (Transaction[F], Services[F]) ?=> F[A]): F[A] =
      s.use(ss => ss.session.transaction.use(xa => fa(using xa, ss)))
