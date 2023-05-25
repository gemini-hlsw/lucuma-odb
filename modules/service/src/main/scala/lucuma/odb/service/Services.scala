// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.model.User
import skunk.Session
import cats.effect.kernel.Sync
import skunk.Transaction
import natchez.Trace

/** A collection of services, all bound to a single Session and User. */
trait Services[F[_]]:
  def session: Session[F]
  def user: User
  def allocationService: AllocationService[F]
  def asterismService: AsterismService[F]
  // def generatorParamsService: GeneratorParamsService[F]
  // def gmosLongSlitService: GmosLongSlitService[F]
  // def groupService: GroupService[F]
  // def obsAttachmentFileService: ObsAttachmentFileService[F]
  // def obsAttachmentMetadataService: ObsAttachmentMetadataService[F]
  def observationService: ObservationService[F]
  def observingModeServices: ObservingModeServices[F]
  // def partnerSplitsService: PartnerSplitsService[F]
  // def programService: ProgramService[F]
  // def proposalAttachmentFileService: ProposalAttachmentFileService[F]
  // def proposalAttachmentMetadataService: ProposalAttachmentMetadataService[F]
  // def proposalService: ProposalService[F]
  // def s3FileService: S3FileService[F]
  // def smartGcalService: SmartGcalService[F]
  // def targetService: TargetService[F]
  def timingWindowService: TimingWindowService[F]
  // def userService: UserService[F]

object Services:
  
  def forUser[F[_]: Sync: Trace](u: User)(s: Session[F]): Services[F[_]] =
    new Services[F]:

      val user = u
      val session = s

      private given Services[F] = this

      lazy val allocationService = AllocationService.instantiate
      lazy val asterismService = AsterismService.instantiate
      // lazy val generatorParamsService = GeneratorParamsService.instantiate
      // lazy val gmosLongSlitService = GmosLongSlitService.instantiate
      // lazy val groupService = GroupService.instantiate
      // lazy val obsAttachmentFileService = ObsAttachmentFileService.instantiate
      // lazy val obsAttachmentMetadataService = ObsAttachmentMetadataService.instantiate
      lazy val observationService = ObservationService.instantiate
      lazy val observingModeServices = ObservingModeServices.instantiate
      // lazy val partnerSplitsService = PartnerSplitsService.instantiate
      // lazy val programService = ProgramService.instantiate
      // lazy val proposalAttachmentFileService = ProposalAttachmentFileService.instantiate
      // lazy val proposalAttachmentMetadataService = ProposalAttachmentMetadataService.instantiate
      // lazy val proposalService = ProposalService.instantiate
      // lazy val s3FileService = S3FileService.instantiate
      // lazy val smartGcalService = SmartGcalService.instantiate
      // lazy val targetService = ???
      lazy val timingWindowService = TimingWindowService.instantiate
      // lazy val userService = UserService.instantiate

  object Syntax:
    def transaction[F[_]](using Transaction[F]): Transaction[F] = summon
    def session[F[_]](using Services[F]): Session[F] = summon[Services[F]].session
    def user[F[_]](using Services[F]): User = summon[Services[F]].user
    def allocationService[F[_]](using Services[F]): AllocationService[F] = summon[Services[F]].allocationService
    def asterismService[F[_]](using Services[F]): AsterismService[F] = summon[Services[F]].asterismService
    // def generatorParamsService[F[_]](using Services[F]): GeneratorParamsService[F] = summon[Services[F]].generatorParamsService
    // def gmosLongSlitService[F[_]](using Services[F]): GmosLongSlitService[F] = summon[Services[F]].gmosLongSlitService
    // def groupService[F[_]](using Services[F]): GroupService[F] = summon[Services[F]].groupService
    // def obsAttachmentFileService[F[_]](using Services[F]): ObsAttachmentFileService[F] = summon[Services[F]].obsAttachmentFileService
    // def obsAttachmentMetadataService[F[_]](using Services[F]): ObsAttachmentMetadataService[F] = summon[Services[F]].obsAttachmentMetadataService
    def observationService[F[_]](using Services[F]): ObservationService[F] = summon[Services[F]].observationService
    def observingModeServices[F[_]](using Services[F]): ObservingModeServices[F] = summon[Services[F]].observingModeServices
    // def partnerSplitsService[F[_]](using Services[F]): PartnerSplitsService[F] = summon[Services[F]].partnerSplitsService
    // def programService[F[_]](using Services[F]): ProgramService[F] = summon[Services[F]].programService
    // def proposalAttachmentFileService[F[_]](using Services[F]): ProposalAttachmentFileService[F] = summon[Services[F]].proposalAttachmentFileService
    // def proposalAttachmentMetadataService[F[_]](using Services[F]): ProposalAttachmentMetadataService[F] = summon[Services[F]].proposalAttachmentMetadataService
    // def proposalService[F[_]](using Services[F]): ProposalService[F] = summon[Services[F]].proposalService
    // def s3FileService[F[_]](using Services[F]): S3FileService[F] = summon[Services[F]].s3FileService
    // def smartGcalService[F[_]](using Services[F]): SmartGcalService[F] = summon[Services[F]].smartGcalService
    // def targetService[F[_]](using Services[F]): TargetService[F] = summon[Services[F]].targetService
    def timingWindowService[F[_]](using Services[F]): TimingWindowService[F] = summon[Services[F]].timingWindowService
    // def userService[F[_]](using Services[F]): UserService[F] = summon[Services[F]].userService