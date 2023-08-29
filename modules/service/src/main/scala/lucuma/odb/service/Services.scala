// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.itc.client.ItcClient
import lucuma.odb.logic.Generator
import lucuma.odb.logic.PlannedTimeCalculator
import lucuma.odb.logic.PlannedTimeRangeService
import lucuma.odb.sequence.util.CommitHash
import natchez.Trace
import org.http4s.client.Client
import skunk.Session
import skunk.Transaction
import skunk.codec.all.*
import skunk.syntax.all.*

import scala.util.NotGiven

/** Witnesses that there is no transaction in context. */
type NoTransaction[F[_]] = NotGiven[Transaction[F]]

/** 
 * A collection of services, all bound to a single `Session` and `User`, together with a canonical
 * operation for interacting with the database in a transactional way.
 */
trait Services[F[_]]:

  /** The underlying `Session`. */
  def session: Session[F]

  /** The associated `User`. */
  def user: User

  /** 
   * Define an interaction with the database that will execute a block `fa` within a transaction,
   * with `user` (see above) set in a transaction-local variable `lucuma.user` in the database,
   * available in PostgreSQL as `current_setting('lucuma.user', true)` 
   * 
   * Within the passed block `fa` the current `Transaction` is implicit, as well as this `Services`
   * instance. The purpose is as follows:
   *
   *   - When `Services` is `given` and `Services.Syntax.*` is imported, all members here are 
   *     available unprefixed; i.e., you can just say `groupService.doWhatever(...)` or `user`
   *     or `session` and the reference will be taken from the implicit `Services` in scope.
   *   - When `Transaction` is `given` and `Services.Syntax.*` is imported, the current transaction
   *     is available as `transaction`. This is rarely necessary, but it allows service code to use 
   *     savepoints and rollback to handle errors when necessary.
   *   - Most service methods have a `using Transaction[F]` clause, to prevent them from being
   *     called without an active transaction. This adds a bit of safety.
   * 
   * See also `Services.Syntax.useTransactionally`, which is more commonly used; it does the same
   * thing but works on `Resource[F, Service[F]]`. In this form you never actually see the
   * `Services` instance; it's always implicit.
   */
  def transactionally[A](fa: (Transaction[F], Services[F]) ?=> F[A])(using NoTransaction[F]): F[A]

  /** The `AllocationService`. */
  def allocationService: AllocationService[F]
  
  /** The `AsterismService`. */
  def asterismService: AsterismService[F]

  /** The `ChronicleService`. */
  def chronicleService: ChronicleService[F]

  /** The `ExecutionDigestService`. */
  def executionDigestService: ExecutionDigestService[F]

  /** The `ExecutionEventService`. */
  def executionEventService: ExecutionEventService[F]
  
  /** The `GeneratorParamsService`. */
  def generatorParamsService: GeneratorParamsService[F]
  
  /** The `GmosLongSlitService`. */
  def gmosLongSlitService: GmosLongSlitService[F]

  /** The `GmosSequenceService` */
  def gmosSequenceService: GmosSequenceService[F]
  
  /** The `GroupService`. */
  def groupService: GroupService[F]

  /** The `ObsAttachmentAssignmentService`. */
  def obsAttachmentAssignmentService: ObsAttachmentAssignmentService[F]

  /** Construct an `ObsAttachmentFileService`, given an `S3FileService`.  */
  def obsAttachmentFileService(s3: S3FileService[F]): ObsAttachmentFileService[F]
  
  /** The `ObsAttachmentMetadataService`. */
  def obsAttachmentMetadataService: ObsAttachmentMetadataService[F]
  
  /** The `ObservationService`. */
  def observationService: ObservationService[F]
  
  /** The `ObservingModeServices`. */
  def observingModeServices: ObservingModeServices[F]
  
  /** The `PartnerSplitsService`. */
  def partnerSplitsService: PartnerSplitsService[F]
  
  /** The `ProgramService`. */
  def programService: ProgramService[F]
  
  /** Construct a `ProposalAttachmentFileService`, given an `S3FileService`. */
  def proposalAttachmentFileService(s3: S3FileService[F]): ProposalAttachmentFileService[F]
  
  /** The `ProposalAttachmentMetadataService`. */
  def proposalAttachmentMetadataService: ProposalAttachmentMetadataService[F]
  
  /** The `ProposalService`. */
  def proposalService: ProposalService[F]
  
  /** The `SmartGcalService`. */
  def smartGcalService: SmartGcalService[F]

  /** The `Sequence`. */
  def sequenceService: SequenceService[F]

  /** The `TargetService`. */
  def targetService: TargetService[F]

  /** The `TimingWindowService`. */
  def timingWindowService: TimingWindowService[F]

  /** The `VisitService` */
  def visitService: VisitService[F]

  /** Construct an `ItcService`, given an `ItcClient`.*/
  def itcService(itcClient: ItcClient[F]): ItcService[F]

  /** Construct a `Generator`, given a `CommitHash` and an `ItcClient`.*/
  def generator(commitHash: CommitHash, itcClient: ItcClient[F], ptc: PlannedTimeCalculator.ForInstrumentMode): Generator[F]

  /** Construct a `PlannedTimeRangeService`, given a `CommitHash` and an `ItcClient`.*/
  def plannedTimeRangeService(commitHash: CommitHash, itcClient: ItcClient[F], ptc: PlannedTimeCalculator.ForInstrumentMode): PlannedTimeRangeService[F]

  /** Construct a `GuideEnvironmentService`, given an http4s `Client`, an `ItcClient`, a `CommitHash` and a `PlannedTimeCalculator`. */
  def guideEnvironmentService(httpClient: Client[F], itcClient: ItcClient[F], commitHash: CommitHash, ptc: PlannedTimeCalculator.ForInstrumentMode): GuideEnvironmentService[F]
  

object Services:

  /**
   * Construct a `Services` for the given `User` and `Session`. Service instances are constructed
   * lazily.
   */
  def forUser[F[_]: Concurrent: Trace: UUIDGen](u: User)(s: Session[F]): Services[F[_]] =
    new Services[F]:
      val user = u
      val session = s

      given Services[F] = this // need is for `instantiate` calls below

      def transactionally[A](fa: (Transaction[F], Services[F]) ?=> F[A])(
        using NoTransaction[F]
      ): F[A] =
        session.transaction.use { xa =>
          session.prepareR(sql"select set_config('lucuma.user', $text, true)".query(text)).use { ps =>
            ps.unique(Gid[User.Id].fromString.reverseGet(u.id))
          } >>
          fa(using xa)
        }

      // Services as passed their "owning" `Services` (i.e., `this`) on instantiation, which is
      // circular and requires everything to be done lazily, which luckily is what we want. No point
      // instantiating anything we're not using.
      lazy val allocationService = AllocationService.instantiate
      lazy val asterismService = AsterismService.instantiate
      lazy val chronicleService = ChronicleService.instantiate
      lazy val executionDigestService = ExecutionDigestService.instantiate
      lazy val executionEventService = ExecutionEventService.instantiate
      lazy val generatorParamsService = GeneratorParamsService.instantiate
      lazy val gmosLongSlitService = GmosLongSlitService.instantiate
      lazy val gmosSequenceService = GmosSequenceService.instantiate
      lazy val groupService = GroupService.instantiate
      lazy val obsAttachmentAssignmentService = ObsAttachmentAssignmentService.instantiate
      lazy val obsAttachmentMetadataService = ObsAttachmentMetadataService.instantiate
      lazy val observationService = ObservationService.instantiate
      lazy val observingModeServices = ObservingModeServices.instantiate
      lazy val partnerSplitsService = PartnerSplitsService.instantiate
      lazy val programService = ProgramService.instantiate
      lazy val proposalAttachmentMetadataService = ProposalAttachmentMetadataService.instantiate
      lazy val proposalService = ProposalService.instantiate
      lazy val smartGcalService = SmartGcalService.instantiate
      lazy val sequenceService = SequenceService.instantiate
      lazy val targetService = TargetService.instantiate
      lazy val visitService = VisitService.instantiate
      lazy val timingWindowService = TimingWindowService.instantiate

      // A few services require additional arguments for instantiation that may not always be
      // available, so we require them here instead of demanding them before constructing a
      // `Services` instance.
      def proposalAttachmentFileService(s3: S3FileService[F]) = ProposalAttachmentFileService.instantiate(s3)
      def obsAttachmentFileService(s3: S3FileService[F]) = ObsAttachmentFileService.instantiate(s3)
      def itcService(itcClient: ItcClient[F]) = ItcService.instantiate(itcClient)
      def generator(commitHash: CommitHash, itcClient: ItcClient[F], ptc: PlannedTimeCalculator.ForInstrumentMode) = Generator.instantiate(commitHash, itcClient, ptc)
      def plannedTimeRangeService(commitHash: CommitHash, itcClient: ItcClient[F], ptc: PlannedTimeCalculator.ForInstrumentMode) = PlannedTimeRangeService.instantiate(commitHash, itcClient, ptc)
      def guideEnvironmentService(httpClient: Client[F], itcClient: ItcClient[F], commitHash: CommitHash, ptc: PlannedTimeCalculator.ForInstrumentMode) = GuideEnvironmentService.instantiate(httpClient, itcClient, commitHash, ptc)


  /**
   * This adds syntax to access the members of `Services` and the current `Transaction` when they
   * are present implicitly.
   */
  object Syntax:
    def services[F[_]](using Services[F]): Services[F] = summon
    def transaction[F[_]](using Transaction[F]): Transaction[F] = summon
    def session[F[_]](using Services[F]): Session[F] = summon[Services[F]].session
    def user[F[_]](using Services[F]): User = summon[Services[F]].user
    def allocationService[F[_]](using Services[F]): AllocationService[F] = summon[Services[F]].allocationService
    def asterismService[F[_]](using Services[F]): AsterismService[F] = summon[Services[F]].asterismService
    def chronicleService[F[_]](using Services[F]): ChronicleService[F] = summon[Services[F]].chronicleService
    def executionDigestService[F[_]](using Services[F]): ExecutionDigestService[F] = summon[Services[F]].executionDigestService
    def executionEventService[F[_]](using Services[F]): ExecutionEventService[F] = summon[Services[F]].executionEventService
    def generatorParamsService[F[_]](using Services[F]): GeneratorParamsService[F] = summon[Services[F]].generatorParamsService
    def gmosLongSlitService[F[_]](using Services[F]): GmosLongSlitService[F] = summon[Services[F]].gmosLongSlitService
    def gmosSequenceService[F[_]](using Services[F]): GmosSequenceService[F] = summon[Services[F]].gmosSequenceService
    def groupService[F[_]](using Services[F]): GroupService[F] = summon[Services[F]].groupService
    def obsAttachmentAssignmentService[F[_]](using Services[F]): ObsAttachmentAssignmentService[F] = summon[Services[F]].obsAttachmentAssignmentService
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
    def sequenceService[F[_]](using Services[F]): SequenceService[F] = summon[Services[F]].sequenceService
    def targetService[F[_]](using Services[F]): TargetService[F] = summon[Services[F]].targetService
    def timingWindowService[F[_]](using Services[F]): TimingWindowService[F] = summon[Services[F]].timingWindowService
    def visitService[F[_]](using Services[F]): VisitService[F] = summon[Services[F]].visitService
    def itcService[F[_]](client: ItcClient[F])(using Services[F]): ItcService[F] = summon[Services[F]].itcService(client)
    def generator[F[_]](commitHash: CommitHash, itcClient: ItcClient[F], ptc: PlannedTimeCalculator.ForInstrumentMode)(using Services[F]): Generator[F] = summon[Services[F]].generator(commitHash, itcClient, ptc)
    def plannedTimeRangeService[F[_]](commitHash: CommitHash, itcClient: ItcClient[F], ptc: PlannedTimeCalculator.ForInstrumentMode)(using Services[F]): PlannedTimeRangeService[F] = summon[Services[F]].plannedTimeRangeService(commitHash, itcClient, ptc)
    def guideEnvironmentService[F[_]](httpClient: Client[F], itcClient: ItcClient[F], commitHash: CommitHash, ptc: PlannedTimeCalculator.ForInstrumentMode)(using Services[F]): GuideEnvironmentService[F] = summon[Services[F]].guideEnvironmentService(httpClient, itcClient, commitHash, ptc)

    extension [F[_]: MonadCancelThrow, A](s: Resource[F, Services[F]])

      def useTransactionally(fa: (Transaction[F], Services[F]) ?=> F[A])(
        using NoTransaction[F], NotGiven[Services[F]] // discourage nested calls
      ): F[A] =
          s.use(_.transactionally(fa))
      
      def useNonTransactionally(fa: (NoTransaction[F], Services[F]) ?=> F[A])(
        using NoTransaction[F], NotGiven[Services[F]] // discourage nested calls
      ): F[A] =
          s.use(s => fa(using summon, s))
