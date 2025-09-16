// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.data.NonEmptyList
import cats.kernel.Eq
import cats.syntax.all.*
import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.Site
import lucuma.core.model.*
import lucuma.core.model.arb.ArbUserInvitation.given
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.Dataset.Filename
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.core.util.Uid
import lucuma.odb.data.OdbError.Tag
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*

import java.time.LocalDate
import java.util.UUID

class OdbErrorSuite extends DisciplineSuite with ArbitraryInstances:

  given Eq[OdbError] =
    Eq.by(_.asJson)

  given Arbitrary[OdbError.Tag] =
    Arbitrary:
      Gen.oneOf(OdbError.Tag.values.toSeq)

  given [A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary:
      (arbitrary[A], arbitrary[List[A]]).mapN(NonEmptyList.apply)

  given [A](using gid: Gid[A]): Arbitrary[A] =
    Arbitrary:
      Gen.posNum[Long].map(gid.fromLong.getOption(_).get)

  given [A](using uid: Uid[A]): Arbitrary[A] =
    Arbitrary:
      arbitrary[UUID].map: uuid =>
        uid.fromString.getOption(s"${uid.tag}-$uuid").get        

  given [A](using e: Enumerated[A]): Arbitrary[A] =
    Arbitrary:
      Gen.oneOf(e.all)

  given Arbitrary[Filename] =
    Arbitrary:
      for
        site <- arbitrary[Site]
        date <- arbitrary[LocalDate].filter(d => d.getYear > 0 && d .getYear < 9999)
        ix   <- arbitrary[PosInt]
      yield Filename.from(site, date, ix).get

  given Arbitrary[OdbError] =
    Arbitrary:
      (arbitrary[OdbError.Tag], arbitrary[Option[String]]).tupled.flatMap: (tag, detail) =>
        tag match
          case Tag.InvalidArgument           => OdbError.InvalidArgument(detail).pure[Gen]
          case Tag.NoAction                  => OdbError.NoAction(detail).pure[Gen]
          case Tag.NotAuthorized             => arbitrary[User.Id].map(OdbError.NotAuthorized(_, detail))
          case Tag.InvitationError           => arbitrary[UserInvitation.Id].map(OdbError.InvitationError(_, detail))
          case Tag.InvalidProgram            => arbitrary[Program.Id].map(OdbError.InvalidProgram(_, detail))
          case Tag.InvalidObservation        => arbitrary[Observation.Id].map(OdbError.InvalidObservation(_, detail))
          case Tag.InvalidObservationList    => arbitrary[NonEmptyList[Observation.Id]].map(OdbError.InvalidObservationList(_, detail))
          case Tag.SequenceUnavailable       => arbitrary[Observation.Id].map(OdbError.SequenceUnavailable(_, detail))
          case Tag.InvalidTarget             => arbitrary[Target.Id].map(OdbError.InvalidTarget(_, detail))
          case Tag.InvalidTargetList         => (arbitrary[Program.Id], arbitrary[NonEmptyList[Target.Id]]).mapN(OdbError.InvalidTargetList(_, _, detail))
          case Tag.InvalidVisit              => arbitrary[Visit.Id].map(OdbError.InvalidVisit(_, detail))
          case Tag.InvalidStep               => arbitrary[Step.Id].map(OdbError.InvalidStep(_, detail))
          case Tag.InvalidFilename           => arbitrary[Filename].map(OdbError.InvalidFilename(_, detail))
          case Tag.InvalidAtom               => arbitrary[Atom.Id].map(OdbError.InvalidAtom(_, detail))
          case Tag.InvalidDataset            => arbitrary[Dataset.Id].map(OdbError.InvalidDataset(_, detail))
          case Tag.InvalidProgramUser        => arbitrary[ProgramUser.Id].map(OdbError.InvalidProgramUser(_, detail))
          case Tag.InvalidUser               => arbitrary[User.Id].map(OdbError.InvalidUser(_, detail))
          case Tag.UpdateFailed              => OdbError.UpdateFailed(detail).pure[Gen]
          case Tag.ItcError                  => OdbError.ItcError(detail).pure[Gen]
          case Tag.GuideEnvironmentError     => OdbError.GuideEnvironmentError(detail).pure[Gen]
          case Tag.EmailSendError            => OdbError.EmailSendError(detail).pure[Gen]
          case Tag.InconsistentGroup         => OdbError.InconsistentGroupError(detail).pure[Gen]
          case Tag.InvalidConfiguration      => OdbError.InvalidConfiguration(detail).pure[Gen]
          case Tag.InvalidWorkflowTransition => (arbitrary[ObservationWorkflowState], arbitrary[ObservationWorkflowState]).mapN(OdbError.InvalidWorkflowTransition(_, _, detail))
          case Tag.RemoteServiceCallError    => OdbError.RemoteServiceCallError(detail).pure[Gen]

  checkAll("OdbErrorCodec", CodecTests[OdbError].codec)


