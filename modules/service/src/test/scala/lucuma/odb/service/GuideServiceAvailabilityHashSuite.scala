// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.ags.GuideStarName
import lucuma.ags.arb.ArbGuideStarName
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.arb.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.*
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.arb.ArbMd5Hash
import lucuma.odb.service.GuideService.ObservationInfo
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class GuideServiceAvailabilityHashSuite  extends ScalaCheckSuite {
  import ArbConstraintSet.given
  import ArbCoordinates.given
  import ArbGid.given
  import ArbGuideStarName.given
  import ArbMd5Hash.given
  import ArbPosAngleConstraint.given
  import ArbTimeSpan.given
  import ArbTimestamp.given
  import ArbWavelength.given

  test("hashes equal for info equal with different Observation.Id") {
    forAll { (
      obsId1:  Observation.Id,
      obsId2:  Observation.Id,
      cs:      ConstraintSet,
      pac:     PosAngleConstraint,
      ow:      Option[Wavelength],
      oc:      Option[Coordinates],
      ot:      Option[Timestamp],
      od:      Option[TimeSpan],
      gs:      Option[GuideStarName],
      gsHash:  Option[Md5Hash],
      genHash: Md5Hash
    ) =>
      val obsInfo1 = ObservationInfo(obsId1, cs, pac, ow, oc, ot, od, gs, gsHash)
      val obsInfo2 = ObservationInfo(obsId2, cs, pac, ow, oc, ot, od, gs, gsHash)
      assertEquals(obsInfo1.availabilityHash(genHash), obsInfo2.availabilityHash(genHash))
    }
  }

  test("hashes different for different generator hashes") {
    forAll { (
      obsId:    Observation.Id,
      cs:       ConstraintSet,
      pac:      PosAngleConstraint,
      ow:       Option[Wavelength],
      oc:       Option[Coordinates],
      ot:       Option[Timestamp],
      od:       Option[TimeSpan],
      gs:       Option[GuideStarName],
      gsHash:   Option[Md5Hash],
      genHash1: Md5Hash,
      genHash2: Md5Hash
    ) =>
      val hash1 = ObservationInfo(obsId, cs, pac, ow, oc, ot, od, gs, gsHash).availabilityHash(genHash1)
      val hash2 = ObservationInfo(obsId, cs, pac, ow, oc, ot, od, gs, gsHash).availabilityHash(genHash2)

      if (genHash1 === genHash2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different constraint sets") {
    forAll { (
      obsId:   Observation.Id,
      cs1:     ConstraintSet,
      cs2:     ConstraintSet,
      pac:     PosAngleConstraint,
      ow:      Option[Wavelength],
      oc:      Option[Coordinates],
      ot:      Option[Timestamp],
      od:      Option[TimeSpan],
      gs:      Option[GuideStarName],
      gsHash:  Option[Md5Hash],
      genHash: Md5Hash
    ) =>
      val hash1 = ObservationInfo(obsId, cs1, pac, ow, oc, ot, od, gs, gsHash).availabilityHash(genHash)
      val hash2 = ObservationInfo(obsId, cs2, pac, ow, oc, ot, od, gs, gsHash).availabilityHash(genHash)

      if (cs1 === cs2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different position angle constraints") {
    forAll { (
      obsId:   Observation.Id,
      cs:      ConstraintSet,
      pac1:    PosAngleConstraint,
      pac2:    PosAngleConstraint,
      ow:      Option[Wavelength],
      oc:      Option[Coordinates],
      ot:      Option[Timestamp],
      od:      Option[TimeSpan],
      gs:      Option[GuideStarName],
      gsHash:  Option[Md5Hash],
      genHash: Md5Hash
    ) =>
      val obsInfo1 = ObservationInfo(obsId, cs, pac1, ow, oc, ot, od, gs, gsHash)
      val obsInfo2 = ObservationInfo(obsId, cs, pac2, ow, oc, ot, od, gs, gsHash)

      val hash1 = obsInfo1.availabilityHash(genHash)
      val hash2 = obsInfo2.availabilityHash(genHash)

      // hash just depends on the angles we need to check. So, for example,
      // unconstrained is the same as average parallactic
      if (obsInfo1.availabilityAngles === obsInfo2.availabilityAngles)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different wavelengths") {
    forAll { (
      obsId:   Observation.Id,
      cs:      ConstraintSet,
      pac:     PosAngleConstraint,
      ow1:     Option[Wavelength],
      ow2:     Option[Wavelength],
      oc:      Option[Coordinates],
      ot:      Option[Timestamp],
      od:      Option[TimeSpan],
      gs:      Option[GuideStarName],
      gsHash:  Option[Md5Hash],
      genHash: Md5Hash
    ) =>
      val hash1 = ObservationInfo(obsId, cs, pac, ow1, oc, ot, od, gs, gsHash).availabilityHash(genHash)
      val hash2 = ObservationInfo(obsId, cs, pac, ow2, oc, ot, od, gs, gsHash).availabilityHash(genHash)

      if (ow1 === ow2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different explicit base coordinates") {
    forAll { (
      obsId:   Observation.Id,
      cs:      ConstraintSet,
      pac:     PosAngleConstraint,
      ow:      Option[Wavelength],
      oc1:     Option[Coordinates],
      oc2:     Option[Coordinates],
      ot:      Option[Timestamp],
      od:      Option[TimeSpan],
      gs:      Option[GuideStarName],
      gsHash:  Option[Md5Hash],
      genHash: Md5Hash
    ) =>
      val hash1 = ObservationInfo(obsId, cs, pac, ow, oc1, ot, od, gs, gsHash).availabilityHash(genHash)
      val hash2 = ObservationInfo(obsId, cs, pac, ow, oc2, ot, od, gs, gsHash).availabilityHash(genHash)

      if (oc1 === oc2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes equal for other parameters differing") {
    forAll { (
      obsId:   Observation.Id,
      cs:      ConstraintSet,
      pac:     PosAngleConstraint,
      ow:      Option[Wavelength],
      oc:      Option[Coordinates],
      ot1:     Option[Timestamp],
      ot2:     Option[Timestamp],
      od1:     Option[TimeSpan],
      od2:     Option[TimeSpan],
      gs1:     Option[GuideStarName],
      gs2:     Option[GuideStarName],
      gsHash1: Option[Md5Hash],
      gsHash2: Option[Md5Hash],
      genHash: Md5Hash
    ) =>
      val obsInfo1 = ObservationInfo(obsId, cs, pac, ow, oc, ot1, od1, gs1, gsHash1)
      val obsInfo2 = ObservationInfo(obsId, cs, pac, ow, oc, ot2, od2, gs2, gsHash2)
      assertEquals(obsInfo1.availabilityHash(genHash), obsInfo2.availabilityHash(genHash))
    }
  }
}
