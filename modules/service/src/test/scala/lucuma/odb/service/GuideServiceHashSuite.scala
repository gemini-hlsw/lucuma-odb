// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.arb.*
import lucuma.core.util.arb.*
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.arb.ArbMd5Hash
import lucuma.odb.service.GuideService.ObservationInfo
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class GuideServiceHashSuite  extends ScalaCheckSuite {
  import ArbConstraintSet.given
  import ArbCoordinates.given
  import ArbGid.given
  import ArbMd5Hash.given
  import ArbPosAngleConstraint.given
  import ArbWavelength.given

  test("hashes equal for info equal with different Observation.Id") {
    forAll { (
      obsId1:  Observation.Id,
      obsId2:  Observation.Id,
      cs:      ConstraintSet,
      pac:     PosAngleConstraint,
      ow:      Option[Wavelength],
      oc:      Option[Coordinates],
      genHash: Md5Hash
    ) =>
      val obsInfo1 = ObservationInfo(obsId1, cs, pac, ow, oc)
      val obsInfo2 = ObservationInfo(obsId2, cs, pac, ow, oc)
      assertEquals(obsInfo1.hash(genHash), obsInfo2.hash(genHash))
    }
  }

  test("hashes different for different generator hashes") {
    forAll { (
      obsId:    Observation.Id,
      cs:       ConstraintSet,
      pac:      PosAngleConstraint,
      ow:       Option[Wavelength],
      oc:       Option[Coordinates],
      genHash1: Md5Hash,
      genHash2: Md5Hash
    ) =>
      val hash1 = ObservationInfo(obsId, cs, pac, ow, oc).hash(genHash1)
      val hash2 = ObservationInfo(obsId, cs, pac, ow, oc).hash(genHash2)

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
      genHash: Md5Hash
    ) =>
      val hash1 = ObservationInfo(obsId, cs1, pac, ow, oc).hash(genHash)
      val hash2 = ObservationInfo(obsId, cs2, pac, ow, oc).hash(genHash)

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
      genHash: Md5Hash
    ) =>
      val obsInfo1 = ObservationInfo(obsId, cs, pac1, ow, oc)
      val obsInfo2 = ObservationInfo(obsId, cs, pac2, ow, oc)

      val hash1 = obsInfo1.hash(genHash)
      val hash2 = obsInfo2.hash(genHash)

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
      genHash: Md5Hash
    ) =>
      val hash1 = ObservationInfo(obsId, cs, pac, ow1, oc).hash(genHash)
      val hash2 = ObservationInfo(obsId, cs, pac, ow2, oc).hash(genHash)

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
      genHash: Md5Hash
    ) =>
      val hash1 = ObservationInfo(obsId, cs, pac, ow, oc1).hash(genHash)
      val hash2 = ObservationInfo(obsId, cs, pac, ow, oc2).hash(genHash)

      if (oc1 === oc2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

}
