// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.ags.GuideStarName
import lucuma.ags.arb.ArbGuideStarName
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.CassRotator
import lucuma.core.enums.FieldLens
import lucuma.core.enums.GuideProbe
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.arb.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.*
import lucuma.odb.data.AltairConfiguration
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.arb.ArbMd5Hash
import lucuma.odb.service.GuideService.ObservationInfo
import lucuma.odb.service.GuideService.SearchTargets
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class GuideServiceGuideStarHash extends ScalaCheckSuite {
  import ArbConstraintSet.given
  import ArbEnumerated.given
  import ArbCoordinates.given
  import ArbGid.given
  import ArbGuideStarName.given
  import ArbMd5Hash.given
  import ArbPosAngleConstraint.given
  import ArbTimeSpan.given
  import ArbTimestamp.given

  val NoTargets: SearchTargets = SearchTargets(Nil, None)

  test("hashes equal for info equal with different Observation.Id") {
    forAll { (
      obsId1:        Observation.Id,
      obsId2:        Observation.Id,
      pid:           Program.Id,
      cs:            ConstraintSet,
      pac:           PosAngleConstraint,
      oc:            Option[Coordinates],
      ot:            Option[Timestamp],
      od:            Option[TimeSpan],
      gs:            Option[GuideStarName],
      gsHash:        Option[Md5Hash],
      settings:      Md5Hash,
    ) =>
      val obsInfo1 = ObservationInfo(obsId1, pid, cs, pac, oc, ot, od, gs, gsHash, None, None)
      val obsInfo2 = ObservationInfo(obsId2, pid, cs, pac, oc, ot, od, gs, gsHash, None, None)
      assertEquals(obsInfo1.overrideHash(settings, NoTargets), obsInfo2.overrideHash(settings, NoTargets), "hashes should be equal")
    }
  }

  test("hashes different for different search settings") {
    forAll { (
      obsId:         Observation.Id,
      pid:           Program.Id,
      cs:            ConstraintSet,
      pac:           PosAngleConstraint,
      oc:            Option[Coordinates],
      ot:            Option[Timestamp],
      od:            Option[TimeSpan],
      gs:            Option[GuideStarName],
      gsHash:        Option[Md5Hash],
      settings1:     Md5Hash,
      settings2:     Md5Hash,
    ) =>
      val hash1 = ObservationInfo(obsId, pid, cs, pac, oc, ot, od, gs, gsHash, None, None).overrideHash(settings1, NoTargets)
      val hash2 = ObservationInfo(obsId, pid, cs, pac, oc, ot, od, gs, gsHash, None, None).overrideHash(settings2, NoTargets)

      if (settings1 === settings2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different constraint sets") {
    forAll { (
      obsId:         Observation.Id,
      pid:           Program.Id,
      cs1:           ConstraintSet,
      cs2:           ConstraintSet,
      pac:           PosAngleConstraint,
      oc:            Option[Coordinates],
      ot:            Option[Timestamp],
      od:            Option[TimeSpan],
      gs:            Option[GuideStarName],
      gsHash:        Option[Md5Hash],
      settings:      Md5Hash,
    ) =>
      val hash1 = ObservationInfo(obsId, pid, cs1, pac, oc, ot, od, gs, gsHash, None, None).overrideHash(settings, NoTargets)
      val hash2 = ObservationInfo(obsId, pid, cs2, pac, oc, ot, od, gs, gsHash, None, None).overrideHash(settings, NoTargets)

      if (cs1 === cs2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different position angle constraints") {
    forAll { (
      obsId:         Observation.Id,
      pid:           Program.Id,
      cs:            ConstraintSet,
      pac1:          PosAngleConstraint,
      pac2:          PosAngleConstraint,
      oc:            Option[Coordinates],
      ot:            Option[Timestamp],
      od:            Option[TimeSpan],
      gs:            Option[GuideStarName],
      gsHash:        Option[Md5Hash],
      settings:      Md5Hash,
    ) =>
      val obsInfo1 = ObservationInfo(obsId, pid, cs, pac1, oc, ot, od, gs, gsHash, None, None)
      val obsInfo2 = ObservationInfo(obsId, pid, cs, pac2, oc, ot, od, gs, gsHash, None, None)

      val hash1 = obsInfo1.overrideHash(settings, NoTargets)
      val hash2 = obsInfo2.overrideHash(settings, NoTargets)

      if (pac1 === pac2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different explicit base coordinates") {
    forAll { (
      obsId:         Observation.Id,
      pid:           Program.Id,
      cs:            ConstraintSet,
      pac:           PosAngleConstraint,
      oc1:           Option[Coordinates],
      oc2:           Option[Coordinates],
      ot:            Option[Timestamp],
      od:            Option[TimeSpan],
      gs:            Option[GuideStarName],
      gsHash:        Option[Md5Hash],
      settings:      Md5Hash,
    ) =>
      val hash1 = ObservationInfo(obsId, pid, cs, pac, oc1, ot, od, gs, gsHash, None, None).overrideHash(settings, NoTargets)
      val hash2 = ObservationInfo(obsId, pid, cs, pac, oc2, ot, od, gs, gsHash, None, None).overrideHash(settings, NoTargets)

      if (oc1 === oc2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different observation times") {
    forAll { (
      obsId:         Observation.Id,
      pid:           Program.Id,
      cs:            ConstraintSet,
      pac:           PosAngleConstraint,
      oc:            Option[Coordinates],
      ot1:           Option[Timestamp],
      ot2:           Option[Timestamp],
      od:            Option[TimeSpan],
      gs:            Option[GuideStarName],
      gsHash:        Option[Md5Hash],
      settings:      Md5Hash,
    ) =>
      val hash1 = ObservationInfo(obsId, pid, cs, pac, oc, ot1, od, gs, gsHash, None, None).overrideHash(settings, NoTargets)
      val hash2 = ObservationInfo(obsId, pid, cs, pac, oc, ot2, od, gs, gsHash, None, None).overrideHash(settings, NoTargets)

      if (ot1 === ot2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different explicit guide probes") {
    forAll { (
      obsId:         Observation.Id,
      pid:           Program.Id,
      cs:            ConstraintSet,
      pac:           PosAngleConstraint,
      oc:            Option[Coordinates],
      ot:            Option[Timestamp],
      od:            Option[TimeSpan],
      gs:            Option[GuideStarName],
      gsHash:        Option[Md5Hash],
      gp1:           Option[GuideProbe],
      gp2:           Option[GuideProbe],
      settings:      Md5Hash,
    ) =>
      val hash1 = ObservationInfo(obsId, pid, cs, pac, oc, ot, od, gs, gsHash, gp1, None).overrideHash(settings, NoTargets)
      val hash2 = ObservationInfo(obsId, pid, cs, pac, oc, ot, od, gs, gsHash, gp2, None).overrideHash(settings, NoTargets)

      if (gp1 === gp2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different observation durations") {
    forAll { (
      obsId:         Observation.Id,
      pid:           Program.Id,
      cs:            ConstraintSet,
      pac:           PosAngleConstraint,
      oc:            Option[Coordinates],
      ot:            Option[Timestamp],
      od1:           Option[TimeSpan],
      od2:           Option[TimeSpan],
      gs:            Option[GuideStarName],
      gsHash:        Option[Md5Hash],
      settings:      Md5Hash,
    ) =>
      val hash1 = ObservationInfo(obsId, pid, cs, pac, oc, ot, od1, gs, gsHash, None, None).overrideHash(settings, NoTargets)
      val hash2 = ObservationInfo(obsId, pid, cs, pac, oc, ot, od2, gs, gsHash, None, None).overrideHash(settings, NoTargets)

      if (od1 === od2)
        assertEquals(hash1, hash2)
      else
        assertNotEquals(hash1, hash2)
    }
  }

  test("hashes different for different Altair configurations") {
    forAll { (
      obsId:         Observation.Id,
      pid:           Program.Id,
      cs:            ConstraintSet,
      pac:           PosAngleConstraint,
      oc:            Option[Coordinates],
      ot:            Option[Timestamp],
      od:            Option[TimeSpan],
      gs:            Option[GuideStarName],
      gsHash:        Option[Md5Hash],
      settings:      Md5Hash,
    ) =>
      def hashFor(altair: Option[AltairConfiguration]): Md5Hash =
        ObservationInfo(obsId, pid, cs, pac, oc, ot, od, gs, gsHash, None, altair).overrideHash(settings, NoTargets)

      val ngs: AltairConfiguration =
        AltairConfiguration(AltairMode.Ngs, none, CassRotator.Following, AltairNdFilter.Out)

      val variants: List[Option[AltairConfiguration]] =
        List(
          none,
          ngs.some,
          AltairConfiguration.mode.replace(AltairMode.Lgs)(ngs).some,
          AltairConfiguration.explicitFieldLens.replace(FieldLens.In.some)(ngs).some,
          AltairConfiguration.cassRotator.replace(CassRotator.Fixed)(ngs).some,
          AltairConfiguration.ndFilter.replace(AltairNdFilter.In)(ngs).some
        )

      assertEquals(variants.map(hashFor).distinct.length, variants.length, "hashes should all differ")
    }
  }

  test("hashes equal for other parameters differing") {
    forAll { (
      obsId:         Observation.Id,
      pid1:          Program.Id,
      pid2:          Program.Id,
      cs:            ConstraintSet,
      pac:           PosAngleConstraint,
      oc:            Option[Coordinates],
      ot:            Option[Timestamp],
      od:            Option[TimeSpan],
      gs1:           Option[GuideStarName],
      gs2:           Option[GuideStarName],
      gsHash1:       Option[Md5Hash],
      gsHash2:       Option[Md5Hash],
      settings:      Md5Hash,
    ) =>
      val obsInfo1 = ObservationInfo(obsId, pid1, cs, pac, oc, ot, od, gs1, gsHash1, None, None)
      val obsInfo2 = ObservationInfo(obsId, pid2, cs, pac, oc, ot, od, gs2, gsHash2, None, None)
      assertEquals(obsInfo1.overrideHash(settings, NoTargets), obsInfo2.overrideHash(settings, NoTargets), "hashes should be equal")
    }
  }

}
