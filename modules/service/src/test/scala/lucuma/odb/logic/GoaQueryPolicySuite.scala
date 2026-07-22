// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.goa.GoaParams
import lucuma.catalog.goa.syntax.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosLongSlitAcquisitionRoi
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.Instrument
import lucuma.core.enums.KeckInstrument
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Region
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.Ephemeris
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.Target
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Enumerated
import lucuma.odb.data.ArchiveSearchCenter
import lucuma.odb.logic.GoaQueryPolicy.TargetPointing
import lucuma.odb.sequence.exchange.Config as Exchange
import lucuma.odb.sequence.gmos.imaging.Config as GmosImaging
import lucuma.odb.sequence.gmos.imaging.Filter as GmosImagingFilter
import lucuma.odb.sequence.gmos.longslit.AcquisitionConfig
import lucuma.odb.sequence.gmos.longslit.Config as GmosLongSlit
import lucuma.odb.sequence.imaging.Variant
import lucuma.odb.sequence.visitor.Config as Visitor
import munit.FunSuite

import scala.collection.immutable.SortedMap

/**
 * The science policy behind the Archive Duplication Search, tested directly.
 * These are the decisions expected to change, so the point of pinning them is
 * that a change shows up here rather than silently in a match count.
 */
class GoaQueryPolicySuite extends FunSuite:

  // -- Fixtures ------------------------------------------------------------

  private val wavelength: Wavelength =
    Wavelength.fromIntNanometers(500).get

  private val exposureTimeMode: ExposureTimeMode =
    ExposureTimeMode.TimeAndCountMode(10.secondTimeSpan, PosInt.unsafeFrom(1), wavelength)

  private val gmosNorthImaging: GmosImaging.GmosNorth =
    GmosImaging.GmosNorth(
      variant = Variant.Interleaved.Default,
      filters = NonEmptyList.of(GmosImagingFilter(GmosNorthFilter.GPrime, exposureTimeMode)),
      common  = GmosImaging.Common(defaultBin = GmosBinning.Two)
    )

  private def gmosNorthLongSlit(fpu: GmosNorthFpu): GmosLongSlit.GmosNorth =
    GmosLongSlit.GmosNorth(
      grating     = GmosNorthGrating.B1200_G5301,
      filter      = GmosNorthFilter.GPrime.some,
      fpu         = fpu,
      common      = GmosLongSlit.Common(
        centralWavelength         = wavelength,
        exposureTimeMode          = exposureTimeMode,
        defaultXBin               = GmosXBinning.One,
        explicitXBin              = none,
        defaultYBin               = GmosYBinning.One,
        explicitYBin              = none,
        explicitAmpReadMode       = none[GmosAmpReadMode],
        explicitAmpGain           = none[GmosAmpGain],
        explicitRoi               = GmosRoi.CentralSpectrum.some,
        explicitWavelengthDithers = none,
        explicitSpatialOffsets    = none
      ),
      acquisition = AcquisitionConfig.GmosNorth(
        exposureTimeMode = exposureTimeMode,
        defaultFilter    = GmosNorthFilter.GPrime,
        explicitFilter   = none,
        defaultRoi       = GmosLongSlitAcquisitionRoi.Ccd2,
        explicitRoi      = none
      )
    )

  private val exchange: Exchange =
    Exchange(KeckInstrument.Hires.asLeft, 1.hourTimeSpan)

  private val maroonX: Visitor =
    Visitor(
      mode               = VisitorObservingModeType.MaroonX,
      centralWavelength  = wavelength,
      agsDiameter        = Angle.fromDoubleArcseconds(60.0),
      scienceFovDiameter = Angle.fromDoubleArcseconds(10.0),
      name               = none,
      totalRequestTime   = none
    )

  private def coordinates(raDeg: Double, decDeg: Double): Coordinates =
    Coordinates(
      RightAscension.fromDoubleDegrees(raDeg),
      Declination.fromDoubleDegrees(decDeg).get
    )

  private val base: Coordinates   = coordinates(10.0, 20.0)
  private val center: Coordinates = coordinates(30.0, 40.0)

  private def name(s: String): NonEmptyString =
    NonEmptyString.unsafeFrom(s)

  private val sourceProfile: SourceProfile =
    SourceProfile.Point(BandNormalized(none, SortedMap.empty))

  /** Arcseconds to the milliarcsecond, which is as fine as a search radius is worth pinning. */
  private def arcseconds(a: Angle): BigDecimal =
    (BigDecimal(a.toMicroarcseconds) / 1_000_000).setScale(3, BigDecimal.RoundingMode.HALF_UP)

  // -- Duplication-Equivalence Group ---------------------------------------

  test("GMOS-N and GMOS-S are searched against each other"):
    // GOA's single GMOS umbrella cannot be expressed through GoaParams, so the
    // group is two queries whose union is the same record set.
    val both = List(Instrument.GmosNorth, Instrument.GmosSouth)
    assertEquals(GoaQueryPolicy.equivalenceGroup(Instrument.GmosNorth), both)
    assertEquals(GoaQueryPolicy.equivalenceGroup(Instrument.GmosSouth), both)

  test("Alopeke and Zorro are searched against each other"):
    val both = List(Instrument.Alopeke, Instrument.Zorro)
    assertEquals(GoaQueryPolicy.equivalenceGroup(Instrument.Alopeke), both)
    assertEquals(GoaQueryPolicy.equivalenceGroup(Instrument.Zorro), both)

  test("Flamingos-2 and GNIRS are searched against each other"):
    val both = List(Instrument.Flamingos2, Instrument.Gnirs)
    assertEquals(GoaQueryPolicy.equivalenceGroup(Instrument.Flamingos2), both)
    assertEquals(GoaQueryPolicy.equivalenceGroup(Instrument.Gnirs), both)

  test("an instrument with no equivalent is searched on its own"):
    List(Instrument.Ghost, Instrument.Gsaoi, Instrument.Niri, Instrument.Gpi, Instrument.Igrins2)
      .foreach: i =>
        assertEquals(GoaQueryPolicy.equivalenceGroup(i), List(i), i.tag)

  test("an instrument GOA does not know yields no queries at all"):
    assertEquals(GoaQueryPolicy.equivalenceGroup(Instrument.MaroonX), Nil)

  test("every instrument in a group is one GOA can be asked about"):
    Enumerated[Instrument].all.foreach: i =>
      assert(GoaQueryPolicy.equivalenceGroup(i).forall(_.goaName.isDefined), i.tag)

  test("equivalence is symmetric: every member of a group has the same group"):
    Enumerated[Instrument].all.foreach: i =>
      val group = GoaQueryPolicy.equivalenceGroup(i)
      group.foreach: j =>
        assertEquals(GoaQueryPolicy.equivalenceGroup(j), group, s"${i.tag} / ${j.tag}")

  // -- Search center -------------------------------------------------------

  test("an explicit base wins over the asterism center"):
    assertEquals(
      GoaQueryPolicy.searchCenter(base.some, center.some, List(TargetPointing.Sidereal)),
      ArchiveSearchCenter.Sidereal(base).some
    )

  test("the asterism center is used when there is no explicit base"):
    assertEquals(
      GoaQueryPolicy.searchCenter(none, center.some, List(TargetPointing.Sidereal)),
      ArchiveSearchCenter.Sidereal(center).some
    )

  test("a wholly non-sidereal asterism is searched by name, not by a resolved center"):
    // The center a moving target resolves to is a moment in time; the name is
    // what the archive indexes it under, so the name is preferred.
    assertEquals(
      GoaQueryPolicy.searchCenter(none, center.some, List(TargetPointing.NonSidereal(name("Halley")))),
      ArchiveSearchCenter.NonSidereal(name("Halley")).some
    )

  test("an explicit base still wins over a non-sidereal asterism"):
    assertEquals(
      GoaQueryPolicy.searchCenter(base.some, none, List(TargetPointing.NonSidereal(name("Halley")))),
      ArchiveSearchCenter.Sidereal(base).some
    )

  test("an asterism only partly non-sidereal falls back to the center"):
    assertEquals(
      GoaQueryPolicy.searchCenter(
        none,
        center.some,
        List(TargetPointing.NonSidereal(name("Halley")), TargetPointing.Sidereal)
      ),
      ArchiveSearchCenter.Sidereal(center).some
    )

  test("no pointing and no usable name is not a search center"):
    assertEquals(GoaQueryPolicy.searchCenter(none, none, Nil), none)
    assertEquals(GoaQueryPolicy.searchCenter(none, none, List(TargetPointing.Sidereal)), none)
    assertEquals(GoaQueryPolicy.searchCenter(none, none, List(TargetPointing.Unresolvable)), none)

  test("targets are classified by how they can be pointed at"):
    val sidereal    = Target.Sidereal(name("Star"), SiderealTracking.const(base), sourceProfile, none)
    val nonSidereal = Target.Nonsidereal(name("Halley"), Ephemeris.Key.Comet("1P"), sourceProfile)
    val opportunity = Target.Opportunity(name("Anywhere"), Region.Full, sourceProfile)

    assertEquals(TargetPointing.fromTarget(sidereal), TargetPointing.Sidereal)
    assertEquals(TargetPointing.fromTarget(nonSidereal), TargetPointing.NonSidereal(name("Halley")))
    assertEquals(TargetPointing.fromTarget(opportunity), TargetPointing.Unresolvable)

  // -- Search radius -------------------------------------------------------

  test("an imaging field of view sets the radius"):
    assertEquals(GoaQueryPolicy.searchRadius(gmosNorthImaging).map(arcseconds), BigDecimal("211.018").some)

  test("a spectroscopy field of view sets the radius"):
    assertEquals(
      GoaQueryPolicy.searchRadius(gmosNorthLongSlit(GmosNorthFpu.LongSlit_1_00)).map(arcseconds),
      BigDecimal("165.201").some
    )

  test("a long slit searches a smaller area than imaging with the same instrument"):
    val imaging  = GoaQueryPolicy.searchRadius(gmosNorthImaging).get
    val longSlit = GoaQueryPolicy.searchRadius(gmosNorthLongSlit(GmosNorthFpu.LongSlit_1_00)).get
    assert(Angle.AngleOrder.lt(longSlit, imaging), s"$longSlit >= $imaging")

  test("a mode with no measurable science area has no radius"):
    // A long slit configuration carrying an IFU FPU evaluates to an empty
    // shape; there is nothing to search, which is not the same as searching a
    // zero-radius field.
    assertEquals(GoaQueryPolicy.searchRadius(gmosNorthLongSlit(GmosNorthFpu.Ifu2Slits)), none)

  test("an exchange observation has no Gemini science area to measure"):
    assertEquals(GoaQueryPolicy.searchRadius(exchange), none)

  // -- The three combined --------------------------------------------------

  test("a GMOS observation asks both sites about the same field"):
    val ps = GoaQueryPolicy.queries(gmosNorthImaging, base.some, none, List(TargetPointing.Sidereal))
    assertEquals(ps.map(_.instrument), List(Instrument.GmosNorth, Instrument.GmosSouth))
    assertEquals(ps.map(_.searchRadius).distinct.length, 1)
    assertEquals(
      ps.collect { case s: GoaParams.Sidereal => s.coords }.distinct,
      List(base)
    )

  test("a non-sidereal observation is queried by target name"):
    val ps = GoaQueryPolicy.queries(
      gmosNorthImaging,
      none,
      center.some,
      List(TargetPointing.NonSidereal(name("Halley")))
    )
    assertEquals(ps.collect { case n: GoaParams.NonSidereal => n.targetName }, List("Halley", "Halley"))

  test("queries are never pinned to a date range in v1"):
    val ps = GoaQueryPolicy.queries(gmosNorthImaging, base.some, none, List(TargetPointing.Sidereal))
    assert(ps.nonEmpty)
    assert(ps.forall(_.dateRange.isEmpty))

  test("an observation GOA cannot be asked about yields no queries"):
    assertEquals(
      GoaQueryPolicy.queries(maroonX, base.some, none, List(TargetPointing.Sidereal)),
      Nil
    )

  test("an observation with no pointing yields no queries"):
    assertEquals(GoaQueryPolicy.queries(gmosNorthImaging, none, none, Nil), Nil)

  test("an observation with no measurable science area yields no queries"):
    assertEquals(
      GoaQueryPolicy.queries(exchange, base.some, none, List(TargetPointing.Sidereal)),
      Nil
    )
