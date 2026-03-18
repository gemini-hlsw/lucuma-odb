// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.issue.shortcut

import cats.effect.IO
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.service.TrackingService

import java.time.Instant

// Test ephemeris caching
class ShortCut_6900 extends OdbSuite:

  val pi = TestUsers.Standard.pi(nextId, nextId)
  val validUsers = List(pi)

  object Intervals:
    val start: Timestamp = Timestamp.fromInstantTruncatedAndBounded(Instant.ofEpochMilli(1762965822812L)) // arbitrary date in Nov 2025
    def hours(n: Int): TimestampInterval = TimestampInterval.between(start, start.plusSecondsOption(n * 60 * 60).get)
    def days(n: Int): TimestampInterval = hours(n * 24)

  def getEphemerisCacheMisses(user: User, oid: Observation.Id, interval: TimestampInterval, force: Boolean = false) =
    withServices(user): svcs =>
      svcs.trackingService.asInstanceOf[TrackingService.Whitebox[IO]].getTrackingSnapshotEx(oid, interval, force).map: r =>
        r.toOption.get.base._2

  test("Identical requests should be fully cached."):
    for 
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      n1  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(10), true), 24)
      n2  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(10)), 0)
    yield ()

  test("Requests within the same day should be cached."):
    for 
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      n1  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(10), true), 24)
      n2  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(20)), 0) 
    yield ()

  test("A longer ephemeris should have some cache misses."):
    for 
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      n1  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(10), true), 24)
      n2  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(50)), 8) 
    yield ()

  test("A longer ephemeris should have some cache misses, but only once."):
    for 
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      n1  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(10), true), 24)
      n2  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(50)), 8) 
      n2  <- assertIO(getEphemerisCacheMisses(pi, oid, Intervals.hours(50)), 0) 
    yield ()

  override def horizonsFixture: Map[Set[(String, String)], String] =
    super.horizonsFixture ++ Map(
      Set(("format", "text"), ("MAKE_EPHEM", "YES"), ("CENTER", "T15"), ("COMMAND", "'NAME=1P;CAP'"), ("START_TIME", "'2025-Nov-12 00:00:00.000'"), ("STOP_TIME", "'2025-Nov-13 00:00:00.000'"), ("STEP_SIZE", "60m"), ("extra_prec", "YES"), ("time_digits", "FRACSEC"), ("QUANTITIES", "'1,3,8,9'")) ->
      """|API VERSION: 1.2
      |API SOURCE: NASA/JPL Horizons API
      |
      |*******************************************************************************
      |JPL/HORIZONS                      1P/Halley                2025-Dec-22 12:22:35
      |Rec #:90000030        Soln.date: 2025-Nov-21_15:57:34   # obs: 8518 (1835-1994)
      | 
      |IAU76/J2000 helio. ecliptic osc. elements (au, days, deg., period=Julian yrs):
      | 
      |  EPOCH=  2439875.5 ! 1968-Jan-20.0000000 (TDB)    RMSW= n.a.
      |   EC= .9679359956953212   QR= .5748638313743413   TP= 2446469.9736161465
      |   OM= 59.09894720612437   W= 112.2414314637764    IN= 162.1905300439129
      |   A= 17.92863504856929    MA= 274.3823371364693   ADIST= 35.28240626576424
      |   PER= 75.915252807404    N= .012983244           ANGMOM= .018296559
      |   DAN= 1.78543            DDN= .82795             L= 305.8544912
      |   B= 16.4450919           MOID= .0745097          TP= 1986-Feb-08.4736161465
      | 
      |Comet physical (GM= km^3/s^2; RAD= km):
      |   GM= n.a.                RAD= 5.5
      |   M1=  5.5      M2=  13.6     k1=  8.     k2=  5.      PHCOF=  .030
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au):
      |   AMRAT=  0.                                      DT=  0.
      |   A1= 4.887055233121E-10  A2= 1.554720290005E-10  A3= 0.
      | Standard model:
      |   ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808
      | 
      |COMET comments 
      |1: soln ref.= JPL#75, data arc: 1835-08-21 to 1994-01-11
      |2: k1=8.0, k2=5.0, phase coef.=0.03;
      |*******************************************************************************
      |
      |
      |*******************************************************************************
      |Ephemeris / API_USER Mon Dec 22 12:22:35 2025 Pasadena, USA      / Horizons    
      |*******************************************************************************
      |Target body name: 1P/Halley                       {source: JPL#75}
      |Center body name: Earth (399)                     {source: DE441}
      |Center-site name: Gemini North Observatory, Maunakea
      |*******************************************************************************
      |Start time      : A.D. 2025-Nov-12 00:00:00.0000 UT      
      |Stop  time      : A.D. 2025-Nov-13 00:00:00.0000 UT      
      |Step-size       : 60 minutes
      |*******************************************************************************
      |Target pole/equ : undefined
      |Target radii    : 5.5 km                                                       
      |Center geodetic : 204.5309, 19.8238126, 4.24672   {E-lon(deg),Lat(deg),Alt(km)}
      |Center cylindric: 204.5309,6006.47419,2150.79851  {E-lon(deg),Dxy(km),Dz(km)}
      |Center pole/equ : ITRF93                          {East-longitude positive}
      |Center radii    : 6378.137, 6378.137, 6356.752 km {Equator_a, b, pole_c}       
      |Target primary  : Sun
      |Vis. interferer : MOON (R_eq= 1737.400) km        {source: DE441}
      |Rel. light bend : Sun                             {source: DE441}
      |Rel. lght bnd GM: 1.3271E+11 km^3/s^2                                          
      |Small-body perts: Yes                             {source: SB441-N16}
      |Atmos refraction: NO (AIRLESS)
      |RA format       : HMS
      |Time format     : CAL 
      |Calendar mode   : Mixed Julian/Gregorian
      |EOP file        : eop.251219.p260317                                           
      |EOP coverage    : DATA-BASED 1962-JAN-20 TO 2025-DEC-19. PREDICTS-> 2026-MAR-16
      |Units conversion: 1 au= 149597870.700 km, c= 299792.458 km/s, 1 day= 86400.0 s 
      |Table cut-offs 1: Elevation (-90.0deg=NO ),Airmass (>38.000=NO), Daylight (NO )
      |Table cut-offs 2: Solar elongation (  0.0,180.0=NO ),Local Hour Angle( 0.0=NO )
      |Table cut-offs 3: RA/DEC angular rate (     0.0=NO )                           
      |*******************************************************************************
      |Initial IAU76/J2000 heliocentric ecliptic osculating elements (au, days, deg.):
      |  EPOCH=  2439875.5 ! 1968-Jan-20.0000000 (TDB)    RMSW= n.a.                  
      |   EC= .9679359956953212   QR= .5748638313743413   TP= 2446469.9736161465      
      |   OM= 59.09894720612437   W= 112.2414314637764    IN= 162.1905300439129       
      |  Equivalent ICRF heliocentric cartesian coordinates (au, au/d):
      |   X=-1.331029360169393E+01  Y= 2.541249958785733E+01  Z= 2.637549316318327E+00
      |  VX= 1.418949944126011E-03 VY=-1.422475975617656E-03 VZ= 4.131321199969281E-05
      |Comet physical (GM= km^3/s^2; RAD= km):                                        
      |   GM= n.a.                RAD= 5.5                                            
      |   M1=  5.5      M2=  13.6     k1=  8.     k2=  5.      PHCOF=  .030           
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au): 
      |   AMRAT=  0.                                      DT=  0.                     
      |   A1= 4.887055233121E-10  A2= 1.554720290005E-10  A3= 0.                      
      | Standard model:                                                               
      |   ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808    
      |*****************************************************************************************************************
      | Date__(UT)__HR:MN:SC.fff     R.A._________(ICRF)_________DEC  dRA*cosD d(DEC)/dt  a-mass mag_ex    T-mag   N-mag
      |*****************************************************************************************************************
      |$$SOE
      | 2025-Nov-12 00:00:00.000 *   08 21 31.081989 +02 30 58.71040  -1.30923  -0.86932    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 01:00:00.000 *   08 21 30.995022 +02 30 57.83898  -1.30119  -0.86793    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 02:00:00.000 *   08 21 30.908476 +02 30 56.96901  -1.29667  -0.86643    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 03:00:00.000 *   08 21 30.822101 +02 30 56.10058  -1.29616  -0.86488    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 04:00:00.000 C   08 21 30.735617 +02 30 55.23372  -1.29992  -0.86330    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 05:00:00.000     08 21 30.648741 +02 30 54.36840  -1.30787  -0.86176    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 06:00:00.000     08 21 30.561202 +02 30 53.50457  -1.31969  -0.86030    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 07:00:00.000     08 21 30.472762 +02 30 52.64211  -1.33476  -0.85896    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 08:00:00.000     08 21 30.383230 +02 30 51.78090  -1.35225  -0.85776    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 09:00:00.000     08 21 30.292478 +02 30 50.92076  -1.37117  -0.85673    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 10:00:00.000     08 21 30.200445 +02 30 50.06151  -1.39041  -0.85589   5.161  0.559   25.573  29.085
      | 2025-Nov-12 11:00:00.000  m  08 21 30.107148 +02 30 49.20298  -1.40888  -0.85522   2.361  0.256   25.573  29.085
      | 2025-Nov-12 12:00:00.000  m  08 21 30.012675 +02 30 48.34501  -1.42549  -0.85471   1.591  0.172   25.573  29.085
      | 2025-Nov-12 13:00:00.000  m  08 21 29.917181 +02 30 47.48743  -1.43931  -0.85434   1.263  0.137   25.573  29.085
      | 2025-Nov-12 14:00:00.000  m  08 21 29.820879 +02 30 46.63014  -1.44961  -0.85408   1.108  0.120   25.572  29.084
      | 2025-Nov-12 15:00:00.000  m  08 21 29.724023 +02 30 45.77307  -1.45587  -0.85387   1.050  0.114   25.572  29.084
      | 2025-Nov-12 16:00:00.000 Nm  08 21 29.626890 +02 30 44.91619  -1.45787  -0.85367   1.066  0.116   25.572  29.084
      | 2025-Nov-12 17:00:00.000 *m  08 21 29.529766 +02 30 44.05952  -1.45567  -0.85344   1.162  0.126   25.572  29.084
      | 2025-Nov-12 18:00:00.000 *m  08 21 29.432919 +02 30 43.20313  -1.44962  -0.85313   1.379  0.149   25.572  29.084
      | 2025-Nov-12 19:00:00.000 *m  08 21 29.336588 +02 30 42.34712  -1.44035  -0.85270   1.845  0.200   25.572  29.084
      | 2025-Nov-12 20:00:00.000 *m  08 21 29.240960 +02 30 41.49164  -1.42868  -0.85212   3.091  0.335   25.572  29.084
      | 2025-Nov-12 21:00:00.000 *m  08 21 29.146163 +02 30 40.63684  -1.41563  -0.85137  10.837  1.175   25.572  29.084
      | 2025-Nov-12 22:00:00.000 *m  08 21 29.052254 +02 30 39.78292  -1.40227  -0.85043    n.a.   n.a.   25.572  29.084
      | 2025-Nov-12 23:00:00.000 *m  08 21 28.959214 +02 30 38.93005  -1.38974  -0.84932    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 00:00:00.000 *   08 21 28.866954 +02 30 38.07840  -1.37909  -0.84804    n.a.   n.a.   25.572  29.084
      |$$EOE
      |*****************************************************************************************************************
      |Column meaning:
      | 
      |TIME
      |
      |  Times PRIOR to 1962 are UT1, a mean-solar time closely related to the
      |prior but now-deprecated GMT. Times AFTER 1962 are in UTC, the current
      |civil or "wall-clock" time-scale. UTC is kept within 0.9 seconds of UT1
      |using integer leap-seconds for 1972 and later years.
      |
      |  Conversion from the internal Barycentric Dynamical Time (TDB) of solar
      |system dynamics to the non-uniform civil UT time-scale requested for output
      |has not been determined for UTC times after the next July or January 1st.
      |Therefore, the last known leap-second is used as a constant over future
      |intervals.
      |
      |  Time tags refer to the UT time-scale conversion from TDB on Earth
      |regardless of observer location within the solar system, although clock
      |rates may differ due to the local gravity field and no analog to "UT"
      |may be defined for that location.
      |
      |  Any 'b' symbol in the 1st-column denotes a B.C. date. First-column blank
      |(" ") denotes an A.D. date.
      | 
      |CALENDAR SYSTEM
      |
      |  Mixed calendar mode was active such that calendar dates after AD 1582-Oct-15
      |(if any) are in the modern Gregorian system. Dates prior to 1582-Oct-5 (if any)
      |are in the Julian calendar system, which is automatically extended for dates
      |prior to its adoption on 45-Jan-1 BC.  The Julian calendar is useful for
      |matching historical dates. The Gregorian calendar more accurately corresponds
      |to the Earth's orbital motion and seasons. A "Gregorian-only" calendar mode is
      |available if such physical events are the primary interest.
      |
      |  NOTE: "n.a." in output means quantity "not available" at the print-time.
      | 
      |SOLAR PRESENCE (OBSERVING SITE)
      |  Time tag is followed by a blank, then a solar-presence symbol:
      |
      |       '*'  Daylight (refracted solar upper-limb on or above apparent horizon)
      |       'C'  Civil twilight/dawn
      |       'N'  Nautical twilight/dawn
      |       'A'  Astronomical twilight/dawn
      |       ' '  Night OR geocentric ephemeris
      |
      |LUNAR PRESENCE (OBSERVING SITE)
      |  The solar-presence symbol is immediately followed by a lunar-presence symbol:
      |
      |       'm'  Refracted upper-limb of Moon on or above apparent horizon
      |       ' '  Refracted upper-limb of Moon below apparent horizon OR geocentric
      |            ephemeris
      | 
      | 'R.A._________(ICRF)_________DEC' =
      |  Astrometric right ascension and declination of the target center with
      |respect to the observing site (coordinate origin) in the reference frame of
      |the planetary ephemeris (ICRF). Compensated for down-leg light-time delay
      |aberration.
      |
      |  Units: RA  in hours-minutes-seconds of time,    HH MM SS.ff{ffff}
      |         DEC in degrees-minutes-seconds of arc,  sDD MN SC.f{ffff}
      | 
      | 'dRA*cosD d(DEC)/dt' =
      |  The angular rate of change in apparent RA and DEC of the target. This is
      |with respect to the non-inertial IAU76/80 Earth true equator and equinox
      |of-date reference frame.  d(RA)/dt is multiplied by the cosine of declination
      |to provide a linear rate in the plane-of-sky. Units: ARCSECONDS PER HOUR
      | 
      | 'a-mass mag_ex' =
      |    RELATIVE optical airmass and visual magnitude extinction. Airmass is the
      |ratio between the absolute optical airmass for the targets' refracted CENTER
      |point to the absolute optical airmass at zenith. Also output is the estimated
      |visual magnitude extinction due to the atmosphere, as seen by the observer.
      |AVAILABLE ONLY FOR TOPOCENTRIC EARTH SITES WHEN THE TARGET IS ABOVE THE
      |HORIZON.  Units: none (airmass) and magnitudes (extinction).
      | 
      | 'T-mag   N-mag' =
      |   Comets' apparent visual total magnitude ("T-mag") and nuclear magnitude
      |("N-mag") using the standard IAU model:
      |
      |   T-mag= M1 + 5*log10(delta) + k1*log10(r)
      |   N-mag= M2 + 5*log10(delta) + k2*log10(r) + phcof*beta
      |
      |   Units: MAGNITUDES
      |
      |Computations by ...
      |
      |    Solar System Dynamics Group, Horizons On-Line Ephemeris System
      |    4800 Oak Grove Drive, Jet Propulsion Laboratory
      |    Pasadena, CA  91109   USA
      |
      |    General site: https://ssd.jpl.nasa.gov/
      |    Mailing list: https://ssd.jpl.nasa.gov/email_list.html
      |    System news : https://ssd.jpl.nasa.gov/horizons/news.html
      |    User Guide  : https://ssd.jpl.nasa.gov/horizons/manual.html
      |    Connect     : browser        https://ssd.jpl.nasa.gov/horizons/app.html#/x
      |                  API            https://ssd-api.jpl.nasa.gov/doc/horizons.html
      |                  command-line   telnet ssd.jpl.nasa.gov 6775
      |                  e-mail/batch   https://ssd.jpl.nasa.gov/ftp/ssd/horizons_batch.txt
      |                  scripts        https://ssd.jpl.nasa.gov/ftp/ssd/SCRIPTS
      |    Author      : Jon.D.Giorgini@jpl.nasa.gov
      |
      |*****************************************************************************************************************
      """.stripMargin,
      Set(("format", "text"), ("MAKE_EPHEM", "YES"), ("CENTER", "I11"), ("COMMAND", "'NAME=1P;CAP'"), ("START_TIME", "'2025-Nov-12 00:00:00.000'"), ("STOP_TIME", "'2025-Nov-13 00:00:00.000'"), ("STEP_SIZE", "60m"), ("extra_prec", "YES"), ("time_digits", "FRACSEC"), ("QUANTITIES", "'1,3,8,9'")) ->
      """|API VERSION: 1.2
      |API SOURCE: NASA/JPL Horizons API
      |
      |*******************************************************************************
      |JPL/HORIZONS                      1P/Halley                2025-Dec-22 12:23:45
      |Rec #:90000030        Soln.date: 2025-Nov-21_15:57:34   # obs: 8518 (1835-1994)
      | 
      |IAU76/J2000 helio. ecliptic osc. elements (au, days, deg., period=Julian yrs):
      | 
      |  EPOCH=  2439875.5 ! 1968-Jan-20.0000000 (TDB)    RMSW= n.a.
      |   EC= .9679359956953212   QR= .5748638313743413   TP= 2446469.9736161465
      |   OM= 59.09894720612437   W= 112.2414314637764    IN= 162.1905300439129
      |   A= 17.92863504856929    MA= 274.3823371364693   ADIST= 35.28240626576424
      |   PER= 75.915252807404    N= .012983244           ANGMOM= .018296559
      |   DAN= 1.78543            DDN= .82795             L= 305.8544912
      |   B= 16.4450919           MOID= .0745097          TP= 1986-Feb-08.4736161465
      | 
      |Comet physical (GM= km^3/s^2; RAD= km):
      |   GM= n.a.                RAD= 5.5
      |   M1=  5.5      M2=  13.6     k1=  8.     k2=  5.      PHCOF=  .030
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au):
      |   AMRAT=  0.                                      DT=  0.
      |   A1= 4.887055233121E-10  A2= 1.554720290005E-10  A3= 0.
      | Standard model:
      |   ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808
      | 
      |COMET comments 
      |1: soln ref.= JPL#75, data arc: 1835-08-21 to 1994-01-11
      |2: k1=8.0, k2=5.0, phase coef.=0.03;
      |*******************************************************************************
      |
      |
      |*******************************************************************************
      |Ephemeris / API_USER Mon Dec 22 12:23:46 2025 Pasadena, USA      / Horizons    
      |*******************************************************************************
      |Target body name: 1P/Halley                       {source: JPL#75}
      |Center body name: Earth (399)                     {source: DE441}
      |Center-site name: Gemini South Obs., Cerro Pachon
      |*******************************************************************************
      |Start time      : A.D. 2025-Nov-12 00:00:00.0000 UT      
      |Stop  time      : A.D. 2025-Nov-13 00:00:00.0000 UT      
      |Step-size       : 60 minutes
      |*******************************************************************************
      |Target pole/equ : undefined
      |Target radii    : 5.5 km                                                       
      |Center geodetic : 289.2634, -30.2406227, 2.71233  {E-lon(deg),Lat(deg),Alt(km)}
      |Center cylindric: 289.2634,5517.21435,-3194.81213 {E-lon(deg),Dxy(km),Dz(km)}
      |Center pole/equ : ITRF93                          {East-longitude positive}
      |Center radii    : 6378.137, 6378.137, 6356.752 km {Equator_a, b, pole_c}       
      |Target primary  : Sun
      |Vis. interferer : MOON (R_eq= 1737.400) km        {source: DE441}
      |Rel. light bend : Sun                             {source: DE441}
      |Rel. lght bnd GM: 1.3271E+11 km^3/s^2                                          
      |Small-body perts: Yes                             {source: SB441-N16}
      |Atmos refraction: NO (AIRLESS)
      |RA format       : HMS
      |Time format     : CAL 
      |Calendar mode   : Mixed Julian/Gregorian
      |EOP file        : eop.251219.p260317                                           
      |EOP coverage    : DATA-BASED 1962-JAN-20 TO 2025-DEC-19. PREDICTS-> 2026-MAR-16
      |Units conversion: 1 au= 149597870.700 km, c= 299792.458 km/s, 1 day= 86400.0 s 
      |Table cut-offs 1: Elevation (-90.0deg=NO ),Airmass (>38.000=NO), Daylight (NO )
      |Table cut-offs 2: Solar elongation (  0.0,180.0=NO ),Local Hour Angle( 0.0=NO )
      |Table cut-offs 3: RA/DEC angular rate (     0.0=NO )                           
      |*******************************************************************************
      |Initial IAU76/J2000 heliocentric ecliptic osculating elements (au, days, deg.):
      |  EPOCH=  2439875.5 ! 1968-Jan-20.0000000 (TDB)    RMSW= n.a.                  
      |   EC= .9679359956953212   QR= .5748638313743413   TP= 2446469.9736161465      
      |   OM= 59.09894720612437   W= 112.2414314637764    IN= 162.1905300439129       
      |  Equivalent ICRF heliocentric cartesian coordinates (au, au/d):
      |   X=-1.331029360169393E+01  Y= 2.541249958785733E+01  Z= 2.637549316318327E+00
      |  VX= 1.418949944126011E-03 VY=-1.422475975617656E-03 VZ= 4.131321199969281E-05
      |Comet physical (GM= km^3/s^2; RAD= km):                                        
      |   GM= n.a.                RAD= 5.5                                            
      |   M1=  5.5      M2=  13.6     k1=  8.     k2=  5.      PHCOF=  .030           
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au): 
      |   AMRAT=  0.                                      DT=  0.                     
      |   A1= 4.887055233121E-10  A2= 1.554720290005E-10  A3= 0.                      
      | Standard model:                                                               
      |   ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808    
      |*****************************************************************************************************************
      | Date__(UT)__HR:MN:SC.fff     R.A._________(ICRF)_________DEC  dRA*cosD d(DEC)/dt  a-mass mag_ex    T-mag   N-mag
      |*****************************************************************************************************************
      |$$SOE
      | 2025-Nov-12 00:00:00.000 N   08 21 31.102403 +02 30 58.92115  -1.30250  -0.86590    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 01:00:00.000     08 21 31.015178 +02 30 58.05313  -1.31557  -0.86455    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 02:00:00.000     08 21 30.926992 +02 30 57.18637  -1.33116  -0.86333    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 03:00:00.000     08 21 30.837705 +02 30 56.32073  -1.34840  -0.86226    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 04:00:00.000     08 21 30.747238 +02 30 55.45603  -1.36633  -0.86135  13.925  1.936   25.573  29.085
      | 2025-Nov-12 05:00:00.000     08 21 30.655581 +02 30 54.59213  -1.38391  -0.86061   3.574  0.497   25.573  29.085
      | 2025-Nov-12 06:00:00.000  m  08 21 30.562790 +02 30 53.72885  -1.40014  -0.86003   2.093  0.291   25.573  29.085
      | 2025-Nov-12 07:00:00.000  m  08 21 30.468987 +02 30 52.86605  -1.41411  -0.85958   1.555  0.216   25.573  29.085
      | 2025-Nov-12 08:00:00.000  m  08 21 30.374348 +02 30 52.00362  -1.42506  -0.85923   1.310  0.182   25.573  29.085
      | 2025-Nov-12 09:00:00.000 Nm  08 21 30.279095 +02 30 51.14148  -1.43245  -0.85896   1.205  0.167   25.573  29.085
      | 2025-Nov-12 10:00:00.000 *m  08 21 30.183477 +02 30 50.27958  -1.43597  -0.85871   1.192  0.166   25.573  29.085
      | 2025-Nov-12 11:00:00.000 *m  08 21 30.087756 +02 30 49.41794  -1.43558  -0.85845   1.268  0.176   25.573  29.085
      | 2025-Nov-12 12:00:00.000 *m  08 21 29.992184 +02 30 48.55659  -1.43152  -0.85813   1.464  0.204   25.573  29.085
      | 2025-Nov-12 13:00:00.000 *m  08 21 29.896995 +02 30 47.69561  -1.42425  -0.85771   1.888  0.262   25.573  29.085
      | 2025-Nov-12 14:00:00.000 *m  08 21 29.802378 +02 30 46.83514  -1.41448  -0.85716   2.943  0.409   25.572  29.084
      | 2025-Nov-12 15:00:00.000 *m  08 21 29.708473 +02 30 45.97532  -1.40308  -0.85646   7.689  1.069   25.572  29.084
      | 2025-Nov-12 16:00:00.000 *m  08 21 29.615355 +02 30 45.11630  -1.39104  -0.85559    n.a.   n.a.   25.572  29.084
      | 2025-Nov-12 17:00:00.000 *   08 21 29.523034 +02 30 44.25826  -1.37938  -0.85456    n.a.   n.a.   25.572  29.084
      | 2025-Nov-12 18:00:00.000 *   08 21 29.431450 +02 30 43.40135  -1.36910  -0.85337    n.a.   n.a.   25.572  29.084
      | 2025-Nov-12 19:00:00.000 *   08 21 29.340480 +02 30 42.54572  -1.36111  -0.85204    n.a.   n.a.   25.572  29.084
      | 2025-Nov-12 20:00:00.000 *   08 21 29.249946 +02 30 41.69148  -1.35616  -0.85061    n.a.   n.a.   25.572  29.084
      | 2025-Nov-12 21:00:00.000 *   08 21 29.159625 +02 30 40.83872  -1.35478  -0.84911    n.a.   n.a.   25.572  29.084
      | 2025-Nov-12 22:00:00.000 *   08 21 29.069267 +02 30 39.98747  -1.35728  -0.84759    n.a.   n.a.   25.572  29.084
      | 2025-Nov-12 23:00:00.000 *   08 21 28.978611 +02 30 39.13774  -1.36369  -0.84608    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 00:00:00.000 N   08 21 28.887403 +02 30 38.28948  -1.37376  -0.84462    n.a.   n.a.   25.572  29.084
      |$$EOE
      |*****************************************************************************************************************
      |Column meaning:
      | 
      |TIME
      |
      |  Times PRIOR to 1962 are UT1, a mean-solar time closely related to the
      |prior but now-deprecated GMT. Times AFTER 1962 are in UTC, the current
      |civil or "wall-clock" time-scale. UTC is kept within 0.9 seconds of UT1
      |using integer leap-seconds for 1972 and later years.
      |
      |  Conversion from the internal Barycentric Dynamical Time (TDB) of solar
      |system dynamics to the non-uniform civil UT time-scale requested for output
      |has not been determined for UTC times after the next July or January 1st.
      |Therefore, the last known leap-second is used as a constant over future
      |intervals.
      |
      |  Time tags refer to the UT time-scale conversion from TDB on Earth
      |regardless of observer location within the solar system, although clock
      |rates may differ due to the local gravity field and no analog to "UT"
      |may be defined for that location.
      |
      |  Any 'b' symbol in the 1st-column denotes a B.C. date. First-column blank
      |(" ") denotes an A.D. date.
      | 
      |CALENDAR SYSTEM
      |
      |  Mixed calendar mode was active such that calendar dates after AD 1582-Oct-15
      |(if any) are in the modern Gregorian system. Dates prior to 1582-Oct-5 (if any)
      |are in the Julian calendar system, which is automatically extended for dates
      |prior to its adoption on 45-Jan-1 BC.  The Julian calendar is useful for
      |matching historical dates. The Gregorian calendar more accurately corresponds
      |to the Earth's orbital motion and seasons. A "Gregorian-only" calendar mode is
      |available if such physical events are the primary interest.
      |
      |  NOTE: "n.a." in output means quantity "not available" at the print-time.
      | 
      |SOLAR PRESENCE (OBSERVING SITE)
      |  Time tag is followed by a blank, then a solar-presence symbol:
      |
      |       '*'  Daylight (refracted solar upper-limb on or above apparent horizon)
      |       'C'  Civil twilight/dawn
      |       'N'  Nautical twilight/dawn
      |       'A'  Astronomical twilight/dawn
      |       ' '  Night OR geocentric ephemeris
      |
      |LUNAR PRESENCE (OBSERVING SITE)
      |  The solar-presence symbol is immediately followed by a lunar-presence symbol:
      |
      |       'm'  Refracted upper-limb of Moon on or above apparent horizon
      |       ' '  Refracted upper-limb of Moon below apparent horizon OR geocentric
      |            ephemeris
      | 
      | 'R.A._________(ICRF)_________DEC' =
      |  Astrometric right ascension and declination of the target center with
      |respect to the observing site (coordinate origin) in the reference frame of
      |the planetary ephemeris (ICRF). Compensated for down-leg light-time delay
      |aberration.
      |
      |  Units: RA  in hours-minutes-seconds of time,    HH MM SS.ff{ffff}
      |         DEC in degrees-minutes-seconds of arc,  sDD MN SC.f{ffff}
      | 
      | 'dRA*cosD d(DEC)/dt' =
      |  The angular rate of change in apparent RA and DEC of the target. This is
      |with respect to the non-inertial IAU76/80 Earth true equator and equinox
      |of-date reference frame.  d(RA)/dt is multiplied by the cosine of declination
      |to provide a linear rate in the plane-of-sky. Units: ARCSECONDS PER HOUR
      | 
      | 'a-mass mag_ex' =
      |    RELATIVE optical airmass and visual magnitude extinction. Airmass is the
      |ratio between the absolute optical airmass for the targets' refracted CENTER
      |point to the absolute optical airmass at zenith. Also output is the estimated
      |visual magnitude extinction due to the atmosphere, as seen by the observer.
      |AVAILABLE ONLY FOR TOPOCENTRIC EARTH SITES WHEN THE TARGET IS ABOVE THE
      |HORIZON.  Units: none (airmass) and magnitudes (extinction).
      | 
      | 'T-mag   N-mag' =
      |   Comets' apparent visual total magnitude ("T-mag") and nuclear magnitude
      |("N-mag") using the standard IAU model:
      |
      |   T-mag= M1 + 5*log10(delta) + k1*log10(r)
      |   N-mag= M2 + 5*log10(delta) + k2*log10(r) + phcof*beta
      |
      |   Units: MAGNITUDES
      |
      |Computations by ...
      |
      |    Solar System Dynamics Group, Horizons On-Line Ephemeris System
      |    4800 Oak Grove Drive, Jet Propulsion Laboratory
      |    Pasadena, CA  91109   USA
      |
      |    General site: https://ssd.jpl.nasa.gov/
      |    Mailing list: https://ssd.jpl.nasa.gov/email_list.html
      |    System news : https://ssd.jpl.nasa.gov/horizons/news.html
      |    User Guide  : https://ssd.jpl.nasa.gov/horizons/manual.html
      |    Connect     : browser        https://ssd.jpl.nasa.gov/horizons/app.html#/x
      |                  API            https://ssd-api.jpl.nasa.gov/doc/horizons.html
      |                  command-line   telnet ssd.jpl.nasa.gov 6775
      |                  e-mail/batch   https://ssd.jpl.nasa.gov/ftp/ssd/horizons_batch.txt
      |                  scripts        https://ssd.jpl.nasa.gov/ftp/ssd/SCRIPTS
      |    Author      : Jon.D.Giorgini@jpl.nasa.gov
      |
      |*****************************************************************************************************************
      """.stripMargin,
      Set(("format", "text"), ("MAKE_EPHEM", "YES"), ("CENTER", "T15"), ("COMMAND", "'NAME=1P;CAP'"), ("START_TIME", "'2025-Nov-12 00:00:00.000'"), ("STOP_TIME", "'2025-Nov-14 00:00:00.000'"), ("STEP_SIZE", "180m"), ("extra_prec", "YES"), ("time_digits", "FRACSEC"), ("QUANTITIES", "'1,3,8,9'")) ->
      """|API VERSION: 1.2
      |API SOURCE: NASA/JPL Horizons API
      |
      |*******************************************************************************
      |JPL/HORIZONS                      1P/Halley                2025-Dec-22 12:55:27
      |Rec #:90000030        Soln.date: 2025-Nov-21_15:57:34   # obs: 8518 (1835-1994)
      | 
      |IAU76/J2000 helio. ecliptic osc. elements (au, days, deg., period=Julian yrs):
      | 
      |  EPOCH=  2439875.5 ! 1968-Jan-20.0000000 (TDB)    RMSW= n.a.
      |   EC= .9679359956953212   QR= .5748638313743413   TP= 2446469.9736161465
      |   OM= 59.09894720612437   W= 112.2414314637764    IN= 162.1905300439129
      |   A= 17.92863504856929    MA= 274.3823371364693   ADIST= 35.28240626576424
      |   PER= 75.915252807404    N= .012983244           ANGMOM= .018296559
      |   DAN= 1.78543            DDN= .82795             L= 305.8544912
      |   B= 16.4450919           MOID= .0745097          TP= 1986-Feb-08.4736161465
      | 
      |Comet physical (GM= km^3/s^2; RAD= km):
      |   GM= n.a.                RAD= 5.5
      |   M1=  5.5      M2=  13.6     k1=  8.     k2=  5.      PHCOF=  .030
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au):
      |   AMRAT=  0.                                      DT=  0.
      |   A1= 4.887055233121E-10  A2= 1.554720290005E-10  A3= 0.
      | Standard model:
      |   ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808
      | 
      |COMET comments 
      |1: soln ref.= JPL#75, data arc: 1835-08-21 to 1994-01-11
      |2: k1=8.0, k2=5.0, phase coef.=0.03;
      |*******************************************************************************
      |
      |
      |*******************************************************************************
      |Ephemeris / API_USER Mon Dec 22 12:55:28 2025 Pasadena, USA      / Horizons    
      |*******************************************************************************
      |Target body name: 1P/Halley                       {source: JPL#75}
      |Center body name: Earth (399)                     {source: DE441}
      |Center-site name: Gemini North Observatory, Maunakea
      |*******************************************************************************
      |Start time      : A.D. 2025-Nov-12 00:00:00.0000 UT      
      |Stop  time      : A.D. 2025-Nov-14 00:00:00.0000 UT      
      |Step-size       : 180 minutes
      |*******************************************************************************
      |Target pole/equ : undefined
      |Target radii    : 5.5 km                                                       
      |Center geodetic : 204.5309, 19.8238126, 4.24672   {E-lon(deg),Lat(deg),Alt(km)}
      |Center cylindric: 204.5309,6006.47419,2150.79851  {E-lon(deg),Dxy(km),Dz(km)}
      |Center pole/equ : ITRF93                          {East-longitude positive}
      |Center radii    : 6378.137, 6378.137, 6356.752 km {Equator_a, b, pole_c}       
      |Target primary  : Sun
      |Vis. interferer : MOON (R_eq= 1737.400) km        {source: DE441}
      |Rel. light bend : Sun                             {source: DE441}
      |Rel. lght bnd GM: 1.3271E+11 km^3/s^2                                          
      |Small-body perts: Yes                             {source: SB441-N16}
      |Atmos refraction: NO (AIRLESS)
      |RA format       : HMS
      |Time format     : CAL 
      |Calendar mode   : Mixed Julian/Gregorian
      |EOP file        : eop.251219.p260317                                           
      |EOP coverage    : DATA-BASED 1962-JAN-20 TO 2025-DEC-19. PREDICTS-> 2026-MAR-16
      |Units conversion: 1 au= 149597870.700 km, c= 299792.458 km/s, 1 day= 86400.0 s 
      |Table cut-offs 1: Elevation (-90.0deg=NO ),Airmass (>38.000=NO), Daylight (NO )
      |Table cut-offs 2: Solar elongation (  0.0,180.0=NO ),Local Hour Angle( 0.0=NO )
      |Table cut-offs 3: RA/DEC angular rate (     0.0=NO )                           
      |*******************************************************************************
      |Initial IAU76/J2000 heliocentric ecliptic osculating elements (au, days, deg.):
      |  EPOCH=  2439875.5 ! 1968-Jan-20.0000000 (TDB)    RMSW= n.a.                  
      |   EC= .9679359956953212   QR= .5748638313743413   TP= 2446469.9736161465      
      |   OM= 59.09894720612437   W= 112.2414314637764    IN= 162.1905300439129       
      |  Equivalent ICRF heliocentric cartesian coordinates (au, au/d):
      |   X=-1.331029360169393E+01  Y= 2.541249958785733E+01  Z= 2.637549316318327E+00
      |  VX= 1.418949944126011E-03 VY=-1.422475975617656E-03 VZ= 4.131321199969281E-05
      |Comet physical (GM= km^3/s^2; RAD= km):                                        
      |   GM= n.a.                RAD= 5.5                                            
      |   M1=  5.5      M2=  13.6     k1=  8.     k2=  5.      PHCOF=  .030           
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au): 
      |   AMRAT=  0.                                      DT=  0.                     
      |   A1= 4.887055233121E-10  A2= 1.554720290005E-10  A3= 0.                      
      | Standard model:                                                               
      |   ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808    
      |*****************************************************************************************************************
      | Date__(UT)__HR:MN:SC.fff     R.A._________(ICRF)_________DEC  dRA*cosD d(DEC)/dt  a-mass mag_ex    T-mag   N-mag
      |*****************************************************************************************************************
      |$$SOE
      | 2025-Nov-12 00:00:00.000 *   08 21 31.081989 +02 30 58.71040  -1.30923  -0.86932    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 03:00:00.000 *   08 21 30.822101 +02 30 56.10058  -1.29616  -0.86488    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 06:00:00.000     08 21 30.561202 +02 30 53.50457  -1.31969  -0.86030    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 09:00:00.000     08 21 30.292478 +02 30 50.92076  -1.37117  -0.85673    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 12:00:00.000  m  08 21 30.012675 +02 30 48.34501  -1.42549  -0.85471   1.591  0.172   25.573  29.085
      | 2025-Nov-12 15:00:00.000  m  08 21 29.724023 +02 30 45.77307  -1.45587  -0.85387   1.050  0.114   25.572  29.084
      | 2025-Nov-12 18:00:00.000 *m  08 21 29.432919 +02 30 43.20313  -1.44962  -0.85313   1.379  0.149   25.572  29.084
      | 2025-Nov-12 21:00:00.000 *m  08 21 29.146163 +02 30 40.63684  -1.41563  -0.85137  10.837  1.175   25.572  29.084
      | 2025-Nov-13 00:00:00.000 *   08 21 28.866954 +02 30 38.07840  -1.37909  -0.84804    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 03:00:00.000 *   08 21 28.593003 +02 30 35.53207  -1.36674  -0.84354    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 06:00:00.000     08 21 28.317886 +02 30 32.99967  -1.39104  -0.83894    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 09:00:00.000     08 21 28.034821 +02 30 30.47953  -1.44289  -0.83536    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 12:00:00.000  m  08 21 27.740668 +02 30 27.96747  -1.49694  -0.83333   1.561  0.169   25.572  29.083
      | 2025-Nov-13 15:00:00.000  m  08 21 27.437774 +02 30 25.45931  -1.52657  -0.83245   1.049  0.114   25.571  29.083
      | 2025-Nov-13 18:00:00.000 *m  08 21 27.132594 +02 30 22.95328  -1.51950  -0.83165   1.399  0.152   25.571  29.083
      | 2025-Nov-13 21:00:00.000 *m  08 21 26.831892 +02 30 20.45112  -1.48509  -0.82981  12.809  1.388   25.571  29.083
      | 2025-Nov-14 00:00:00.000 *m  08 21 26.538755 +02 30 17.95704  -1.44879  -0.82641    n.a.   n.a.   25.571  29.083
      |$$EOE
      |*****************************************************************************************************************
      |Column meaning:
      | 
      |TIME
      |
      |  Times PRIOR to 1962 are UT1, a mean-solar time closely related to the
      |prior but now-deprecated GMT. Times AFTER 1962 are in UTC, the current
      |civil or "wall-clock" time-scale. UTC is kept within 0.9 seconds of UT1
      |using integer leap-seconds for 1972 and later years.
      |
      |  Conversion from the internal Barycentric Dynamical Time (TDB) of solar
      |system dynamics to the non-uniform civil UT time-scale requested for output
      |has not been determined for UTC times after the next July or January 1st.
      |Therefore, the last known leap-second is used as a constant over future
      |intervals.
      |
      |  Time tags refer to the UT time-scale conversion from TDB on Earth
      |regardless of observer location within the solar system, although clock
      |rates may differ due to the local gravity field and no analog to "UT"
      |may be defined for that location.
      |
      |  Any 'b' symbol in the 1st-column denotes a B.C. date. First-column blank
      |(" ") denotes an A.D. date.
      | 
      |CALENDAR SYSTEM
      |
      |  Mixed calendar mode was active such that calendar dates after AD 1582-Oct-15
      |(if any) are in the modern Gregorian system. Dates prior to 1582-Oct-5 (if any)
      |are in the Julian calendar system, which is automatically extended for dates
      |prior to its adoption on 45-Jan-1 BC.  The Julian calendar is useful for
      |matching historical dates. The Gregorian calendar more accurately corresponds
      |to the Earth's orbital motion and seasons. A "Gregorian-only" calendar mode is
      |available if such physical events are the primary interest.
      |
      |  NOTE: "n.a." in output means quantity "not available" at the print-time.
      | 
      |SOLAR PRESENCE (OBSERVING SITE)
      |  Time tag is followed by a blank, then a solar-presence symbol:
      |
      |       '*'  Daylight (refracted solar upper-limb on or above apparent horizon)
      |       'C'  Civil twilight/dawn
      |       'N'  Nautical twilight/dawn
      |       'A'  Astronomical twilight/dawn
      |       ' '  Night OR geocentric ephemeris
      |
      |LUNAR PRESENCE (OBSERVING SITE)
      |  The solar-presence symbol is immediately followed by a lunar-presence symbol:
      |
      |       'm'  Refracted upper-limb of Moon on or above apparent horizon
      |       ' '  Refracted upper-limb of Moon below apparent horizon OR geocentric
      |            ephemeris
      | 
      | 'R.A._________(ICRF)_________DEC' =
      |  Astrometric right ascension and declination of the target center with
      |respect to the observing site (coordinate origin) in the reference frame of
      |the planetary ephemeris (ICRF). Compensated for down-leg light-time delay
      |aberration.
      |
      |  Units: RA  in hours-minutes-seconds of time,    HH MM SS.ff{ffff}
      |         DEC in degrees-minutes-seconds of arc,  sDD MN SC.f{ffff}
      | 
      | 'dRA*cosD d(DEC)/dt' =
      |  The angular rate of change in apparent RA and DEC of the target. This is
      |with respect to the non-inertial IAU76/80 Earth true equator and equinox
      |of-date reference frame.  d(RA)/dt is multiplied by the cosine of declination
      |to provide a linear rate in the plane-of-sky. Units: ARCSECONDS PER HOUR
      | 
      | 'a-mass mag_ex' =
      |    RELATIVE optical airmass and visual magnitude extinction. Airmass is the
      |ratio between the absolute optical airmass for the targets' refracted CENTER
      |point to the absolute optical airmass at zenith. Also output is the estimated
      |visual magnitude extinction due to the atmosphere, as seen by the observer.
      |AVAILABLE ONLY FOR TOPOCENTRIC EARTH SITES WHEN THE TARGET IS ABOVE THE
      |HORIZON.  Units: none (airmass) and magnitudes (extinction).
      | 
      | 'T-mag   N-mag' =
      |   Comets' apparent visual total magnitude ("T-mag") and nuclear magnitude
      |("N-mag") using the standard IAU model:
      |
      |   T-mag= M1 + 5*log10(delta) + k1*log10(r)
      |   N-mag= M2 + 5*log10(delta) + k2*log10(r) + phcof*beta
      |
      |   Units: MAGNITUDES
      |
      |Computations by ...
      |
      |    Solar System Dynamics Group, Horizons On-Line Ephemeris System
      |    4800 Oak Grove Drive, Jet Propulsion Laboratory
      |    Pasadena, CA  91109   USA
      |
      |    General site: https://ssd.jpl.nasa.gov/
      |    Mailing list: https://ssd.jpl.nasa.gov/email_list.html
      |    System news : https://ssd.jpl.nasa.gov/horizons/news.html
      |    User Guide  : https://ssd.jpl.nasa.gov/horizons/manual.html
      |    Connect     : browser        https://ssd.jpl.nasa.gov/horizons/app.html#/x
      |                  API            https://ssd-api.jpl.nasa.gov/doc/horizons.html
      |                  command-line   telnet ssd.jpl.nasa.gov 6775
      |                  e-mail/batch   https://ssd.jpl.nasa.gov/ftp/ssd/horizons_batch.txt
      |                  scripts        https://ssd.jpl.nasa.gov/ftp/ssd/SCRIPTS
      |    Author      : Jon.D.Giorgini@jpl.nasa.gov
      |
      |*****************************************************************************************************************
      """.stripMargin,
      Set(("format", "text"), ("MAKE_EPHEM", "YES"), ("CENTER", "I11"), ("COMMAND", "'NAME=1P;CAP'"), ("START_TIME", "'2025-Nov-12 00:00:00.000'"), ("STOP_TIME", "'2025-Nov-14 00:00:00.000'"), ("STEP_SIZE", "180m"), ("extra_prec", "YES"), ("time_digits", "FRACSEC"), ("QUANTITIES", "'1,3,8,9'")) ->
      """|API VERSION: 1.2
      |API SOURCE: NASA/JPL Horizons API
      |
      |*******************************************************************************
      |JPL/HORIZONS                      1P/Halley                2025-Dec-22 12:57:22
      |Rec #:90000030        Soln.date: 2025-Nov-21_15:57:34   # obs: 8518 (1835-1994)
      | 
      |IAU76/J2000 helio. ecliptic osc. elements (au, days, deg., period=Julian yrs):
      | 
      |  EPOCH=  2439875.5 ! 1968-Jan-20.0000000 (TDB)    RMSW= n.a.
      |   EC= .9679359956953212   QR= .5748638313743413   TP= 2446469.9736161465
      |   OM= 59.09894720612437   W= 112.2414314637764    IN= 162.1905300439129
      |   A= 17.92863504856929    MA= 274.3823371364693   ADIST= 35.28240626576424
      |   PER= 75.915252807404    N= .012983244           ANGMOM= .018296559
      |   DAN= 1.78543            DDN= .82795             L= 305.8544912
      |   B= 16.4450919           MOID= .0745097          TP= 1986-Feb-08.4736161465
      | 
      |Comet physical (GM= km^3/s^2; RAD= km):
      |   GM= n.a.                RAD= 5.5
      |   M1=  5.5      M2=  13.6     k1=  8.     k2=  5.      PHCOF=  .030
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au):
      |   AMRAT=  0.                                      DT=  0.
      |   A1= 4.887055233121E-10  A2= 1.554720290005E-10  A3= 0.
      | Standard model:
      |   ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808
      | 
      |COMET comments 
      |1: soln ref.= JPL#75, data arc: 1835-08-21 to 1994-01-11
      |2: k1=8.0, k2=5.0, phase coef.=0.03;
      |*******************************************************************************
      |
      |
      |*******************************************************************************
      |Ephemeris / API_USER Mon Dec 22 12:57:22 2025 Pasadena, USA      / Horizons    
      |*******************************************************************************
      |Target body name: 1P/Halley                       {source: JPL#75}
      |Center body name: Earth (399)                     {source: DE441}
      |Center-site name: Gemini South Obs., Cerro Pachon
      |*******************************************************************************
      |Start time      : A.D. 2025-Nov-12 00:00:00.0000 UT      
      |Stop  time      : A.D. 2025-Nov-14 00:00:00.0000 UT      
      |Step-size       : 180 minutes
      |*******************************************************************************
      |Target pole/equ : undefined
      |Target radii    : 5.5 km                                                       
      |Center geodetic : 289.2634, -30.2406227, 2.71233  {E-lon(deg),Lat(deg),Alt(km)}
      |Center cylindric: 289.2634,5517.21435,-3194.81213 {E-lon(deg),Dxy(km),Dz(km)}
      |Center pole/equ : ITRF93                          {East-longitude positive}
      |Center radii    : 6378.137, 6378.137, 6356.752 km {Equator_a, b, pole_c}       
      |Target primary  : Sun
      |Vis. interferer : MOON (R_eq= 1737.400) km        {source: DE441}
      |Rel. light bend : Sun                             {source: DE441}
      |Rel. lght bnd GM: 1.3271E+11 km^3/s^2                                          
      |Small-body perts: Yes                             {source: SB441-N16}
      |Atmos refraction: NO (AIRLESS)
      |RA format       : HMS
      |Time format     : CAL 
      |Calendar mode   : Mixed Julian/Gregorian
      |EOP file        : eop.251219.p260317                                           
      |EOP coverage    : DATA-BASED 1962-JAN-20 TO 2025-DEC-19. PREDICTS-> 2026-MAR-16
      |Units conversion: 1 au= 149597870.700 km, c= 299792.458 km/s, 1 day= 86400.0 s 
      |Table cut-offs 1: Elevation (-90.0deg=NO ),Airmass (>38.000=NO), Daylight (NO )
      |Table cut-offs 2: Solar elongation (  0.0,180.0=NO ),Local Hour Angle( 0.0=NO )
      |Table cut-offs 3: RA/DEC angular rate (     0.0=NO )                           
      |*******************************************************************************
      |Initial IAU76/J2000 heliocentric ecliptic osculating elements (au, days, deg.):
      |  EPOCH=  2439875.5 ! 1968-Jan-20.0000000 (TDB)    RMSW= n.a.                  
      |   EC= .9679359956953212   QR= .5748638313743413   TP= 2446469.9736161465      
      |   OM= 59.09894720612437   W= 112.2414314637764    IN= 162.1905300439129       
      |  Equivalent ICRF heliocentric cartesian coordinates (au, au/d):
      |   X=-1.331029360169393E+01  Y= 2.541249958785733E+01  Z= 2.637549316318327E+00
      |  VX= 1.418949944126011E-03 VY=-1.422475975617656E-03 VZ= 4.131321199969281E-05
      |Comet physical (GM= km^3/s^2; RAD= km):                                        
      |   GM= n.a.                RAD= 5.5                                            
      |   M1=  5.5      M2=  13.6     k1=  8.     k2=  5.      PHCOF=  .030           
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au): 
      |   AMRAT=  0.                                      DT=  0.                     
      |   A1= 4.887055233121E-10  A2= 1.554720290005E-10  A3= 0.                      
      | Standard model:                                                               
      |   ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808    
      |*****************************************************************************************************************
      | Date__(UT)__HR:MN:SC.fff     R.A._________(ICRF)_________DEC  dRA*cosD d(DEC)/dt  a-mass mag_ex    T-mag   N-mag
      |*****************************************************************************************************************
      |$$SOE
      | 2025-Nov-12 00:00:00.000 N   08 21 31.102403 +02 30 58.92115  -1.30250  -0.86590    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 03:00:00.000     08 21 30.837705 +02 30 56.32073  -1.34840  -0.86226    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 06:00:00.000  m  08 21 30.562790 +02 30 53.72885  -1.40014  -0.86003   2.093  0.291   25.573  29.085
      | 2025-Nov-12 09:00:00.000 Nm  08 21 30.279095 +02 30 51.14148  -1.43245  -0.85896   1.205  0.167   25.573  29.085
      | 2025-Nov-12 12:00:00.000 *m  08 21 29.992184 +02 30 48.55659  -1.43152  -0.85813   1.464  0.204   25.573  29.085
      | 2025-Nov-12 15:00:00.000 *m  08 21 29.708473 +02 30 45.97532  -1.40308  -0.85646   7.689  1.069   25.572  29.084
      | 2025-Nov-12 18:00:00.000 *   08 21 29.431450 +02 30 43.40135  -1.36910  -0.85337    n.a.   n.a.   25.572  29.084
      | 2025-Nov-12 21:00:00.000 *   08 21 29.159625 +02 30 40.83872  -1.35478  -0.84911    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 00:00:00.000 N   08 21 28.887403 +02 30 38.28948  -1.37376  -0.84462    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 03:00:00.000     08 21 28.608381 +02 30 35.75246  -1.42007  -0.84097    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 06:00:00.000  m  08 21 28.319119 +02 30 33.22403  -1.47163  -0.83872   2.041  0.284   25.572  29.084
      | 2025-Nov-13 09:00:00.000 Nm  08 21 28.021164 +02 30 30.70017  -1.50328  -0.83762   1.201  0.167   25.572  29.083
      | 2025-Nov-13 12:00:00.000 *m  08 21 27.720145 +02 30 28.17893  -1.50157  -0.83674   1.483  0.206   25.572  29.083
      | 2025-Nov-13 15:00:00.000 *m  08 21 27.422452 +02 30 25.66151  -1.47270  -0.83499   8.591  1.194   25.571  29.083
      | 2025-Nov-13 18:00:00.000 *   08 21 27.131480 +02 30 23.15162  -1.43885  -0.83183    n.a.   n.a.   25.571  29.083
      | 2025-Nov-13 21:00:00.000 *   08 21 26.845626 +02 30 20.65327  -1.42515  -0.82751    n.a.   n.a.   25.571  29.083
      | 2025-Nov-14 00:00:00.000 N   08 21 26.559234 +02 30 18.16844  -1.44485  -0.82299    n.a.   n.a.   25.571  29.083
      |$$EOE
      |*****************************************************************************************************************
      |Column meaning:
      | 
      |TIME
      |
      |  Times PRIOR to 1962 are UT1, a mean-solar time closely related to the
      |prior but now-deprecated GMT. Times AFTER 1962 are in UTC, the current
      |civil or "wall-clock" time-scale. UTC is kept within 0.9 seconds of UT1
      |using integer leap-seconds for 1972 and later years.
      |
      |  Conversion from the internal Barycentric Dynamical Time (TDB) of solar
      |system dynamics to the non-uniform civil UT time-scale requested for output
      |has not been determined for UTC times after the next July or January 1st.
      |Therefore, the last known leap-second is used as a constant over future
      |intervals.
      |
      |  Time tags refer to the UT time-scale conversion from TDB on Earth
      |regardless of observer location within the solar system, although clock
      |rates may differ due to the local gravity field and no analog to "UT"
      |may be defined for that location.
      |
      |  Any 'b' symbol in the 1st-column denotes a B.C. date. First-column blank
      |(" ") denotes an A.D. date.
      | 
      |CALENDAR SYSTEM
      |
      |  Mixed calendar mode was active such that calendar dates after AD 1582-Oct-15
      |(if any) are in the modern Gregorian system. Dates prior to 1582-Oct-5 (if any)
      |are in the Julian calendar system, which is automatically extended for dates
      |prior to its adoption on 45-Jan-1 BC.  The Julian calendar is useful for
      |matching historical dates. The Gregorian calendar more accurately corresponds
      |to the Earth's orbital motion and seasons. A "Gregorian-only" calendar mode is
      |available if such physical events are the primary interest.
      |
      |  NOTE: "n.a." in output means quantity "not available" at the print-time.
      | 
      |SOLAR PRESENCE (OBSERVING SITE)
      |  Time tag is followed by a blank, then a solar-presence symbol:
      |
      |       '*'  Daylight (refracted solar upper-limb on or above apparent horizon)
      |       'C'  Civil twilight/dawn
      |       'N'  Nautical twilight/dawn
      |       'A'  Astronomical twilight/dawn
      |       ' '  Night OR geocentric ephemeris
      |
      |LUNAR PRESENCE (OBSERVING SITE)
      |  The solar-presence symbol is immediately followed by a lunar-presence symbol:
      |
      |       'm'  Refracted upper-limb of Moon on or above apparent horizon
      |       ' '  Refracted upper-limb of Moon below apparent horizon OR geocentric
      |            ephemeris
      | 
      | 'R.A._________(ICRF)_________DEC' =
      |  Astrometric right ascension and declination of the target center with
      |respect to the observing site (coordinate origin) in the reference frame of
      |the planetary ephemeris (ICRF). Compensated for down-leg light-time delay
      |aberration.
      |
      |  Units: RA  in hours-minutes-seconds of time,    HH MM SS.ff{ffff}
      |         DEC in degrees-minutes-seconds of arc,  sDD MN SC.f{ffff}
      | 
      | 'dRA*cosD d(DEC)/dt' =
      |  The angular rate of change in apparent RA and DEC of the target. This is
      |with respect to the non-inertial IAU76/80 Earth true equator and equinox
      |of-date reference frame.  d(RA)/dt is multiplied by the cosine of declination
      |to provide a linear rate in the plane-of-sky. Units: ARCSECONDS PER HOUR
      | 
      | 'a-mass mag_ex' =
      |    RELATIVE optical airmass and visual magnitude extinction. Airmass is the
      |ratio between the absolute optical airmass for the targets' refracted CENTER
      |point to the absolute optical airmass at zenith. Also output is the estimated
      |visual magnitude extinction due to the atmosphere, as seen by the observer.
      |AVAILABLE ONLY FOR TOPOCENTRIC EARTH SITES WHEN THE TARGET IS ABOVE THE
      |HORIZON.  Units: none (airmass) and magnitudes (extinction).
      | 
      | 'T-mag   N-mag' =
      |   Comets' apparent visual total magnitude ("T-mag") and nuclear magnitude
      |("N-mag") using the standard IAU model:
      |
      |   T-mag= M1 + 5*log10(delta) + k1*log10(r)
      |   N-mag= M2 + 5*log10(delta) + k2*log10(r) + phcof*beta
      |
      |   Units: MAGNITUDES
      |
      |Computations by ...
      |
      |    Solar System Dynamics Group, Horizons On-Line Ephemeris System
      |    4800 Oak Grove Drive, Jet Propulsion Laboratory
      |    Pasadena, CA  91109   USA
      |
      |    General site: https://ssd.jpl.nasa.gov/
      |    Mailing list: https://ssd.jpl.nasa.gov/email_list.html
      |    System news : https://ssd.jpl.nasa.gov/horizons/news.html
      |    User Guide  : https://ssd.jpl.nasa.gov/horizons/manual.html
      |    Connect     : browser        https://ssd.jpl.nasa.gov/horizons/app.html#/x
      |                  API            https://ssd-api.jpl.nasa.gov/doc/horizons.html
      |                  command-line   telnet ssd.jpl.nasa.gov 6775
      |                  e-mail/batch   https://ssd.jpl.nasa.gov/ftp/ssd/horizons_batch.txt
      |                  scripts        https://ssd.jpl.nasa.gov/ftp/ssd/SCRIPTS
      |    Author      : Jon.D.Giorgini@jpl.nasa.gov
      |
      |*****************************************************************************************************************
      """.stripMargin

  )
