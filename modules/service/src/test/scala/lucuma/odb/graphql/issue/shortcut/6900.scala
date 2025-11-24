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

      Set(("format", "text"), ("MAKE_EPHEM", "YES"), ("CENTER", "coord"), ("COORD_TYPE", "GEODETIC"), ("COMMAND", "'NAME=1P;CAP'"), ("SITE_COORD", "'204.530945,19.823807,4.213'"), ("START_TIME", "'2025-Nov-12 00:00:00.812'"), ("STOP_TIME", "'2025-Nov-13 00:00:00.812'"), ("STEP_SIZE", "60m"), ("extra_prec", "YES"), ("time_digits", "FRACSEC"), ("QUANTITIES", "'1,3,8,9'")) ->
      """|
      |API VERSION: 1.2
      |API SOURCE: NASA/JPL Horizons API
      |
      |*******************************************************************************
      |JPL/HORIZONS                      1P/Halley                2025-Nov-24 12:24:14
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
      |Ephemeris / API_USER Mon Nov 24 12:24:15 2025 Pasadena, USA      / Horizons    
      |*******************************************************************************
      |Target body name: 1P/Halley                       {source: JPL#75}
      |Center body name: Earth (399)                     {source: DE441}
      |Center-site name: (user defined site below)
      |*******************************************************************************
      |Start time      : A.D. 2025-Nov-12 00:00:00.8120 UT      
      |Stop  time      : A.D. 2025-Nov-13 00:00:00.8120 UT      
      |Step-size       : 60 minutes
      |*******************************************************************************
      |Target pole/equ : undefined
      |Target radii    : 5.5 km                                                       
      |Center geodetic : 204.530945, 19.823807, 4.213    {E-lon(deg),Lat(deg),Alt(km)}
      |Center cylindric: 204.530945,6006.44268,2150.78648 {E-lon(deg),Dxy(km),Dz(km)}
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
      |EOP file        : eop.251121.p260217                                           
      |EOP coverage    : DATA-BASED 1962-JAN-20 TO 2025-NOV-21. PREDICTS-> 2026-FEB-16
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
      | 2025-Nov-12 00:00:00.812 *   08 21 31.081970 +02 30 58.71021  -1.30922  -0.86932    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 01:00:00.812 *   08 21 30.995002 +02 30 57.83878  -1.30119  -0.86793    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 02:00:00.812 *   08 21 30.908457 +02 30 56.96882  -1.29667  -0.86643    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 03:00:00.812 *   08 21 30.822081 +02 30 56.10039  -1.29616  -0.86488    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 04:00:00.812 C   08 21 30.735597 +02 30 55.23352  -1.29992  -0.86330    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 05:00:00.812     08 21 30.648721 +02 30 54.36821  -1.30788  -0.86176    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 06:00:00.812     08 21 30.561182 +02 30 53.50437  -1.31969  -0.86030    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 07:00:00.812     08 21 30.472742 +02 30 52.64192  -1.33476  -0.85896    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 08:00:00.812     08 21 30.383210 +02 30 51.78070  -1.35225  -0.85776    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 09:00:00.812     08 21 30.292457 +02 30 50.92056  -1.37117  -0.85673    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 10:00:00.812     08 21 30.200424 +02 30 50.06132  -1.39042  -0.85589   5.160  0.562   25.573  29.085
      | 2025-Nov-12 11:00:00.812  m  08 21 30.107127 +02 30 49.20279  -1.40888  -0.85522   2.360  0.257   25.573  29.085
      | 2025-Nov-12 12:00:00.812  m  08 21 30.012653 +02 30 48.34481  -1.42549  -0.85471   1.591  0.173   25.573  29.085
      | 2025-Nov-12 13:00:00.812  m  08 21 29.917160 +02 30 47.48724  -1.43932  -0.85434   1.263  0.138   25.573  29.085
      | 2025-Nov-12 14:00:00.812  m  08 21 29.820857 +02 30 46.62995  -1.44961  -0.85408   1.108  0.121   25.572  29.084
      | 2025-Nov-12 15:00:00.812  m  08 21 29.724001 +02 30 45.77288  -1.45587  -0.85387   1.050  0.114   25.572  29.084
      | 2025-Nov-12 16:00:00.812 Nm  08 21 29.626868 +02 30 44.91600  -1.45787  -0.85367   1.066  0.116   25.572  29.084
      | 2025-Nov-12 17:00:00.812 *m  08 21 29.529744 +02 30 44.05933  -1.45567  -0.85344   1.162  0.127   25.572  29.084
      | 2025-Nov-12 18:00:00.812 *m  08 21 29.432897 +02 30 43.20294  -1.44962  -0.85313   1.379  0.150   25.572  29.084
      | 2025-Nov-12 19:00:00.812 *m  08 21 29.336566 +02 30 42.34693  -1.44035  -0.85270   1.846  0.201   25.572  29.084
      | 2025-Nov-12 20:00:00.812 *m  08 21 29.240939 +02 30 41.49144  -1.42868  -0.85212   3.092  0.337   25.572  29.084
      | 2025-Nov-12 21:00:00.812 *m  08 21 29.146142 +02 30 40.63665  -1.41562  -0.85137  10.843  1.181   25.572  29.084
      | 2025-Nov-12 22:00:00.812 *m  08 21 29.052233 +02 30 39.78273  -1.40227  -0.85043    n.a.   n.a.   25.572  29.084
      | 2025-Nov-12 23:00:00.812 *m  08 21 28.959194 +02 30 38.92986  -1.38974  -0.84932    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 00:00:00.812 *   08 21 28.866933 +02 30 38.07821  -1.37909  -0.84804    n.a.   n.a.   25.572  29.084
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

      Set(("format", "text"), ("MAKE_EPHEM", "YES"), ("CENTER", "coord"), ("COORD_TYPE", "GEODETIC"), ("COMMAND", "'NAME=1P;CAP'"), ("SITE_COORD", "'204.530945,19.823807,4.213'"), ("START_TIME", "'2025-Nov-12 00:00:00.812'"), ("STOP_TIME", "'2025-Nov-14 00:00:00.812'"), ("STEP_SIZE", "180m"), ("extra_prec", "YES"), ("time_digits", "FRACSEC"), ("QUANTITIES", "'1,3,8,9'")) ->
      """
      |API VERSION: 1.2
      |API SOURCE: NASA/JPL Horizons API
      |
      |*******************************************************************************
      |JPL/HORIZONS                      1P/Halley                2025-Nov-24 12:25:25
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
      |Ephemeris / API_USER Mon Nov 24 12:25:25 2025 Pasadena, USA      / Horizons    
      |*******************************************************************************
      |Target body name: 1P/Halley                       {source: JPL#75}
      |Center body name: Earth (399)                     {source: DE441}
      |Center-site name: (user defined site below)
      |*******************************************************************************
      |Start time      : A.D. 2025-Nov-12 00:00:00.8120 UT      
      |Stop  time      : A.D. 2025-Nov-14 00:00:00.8120 UT      
      |Step-size       : 180 minutes
      |*******************************************************************************
      |Target pole/equ : undefined
      |Target radii    : 5.5 km                                                       
      |Center geodetic : 204.530945, 19.823807, 4.213    {E-lon(deg),Lat(deg),Alt(km)}
      |Center cylindric: 204.530945,6006.44268,2150.78648 {E-lon(deg),Dxy(km),Dz(km)}
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
      |EOP file        : eop.251121.p260217                                           
      |EOP coverage    : DATA-BASED 1962-JAN-20 TO 2025-NOV-21. PREDICTS-> 2026-FEB-16
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
      | 2025-Nov-12 00:00:00.812 *   08 21 31.081970 +02 30 58.71021  -1.30922  -0.86932    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 03:00:00.812 *   08 21 30.822081 +02 30 56.10039  -1.29616  -0.86488    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 06:00:00.812     08 21 30.561182 +02 30 53.50437  -1.31969  -0.86030    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 09:00:00.812     08 21 30.292457 +02 30 50.92056  -1.37117  -0.85673    n.a.   n.a.   25.573  29.085
      | 2025-Nov-12 12:00:00.812  m  08 21 30.012653 +02 30 48.34481  -1.42549  -0.85471   1.591  0.173   25.573  29.085
      | 2025-Nov-12 15:00:00.812  m  08 21 29.724001 +02 30 45.77288  -1.45587  -0.85387   1.050  0.114   25.572  29.084
      | 2025-Nov-12 18:00:00.812 *m  08 21 29.432897 +02 30 43.20294  -1.44962  -0.85313   1.379  0.150   25.572  29.084
      | 2025-Nov-12 21:00:00.812 *m  08 21 29.146142 +02 30 40.63665  -1.41562  -0.85137  10.843  1.181   25.572  29.084
      | 2025-Nov-13 00:00:00.812 *   08 21 28.866933 +02 30 38.07821  -1.37909  -0.84804    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 03:00:00.812 *   08 21 28.592983 +02 30 35.53188  -1.36674  -0.84354    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 06:00:00.812     08 21 28.317865 +02 30 32.99948  -1.39105  -0.83894    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 09:00:00.812     08 21 28.034800 +02 30 30.47934  -1.44289  -0.83536    n.a.   n.a.   25.572  29.084
      | 2025-Nov-13 12:00:00.812  m  08 21 27.740646 +02 30 27.96729  -1.49694  -0.83333   1.561  0.170   25.572  29.083
      | 2025-Nov-13 15:00:00.812  m  08 21 27.437751 +02 30 25.45912  -1.52657  -0.83245   1.049  0.114   25.571  29.083
      | 2025-Nov-13 18:00:00.812 *m  08 21 27.132571 +02 30 22.95309  -1.51950  -0.83165   1.400  0.152   25.571  29.083
      | 2025-Nov-13 21:00:00.812 *m  08 21 26.831869 +02 30 20.45093  -1.48509  -0.82981  12.817  1.396   25.571  29.083
      | 2025-Nov-14 00:00:00.812 *m  08 21 26.538733 +02 30 17.95685  -1.44878  -0.82641    n.a.   n.a.   25.571  29.083
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
      |""".stripMargin

    )
