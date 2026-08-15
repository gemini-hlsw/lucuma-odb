// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.model.User
import lucuma.odb.data.Cone
import skunk.AppliedFragment
import skunk.Void
import skunk.codec.numeric.float8
import skunk.codec.numeric.int4
import skunk.codec.numeric.int8
import skunk.implicits.*

/** The shared SQL for `targetCoordinates` cone-candidate lookups, parameterized
 *  by the relation holding the µas coordinate columns.  Used for configuration
 *  requests (`v_configuration_request.c_reference_ra/dec`) and observations
 *  (`t_obscalc.c_base_ra/dec`).
 */
object ConeSearch:

  /** Cap on cone-candidate matches.  The caller injects every id into a
   *  rewritten query, so the list bounds both the heap and the generated
   *  statement.
   */
  val MaxCandidates: Int = 10000

  /** AppliedFragment yielding the ids of `relation` rows whose coordinates lie
   *  within `distance` of `center`.
   *
   *  A wrap-aware bounding-box prefilter on the int8 microarcsecond columns (index-friendly)
   *  is followed by an exact great-circle trim. The dec column uses lucuma-core's
   *  angle encoding, which is safe here because the exact trim relies on sin/cos (2π-periodic).
   *  Null coordinates never satisfy the box predicate, so rows without a position drop out.
   *
   *  Scoped to programs visible to `user` and capped at `max + 1` rows */
  def candidates(
    relation:        String,
    idColumn:        String,
    raColumn:        String,
    decColumn:       String,
    programIdColumn: String,
    user:            User,
    cone:            Cone,
    max:             Int
  ): AppliedFragment =
    // The columns are in microarcseconds, so all params are also in microarcseconds.
    val FullCircle    = 1296000000000L // 360° in µas
    val µasPerDegree  = 3600000000.0
    val dec0ang       = cone.center.dec.toAngle.toMicroarcseconds
    val ra0           = cone.center.ra.toAngle.toMicroarcseconds
    val radius        = cone.distance.toMicroarcseconds
    val dec0rad       = cone.center.dec.toRadians
    val radiusRad     = cone.distance.toDoubleRadians
    val cosDec0       = cone.center.dec.toAngle.cos
    // The nearest pole is 90° - |dec0| away, so the cone reaches it when |dec0| + r >= 90°.
    // Then every meridian passes through the cone and no RA range can exclude anything.
    val pole          = math.abs(dec0rad) + radiusRad >= math.Pi / 2
    // Otherwise the cone spans dra either side of ra0, where sin(dra) = sin(r) / cos(dec0):
    // meridians converge toward the poles, so the same cone covers more RA at higher dec.
    //
    // The small-angle form r / cos(dec0) is not good enough. It undershoots -- by ~13° for
    // a 1° cone at dec 88.9° -- and since this box is only a prefilter, an undershoot
    // silently drops rows that the exact trim below would have kept.
    //
    // asin needs an argument <= 1, which is exactly the non-pole condition above, so `min`
    // is only absorbing float rounding at that boundary. `ceil` rounds the box outward for
    // the same reason the exact half-width is used: too wide costs a little scan, too
    // narrow loses matches.
    val dra           =
      if pole then FullCircle
      else
        val sinDra = math.min(1.0, cone.distance.sin / cosDec0)
        math.min(FullCircle, math.ceil(math.toDegrees(math.asin(sinDra)) * µasPerDegree).toLong)
    val decLo         = dec0ang - radius
    val decHi         = dec0ang + radius
    val raLo          = ra0 - dra
    val raHi          = ra0 + dra

    val µasPerDeg: AppliedFragment = sql"$float8".apply(µasPerDegree)
    val ra:  AppliedFragment = sql"#$raColumn"(Void)
    val dec: AppliedFragment = sql"#$decColumn"(Void)

    // The assembled statement, in outline:
    //
    //   select <id> from <relation>
    //    where <dec in [decLo, decHi], or in either wrapped image of it>      -- box prefilter,
    //      and <ra  in [raLo,  raHi ], or in either wrapped image of it>      -- index-friendly
    //      and <haversine: sin²(sep/2) computed in PG <= sin²(radius/2)>      -- exact trim
    //      and <program visible to user>                                      -- see correlatedIsVisibleTo
    //    limit max + 1                                                        -- one over, to detect
    //
    // The trim uses the haversine form rather than the spherical law of cosines
    // (cos(sep) >= cos(radius)), which is ill-conditioned near sep = 0: cos is flat there,
    // so with radius 0 even the center's own row can round below 1 and be excluded. The
    // haversine is well-conditioned at small separations, an exact match gives an LHS of
    // exactly 0 (integer µas differences are exact).
    val dec0µas: AppliedFragment = sql"$int8".apply(dec0ang)
    sql"select #$idColumn from #$relation"(Void)                                                                      |+|
    void" where (" |+| dec |+| void" between "    |+| sql"$int8".apply(decLo) |+| void" and " |+| sql"$int8".apply(decHi) |+|
    void" or " |+| dec |+| void" >= "             |+| sql"$int8".apply(decLo + FullCircle)                            |+|
    void" or " |+| dec |+| void" <= "             |+| sql"$int8".apply(decHi - FullCircle) |+| void")"                |+|
    void" and (" |+| ra |+| void" between "       |+| sql"$int8".apply(raLo)  |+| void" and " |+| sql"$int8".apply(raHi) |+|
    void" or " |+| ra |+| void" >= "              |+| sql"$int8".apply(raLo + FullCircle)                             |+|
    void" or " |+| ra |+| void" <= "              |+| sql"$int8".apply(raHi - FullCircle) |+| void")"                 |+|
    void" and pow(sin(radians((" |+| dec |+| void" - " |+| dec0µas |+| void") / " |+| µasPerDeg |+| void") / 2), 2)"  |+|
    void" + cos(radians(" |+| dec |+| void" / "   |+| µasPerDeg |+| void")) * cos(radians(" |+| dec0µas |+| void" / " |+| µasPerDeg |+| void"))" |+|
    void" * pow(sin(radians((" |+| ra |+| void" - " |+| sql"$int8".apply(ra0) |+| void") / " |+| µasPerDeg |+| void") / 2), 2)" |+|
    void" <= pow(sin(radians("                    |+| sql"$int8".apply(radius) |+| void" / " |+| µasPerDeg |+| void") / 2), 2)" |+|
    void" and (" |+| ProgramUserService.Statements.correlatedIsVisibleTo(user, programIdColumn) |+| void")"           |+|
    void" limit "                                 |+| sql"$int4".apply(max + 1)
