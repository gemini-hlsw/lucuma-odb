// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import cats.data.StateT
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.util.Enumerated
import org.tpolecat.typename.TypeName
import org.typelevel.log4cats.Logger
import skunk.Codec
import skunk.Session
import skunk.codec.text.varchar
import skunk.data.Type
import skunk.syntax.all.*

/** Startup checks that compare a database schema's enums and lookup tables with their Scala `Enumerated` types. */
object StartupChecks:

  case class DiagState(
    types: List[Type],
    errors: List[String]
  ):
    def updated(newTypes: List[Type], newErrors: List[String]) = DiagState(types ++ newTypes, errors ++ newErrors)
    def updated(newErrors: List[String]) = DiagState(types, errors ++ newErrors)
    def updated(newType: Type) = DiagState(newType :: types, errors)

  type Check[F[_]] = StateT[F, DiagState, Unit]

  /** Run `checks` in order and log failures. If `fatal` is `true` then crash if there are failures. */
  def runChecks[F[_]: Concurrent: Logger](checks: List[Check[F]], fatal: Boolean): F[Unit] =
    Logger[F].info("Running startup diagnostics.") >>
      checks.sequence.runS(DiagState(Nil, Nil)).map(_.errors).flatMap: errors =>
      errors.traverse_(Logger[F].error(_)) >>
        Logger[F].info("Startup diagnostics passed.").whenA(errors.isEmpty) >>
        Logger[F].error("Startup diagnostics failed.").whenA(errors.nonEmpty) >>
        Concurrent[F].raiseError(new Error(s"Startup diagnostics failed. Exiting.")).whenA(fatal && errors.nonEmpty)

  def assertUnusedPostgresEnum[F[_]: Concurrent](tpe: Type): Check[F] =
    StateT.modify(_.updated(tpe))

  def checkPostgresEnum[F[_]: Concurrent, E: Enumerated](codec: Codec[E])(using db: Session[F]): Check[F] =
    StateT.modifyF: ds =>
      db.stream(sql"SELECT unnest(enum_range(NULL::#${codec.types.head.name})::_varchar) as value".query(varchar))(skunk.Void, 104)
        .compile
        .toList
        .map(align(codec, "the database schema"))
        .map(ds.updated(codec.types, _))

  def checkPostgresLookupTable[F[_]: Concurrent, E: Enumerated: TypeName](codec: Codec[E], tableName: String)(using db: Session[F]): Check[F] =
    StateT.modifyF: ds =>
      db.stream(sql"SELECT c_tag FROM #$tableName".query(varchar))(skunk.Void, 104)
        .compile
        .toList
        .map(align(codec, s"lookup table $tableName"))
        .map(ds.updated(codec.types, _))

  /** Fails for every Postgres enum that no earlier check covered, so it belongs last. */
  def checkEnumCoverage[F[_]: Concurrent](using db: Session[F]): Check[F] =
    StateT.modifyF: ds =>
      db.stream(sql"SELECT typname::varchar FROM pg_type WHERE typtype = 'e'".query(varchar))(skunk.Void, 104)
        .compile
        .toList
        .map: es =>
          val seen = ds.types.map(_.name).toSet
          val errors = es.filterNot(seen).map(s => s"Postgres enum $s has not been checked.")
          ds.updated(errors)

  /**
   * In the case of `Site` and maybe other places the Scala side tag isn't the same as the database side tag, and this
   * is mediated through the codec. So what we do is construct the element from its Scala tag and serialize it to get
   * its database tag.
   */
  def databaseTag[A](codec: Codec[A])(tag: String)(using e: Enumerated[A]): String =
    e.fromTag(tag).flatMap(codec.encode(_).headOption.flatten).fold(tag)(_.value)

  def align[A](codec: Codec[A], where: String)(found: List[String])(using e: Enumerated[A], tn: TypeName[A]): List[String] =
    val expected = e.all.map(e.tag).map(databaseTag(codec)).toSet
    val actual   = found.toSet
    (expected ++ actual)
      .toList
      .sorted
      .flatMap: tag =>
        (expected(tag), actual(tag)) match
          case (true, false)  => List(s"${tn.value}: expected tag \"$tag\" was not found in $where.")
          case (false, true)  => List(s"${tn.value}: unexpected tag \"$tag\" was found in $where.")
          case _ => Nil
