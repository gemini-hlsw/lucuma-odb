// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.schema

import cats.syntax.all.*
import fs2.io.file.Path
import grackle.*
import org.tpolecat.sourcepos.SourcePos

import java.lang.System.lineSeparator

private object SchemaStitcherMacros:
  import scala.quoted.*
  import scala.quoted.Quotes

  inline def fromResources(name: String) =
    ${ SchemaStitcherMacros.fromResourcesImpl('name) }

  /**
   * Macro to do some compile time checking for the schema. It will:
   *   - Check that the resource exists
   *   - Check that the schema can be loaded and stitched together without errors
   *   - If the schema can be loaded, it will embed the final schema string in the code, so it
   *     doesn't have to be loaded at runtime
   */
  def fromResourcesImpl(x: Expr[String])(using Quotes): Expr[SchemaStitcher] =
    import quotes.reflect.report

    val location = x.valueOrAbort
    val cl       = this.getClass().getClassLoader()

    SchemaStitcher(
      Path(location),
      SchemaSource.fromResource(cl)
    ).build match
      case Result.Success(schema) =>
        val strSchema = Expr(schema.toString)
        '{ SchemaStitcher.pure($strSchema) }

      case Result.Warning(problems, schema) =>
        report
          .warning(
            s"Loaded schema '${location}' with problems:${lineSeparator}${problems.map(_.toString).mkString_(lineSeparator).indent(4)}",
            x
          )
        val strSchema = Expr(schema.toString)
        '{ SchemaStitcher.pure($strSchema) }

      case Result.Failure(problems) =>
        report.errorAndAbort(
          s"Unable to load schema '${location}':${lineSeparator}${problems.map(_.toString).mkString_(lineSeparator).indent(4)}",
          x
        )

      case Result.InternalError(error) =>
        report.errorAndAbort(s"Unable to load schema '${location}': ${error.getMessage}", x)
