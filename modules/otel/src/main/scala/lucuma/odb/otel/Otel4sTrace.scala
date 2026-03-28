// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.otel

import cats.arrow.FunctionK
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.all.*
import natchez.Kernel
import natchez.Span
import natchez.Trace
import natchez.TraceValue
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer

import java.net.URI

object Otel4sTrace:

  def fromTracer[F[_]: MonadCancelThrow](tracer: Tracer[F]): Trace[F] =
    new Trace[F]:

      def put(fields: (String, TraceValue)*): F[Unit] =
        tracer.currentSpanOrNoop.flatMap: span =>
          fields.toList.traverse_ : (k, v) =>
            span.addAttribute(traceToAttr(k, v))

      def attachError(err: Throwable, fields: (String, TraceValue)*): F[Unit] =
        tracer.currentSpanOrNoop.flatMap: span =>
          span.recordException(err) *>
            span.setStatus(StatusCode.Error, err.getMessage) *>
            fields.toList.traverse_ : (k, v) =>
              span.addAttribute(traceToAttr(k, v))

      def log(fields: (String, TraceValue)*): F[Unit] =
        tracer.currentSpanOrNoop.flatMap: span =>
          span.addEvent("log", fields.map { case (k, v) => traceToAttr(k, v) }*)

      def log(event: String): F[Unit] =
        tracer.currentSpanOrNoop.flatMap: span =>
          span.addEvent(event)

      def kernel: F[Kernel] =
        Kernel(Map.empty).pure[F]

      def span[A](name: String, options: Span.Options)(k: F[A]): F[A] =
        tracer.span(name).surround(k)

      def spanR(name: String, options: Span.Options): Resource[F, FunctionK[F, F]] =
        tracer.span(name).resource.as(FunctionK.id[F])

      def traceId: F[Option[String]] =
        tracer.currentSpanContext.map:
          _.map(_.traceIdHex).filter(id => id.nonEmpty && id != "0" * 32)

      def traceUri: F[Option[URI]] =
        none.pure

  private def traceToAttr(key: String, tv: TraceValue): Attribute[?] =
    tv match
      case TraceValue.StringValue(v)  => Attribute(key, v)
      case TraceValue.BooleanValue(v) => Attribute(key, v)
      case TraceValue.NumberValue(v)  =>
        v match
          case l: java.lang.Long    => Attribute(key, l.longValue)
          case d: java.lang.Double  => Attribute(key, d.doubleValue)
          case f: java.lang.Float   => Attribute(key, f.doubleValue)
          case i: java.lang.Integer => Attribute(key, i.longValue)
          case other                => Attribute(key, other.toString)
