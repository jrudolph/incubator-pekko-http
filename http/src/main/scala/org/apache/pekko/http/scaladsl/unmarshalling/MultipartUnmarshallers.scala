/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, derived from Akka.
 */

/*
 * Copyright (C) 2009-2022 Lightbend Inc. <https://www.lightbend.com>
 */

package org.apache.pekko.http.scaladsl.unmarshalling

import org.apache.pekko
import pekko.event.{ LoggingAdapter, NoLogging }
import pekko.http.impl.engine.parsing.BodyPartParser
import pekko.http.impl.util.StreamUtils
import pekko.http.scaladsl.model.HttpCharsets._
import pekko.http.scaladsl.model.MediaRanges._
import pekko.http.scaladsl.model.MediaTypes._
import pekko.http.scaladsl.model._
import pekko.http.scaladsl.settings.ParserSettings
import pekko.http.scaladsl.unmarshalling.Unmarshaller.UnsupportedContentTypeException
import pekko.http.scaladsl.util.FastFuture
import pekko.stream.ActorMaterializerHelper
import pekko.stream.scaladsl._

import scala.collection.immutable
import scala.collection.immutable.VectorBuilder

/**
 * Provides [[pekko.http.scaladsl.model.Multipart]] marshallers.
 * It is possible to configure the default parsing mode by providing an implicit [[pekko.http.scaladsl.settings.ParserSettings]] instance.
 */
trait MultipartUnmarshallers {

  implicit def defaultMultipartGeneralUnmarshaller(implicit log: LoggingAdapter = NoLogging,
      parserSettings: ParserSettings = null): FromEntityUnmarshaller[Multipart.General] =
    multipartGeneralUnmarshaller(`UTF-8`)

  def multipartGeneralUnmarshaller(defaultCharset: HttpCharset)(implicit log: LoggingAdapter = NoLogging,
      parserSettings: ParserSettings = null): FromEntityUnmarshaller[Multipart.General] =
    multipartUnmarshaller[Multipart.General, Multipart.General.BodyPart, Multipart.General.BodyPart.Strict](
      mediaRange = `multipart/*`,
      defaultContentType = MediaTypes.`text/plain`.withCharset(defaultCharset),
      createBodyPart = Multipart.General.BodyPart(_, _),
      createStreamed = Multipart.General(_, _),
      createStrictBodyPart = Multipart.General.BodyPart.Strict,
      createStrict = Multipart.General.Strict)

  implicit def multipartFormDataUnmarshaller(implicit log: LoggingAdapter = NoLogging,
      parserSettings: ParserSettings = null): FromEntityUnmarshaller[Multipart.FormData] =
    multipartUnmarshaller[Multipart.FormData, Multipart.FormData.BodyPart, Multipart.FormData.BodyPart.Strict](
      mediaRange = `multipart/form-data`,
      defaultContentType = ContentTypes.`text/plain(UTF-8)`,
      createBodyPart = (entity, headers) => Multipart.General.BodyPart(entity, headers).toFormDataBodyPart.get,
      createStreamed = (_, parts) => Multipart.FormData(parts),
      createStrictBodyPart =
        (entity, headers) => Multipart.General.BodyPart.Strict(entity, headers).toFormDataBodyPart.get,
      createStrict = (_, parts) => Multipart.FormData.Strict(parts))

  implicit def defaultMultipartByteRangesUnmarshaller(implicit log: LoggingAdapter = NoLogging,
      parserSettings: ParserSettings = null): FromEntityUnmarshaller[Multipart.ByteRanges] =
    multipartByteRangesUnmarshaller(`UTF-8`)

  def multipartByteRangesUnmarshaller(defaultCharset: HttpCharset)(implicit log: LoggingAdapter = NoLogging,
      parserSettings: ParserSettings = null): FromEntityUnmarshaller[Multipart.ByteRanges] =
    multipartUnmarshaller[Multipart.ByteRanges, Multipart.ByteRanges.BodyPart, Multipart.ByteRanges.BodyPart.Strict](
      mediaRange = `multipart/byteranges`,
      defaultContentType = MediaTypes.`text/plain`.withCharset(defaultCharset),
      createBodyPart = (entity, headers) => Multipart.General.BodyPart(entity, headers).toByteRangesBodyPart.get,
      createStreamed = (_, parts) => Multipart.ByteRanges(parts),
      createStrictBodyPart =
        (entity, headers) => Multipart.General.BodyPart.Strict(entity, headers).toByteRangesBodyPart.get,
      createStrict = (_, parts) => Multipart.ByteRanges.Strict(parts))

  def multipartUnmarshaller[T <: Multipart, BP <: Multipart.BodyPart, BPS <: Multipart.BodyPart.Strict](
      mediaRange: MediaRange,
      defaultContentType: ContentType,
      createBodyPart: (BodyPartEntity, List[HttpHeader]) => BP,
      createStreamed: (MediaType.Multipart, Source[BP, Any]) => T,
      createStrictBodyPart: (HttpEntity.Strict, List[HttpHeader]) => BPS,
      createStrict: (MediaType.Multipart, immutable.Seq[BPS]) => T)(implicit log: LoggingAdapter = NoLogging,
      parserSettings: ParserSettings = null): FromEntityUnmarshaller[T] =
    Unmarshaller.withMaterializer { implicit ec => implicit mat => entity =>
      if (entity.contentType.mediaType.isMultipart && mediaRange.matches(entity.contentType.mediaType)) {
        entity.contentType.mediaType.params.get("boundary") match {
          case None =>
            FastFuture.failed(
              new RuntimeException("Content-Type with a multipart media type must have a 'boundary' parameter"))
          case Some(boundary) =>
            import BodyPartParser._
            val effectiveParserSettings =
              Option(parserSettings).getOrElse(ParserSettings(ActorMaterializerHelper.downcast(mat).system))
            val parser = new BodyPartParser(defaultContentType, boundary, log, effectiveParserSettings)

            entity match {
              case HttpEntity.Strict(ContentType(mediaType: MediaType.Multipart, _), data) =>
                Source.single(data)
                  .via(parser)
                  .runFold(new VectorBuilder[BPS]()) { (builder, output) =>
                    output match {
                      case BodyPartStart(headers, createEntity) =>
                        val entity = createEntity(Source.empty) match {
                          case x: HttpEntity.Strict => x
                          case x =>
                            throw new IllegalStateException("Unexpected entity type from strict BodyPartParser: " + x)
                        }
                        builder += createStrictBodyPart(entity, headers)
                      case ParseError(errorInfo) => throw ParsingException(errorInfo)
                      case x                     => throw new IllegalStateException(s"Unexpected BodyPartParser result $x in strict case")
                    }
                  }.map { builder =>
                    createStrict(mediaType, builder.result())
                  }
              case _ =>
                val bodyParts = entity.dataBytes
                  .via(parser)
                  .splitWhen(_.isInstanceOf[PartStart])
                  .prefixAndTail(1)
                  .collect {
                    case (Seq(BodyPartStart(headers, createEntity)), entityParts) =>
                      createBodyPart(createEntity(entityParts), headers)
                    case (Seq(ParseError(errorInfo)), rest) =>
                      StreamUtils.cancelSource(rest)
                      throw ParsingException(errorInfo)
                  }
                  .concatSubstreams

                FastFuture.successful {
                  createStreamed(entity.contentType.mediaType.asInstanceOf[MediaType.Multipart], bodyParts)
                }
            }
        }
      } else FastFuture.failed(UnsupportedContentTypeException(Some(entity.contentType), mediaRange))
    }
}

object MultipartUnmarshallers extends MultipartUnmarshallers
