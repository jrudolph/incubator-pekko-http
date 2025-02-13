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

package org.apache.pekko.http.impl.model.parser

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import org.apache.pekko
import org.parboiled2.Parser
import pekko.http.scaladsl.model._

private[parser] trait ContentTypeHeader { this: Parser with CommonRules with CommonActions =>

  // http://tools.ietf.org/html/rfc7231#section-3.1.1.5
  def `content-type` = rule {
    `media-type` ~ EOI ~> ((main, sub, params) => headers.`Content-Type`(contentType(main, sub, params)))
  }

  @tailrec private def contentType(
      main: String,
      sub: String,
      params: Seq[(String, String)],
      charset: Option[HttpCharset] = None,
      builder: StringMapBuilder = null): ContentType =
    params match {
      case Nil =>
        val parameters = if (builder eq null) Map.empty[String, String] else builder.result()
        getMediaType(main, sub, charset.isDefined, parameters) match {
          case x: MediaType.Binary                               => ContentType.Binary(x)
          case x: MediaType.WithFixedCharset                     => ContentType.WithFixedCharset(x)
          case x: MediaType.WithOpenCharset if charset.isDefined => ContentType.WithCharset(x, charset.get)
          case x: MediaType.WithOpenCharset if charset.isEmpty   => ContentType.WithMissingCharset(x)
        }

      case Seq((key, value), tail @ _*) if equalsAsciiCaseInsensitive(key, "charset") =>
        contentType(main, sub, tail, Some(getCharset(value)), builder)

      case Seq(kvp, tail @ _*) =>
        val b = if (builder eq null) TreeMap.newBuilder[String, String] else builder
        b += kvp
        contentType(main, sub, tail, charset, b)
    }
}
