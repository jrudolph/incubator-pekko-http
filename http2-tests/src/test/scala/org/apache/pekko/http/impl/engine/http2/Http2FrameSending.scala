/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, derived from Akka.
 */

/*
 * Copyright (C) 2020-2022 Lightbend Inc. <https://www.lightbend.com>
 */

package org.apache.pekko.http.impl.engine.http2

import java.nio.ByteOrder

import org.apache.pekko
import pekko.http.impl.engine.http2.FrameEvent.{
  ContinuationFrame,
  HeadersFrame,
  PriorityFrame,
  Setting,
  SettingsFrame,
  WindowUpdateFrame
}
import pekko.http.impl.engine.http2.Http2Protocol.{ ErrorCode, Flags, FrameType, SettingIdentifier }
import pekko.http.impl.engine.http2.framing.FrameRenderer
import pekko.util.{ ByteString, ByteStringBuilder }

private[http2] trait Http2FrameSending {
  def sendBytes(bytes: ByteString): Unit

  def sendFrame(frame: FrameEvent): Unit =
    sendBytes(FrameRenderer.render(frame))

  def sendFrame(frameType: FrameType, flags: ByteFlag, streamId: Int, payload: ByteString): Unit =
    sendBytes(FrameRenderer.renderFrame(frameType, flags, streamId, payload))

  /** Can be overridden to also update windows */
  def sendDATA(streamId: Int, endStream: Boolean, data: ByteString): Unit =
    sendFrame(FrameType.DATA, Flags.END_STREAM.ifSet(endStream), streamId, data)

  def sendSETTING(identifier: SettingIdentifier, value: Int): Unit =
    sendFrame(SettingsFrame(Setting(identifier, value) :: Nil))

  def sendHEADERS(streamId: Int, endStream: Boolean, endHeaders: Boolean, headerBlockFragment: ByteString): Unit =
    sendBytes(FrameRenderer.render(HeadersFrame(streamId, endStream, endHeaders, headerBlockFragment, None)))

  def sendCONTINUATION(streamId: Int, endHeaders: Boolean, headerBlockFragment: ByteString): Unit =
    sendBytes(FrameRenderer.render(ContinuationFrame(streamId, endHeaders, headerBlockFragment)))

  def sendPRIORITY(streamId: Int, exclusiveFlag: Boolean, streamDependency: Int, weight: Int): Unit =
    sendBytes(FrameRenderer.render(PriorityFrame(streamId, exclusiveFlag, streamDependency, weight)))

  def sendRST_STREAM(streamId: Int, errorCode: ErrorCode): Unit = {
    implicit val bigEndian: ByteOrder = ByteOrder.BIG_ENDIAN
    val bb = new ByteStringBuilder
    bb.putInt(errorCode.id)
    sendFrame(FrameType.RST_STREAM, ByteFlag.Zero, streamId, bb.result())
  }

  /** Can be overridden to also update windows */
  def sendWINDOW_UPDATE(streamId: Int, windowSizeIncrement: Int): Unit =
    sendBytes(FrameRenderer.render(WindowUpdateFrame(streamId, windowSizeIncrement)))

}
