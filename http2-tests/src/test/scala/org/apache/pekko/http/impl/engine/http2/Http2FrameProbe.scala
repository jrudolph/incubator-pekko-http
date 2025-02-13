/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, derived from Akka.
 */

/*
 * Copyright (C) 2019-2022 Lightbend Inc. <https://www.lightbend.com>
 */

package org.apache.pekko.http.impl.engine.http2

import org.apache.pekko
import pekko.actor.ActorSystem
import pekko.http.impl.engine.http2.FrameEvent.SettingsFrame
import pekko.http.impl.engine.http2.Http2FrameProbe.FrameHeader
import pekko.http.impl.engine.http2.Http2Protocol.ErrorCode
import pekko.http.impl.engine.http2.Http2Protocol.Flags
import pekko.http.impl.engine.http2.Http2Protocol.FrameType
import pekko.http.impl.engine.http2.framing.Http2FrameParsing
import pekko.http.impl.engine.ws.ByteStringSinkProbe
import pekko.stream.impl.io.ByteStringParser.ByteReader
import pekko.stream.scaladsl.Sink
import pekko.util.ByteString

import scala.annotation.tailrec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

private[http2] trait Http2FrameProbe {
  def sink: Sink[ByteString, Any]
  def plainDataProbe: ByteStringSinkProbe

  def expectBytes(bytes: ByteString): Unit
  def expectBytes(num: Int): ByteString
  def expectNoBytes(): Unit
  def expectNoBytes(timeout: FiniteDuration): Unit

  def expect[T <: FrameEvent]()(implicit tag: ClassTag[T]): T

  def expectDATAFrame(streamId: Int): (Boolean, ByteString)
  def expectDATA(streamId: Int, endStream: Boolean, numBytes: Int): ByteString
  def expectDATA(streamId: Int, endStream: Boolean, data: ByteString): Unit
  def expectRST_STREAM(streamId: Int, errorCode: ErrorCode): Unit
  def expectRST_STREAM(streamId: Int): ErrorCode

  def expectGOAWAY(lastStreamId: Int = -1): (Int, ErrorCode)

  def expectFrame(frameType: FrameType, expectedFlags: ByteFlag, streamId: Int, payload: ByteString): Unit

  def expectFramePayload(frameType: FrameType, expectedFlags: ByteFlag, streamId: Int): ByteString
  def expectFrameFlagsAndPayload(frameType: FrameType, streamId: Int): (ByteFlag, ByteString)
  def expectFrameFlagsStreamIdAndPayload(frameType: FrameType): (ByteFlag, Int, ByteString)

  def expectSETTINGS(): SettingsFrame
  def expectSettingsAck(): Unit

  /** expect only the header of a frame, the payload will be left on the wire */
  def expectFrameHeader(): FrameHeader

  /** Collect a header block maybe spanning several frames */
  def expectHeaderBlock(streamId: Int, endStream: Boolean = true): ByteString

  def updateFromServerWindows(streamId: Int, update: Int => Int): Unit
  def updateFromServerWindowForConnection(update: Int => Int): Unit

  def remainingFromServerWindowForConnection: Int
  def remainingFromServerWindowFor(streamId: Int): Int

  def expectComplete(): Unit

  def within[T](max: FiniteDuration)(f: => T): T = plainDataProbe.within(max)(f)
}

/**
 * Allows to get all of the probe's methods into scope, delegating to the actual probe. Nice when using the `TestSetup`
 * approach.
 */
private[http2] trait Http2FrameProbeDelegator extends Http2FrameProbe {
  def frameProbeDelegate: Http2FrameProbe

  def sink: Sink[ByteString, Any] = frameProbeDelegate.sink
  def plainDataProbe: ByteStringSinkProbe = frameProbeDelegate.plainDataProbe
  def expectBytes(bytes: ByteString): Unit = frameProbeDelegate.expectBytes(bytes)
  def expectBytes(num: Int): ByteString = frameProbeDelegate.expectBytes(num)
  def expectNoBytes(): Unit = frameProbeDelegate.expectNoBytes()
  def expectNoBytes(timeout: FiniteDuration): Unit = frameProbeDelegate.expectNoBytes(timeout)
  def expect[T <: FrameEvent]()(implicit tag: ClassTag[T]): T = frameProbeDelegate.expect()
  def expectDATAFrame(streamId: Int): (Boolean, ByteString) = frameProbeDelegate.expectDATAFrame(streamId)
  def expectDATA(streamId: Int, endStream: Boolean, numBytes: Int): ByteString =
    frameProbeDelegate.expectDATA(streamId, endStream, numBytes)
  def expectDATA(streamId: Int, endStream: Boolean, data: ByteString): Unit =
    frameProbeDelegate.expectDATA(streamId, endStream, data)
  def expectRST_STREAM(streamId: Int, errorCode: ErrorCode): Unit =
    frameProbeDelegate.expectRST_STREAM(streamId, errorCode)
  def expectRST_STREAM(streamId: Int): ErrorCode = frameProbeDelegate.expectRST_STREAM(streamId)
  def expectGOAWAY(lastStreamId: Int): (Int, ErrorCode) = frameProbeDelegate.expectGOAWAY(lastStreamId)
  def expectSETTINGS(): SettingsFrame = frameProbeDelegate.expectSETTINGS()
  def expectSettingsAck(): Unit = frameProbeDelegate.expectSettingsAck()
  def expectFrame(frameType: FrameType, expectedFlags: ByteFlag, streamId: Int, payload: ByteString): Unit =
    frameProbeDelegate.expectFrame(frameType, expectedFlags, streamId, payload)
  def expectFramePayload(frameType: FrameType, expectedFlags: ByteFlag, streamId: Int): ByteString =
    frameProbeDelegate.expectFramePayload(frameType, expectedFlags, streamId)
  def expectFrameFlagsAndPayload(frameType: FrameType, streamId: Int): (ByteFlag, ByteString) =
    frameProbeDelegate.expectFrameFlagsAndPayload(frameType, streamId)
  def expectFrameFlagsStreamIdAndPayload(frameType: FrameType): (ByteFlag, Int, ByteString) =
    frameProbeDelegate.expectFrameFlagsStreamIdAndPayload(frameType)
  def expectFrameHeader(): FrameHeader = frameProbeDelegate.expectFrameHeader()
  def expectHeaderBlock(streamId: Int, endStream: Boolean): ByteString =
    frameProbeDelegate.expectHeaderBlock(streamId, endStream)
  def updateFromServerWindows(streamId: Int, update: Int => Int): Unit =
    frameProbeDelegate.updateFromServerWindows(streamId, update)
  def updateFromServerWindowForConnection(update: Int => Int): Unit =
    frameProbeDelegate.updateFromServerWindowForConnection(update)
  def remainingFromServerWindowForConnection: Int = frameProbeDelegate.remainingFromServerWindowForConnection
  def remainingFromServerWindowFor(streamId: Int): Int = frameProbeDelegate.remainingFromServerWindowFor(streamId)

  def expectComplete(): Unit = frameProbeDelegate.expectComplete()
}

private[http] object Http2FrameProbe extends Matchers {
  case class FrameHeader(frameType: FrameType, flags: ByteFlag, streamId: Int, payloadLength: Int)

  def apply()(implicit system: ActorSystem): Http2FrameProbe =
    new Http2FrameProbe {
      val probe = ByteStringSinkProbe()
      override def sink: Sink[ByteString, Any] = probe.sink
      override def plainDataProbe: ByteStringSinkProbe = probe

      def expectBytes(bytes: ByteString): Unit = probe.expectBytes(bytes)
      def expectBytes(num: Int): ByteString = probe.expectBytes(num)
      def expectNoBytes(): Unit = probe.expectNoBytes()
      def expectNoBytes(timeout: FiniteDuration): Unit = probe.expectNoBytes(timeout)

      def expect[T <: FrameEvent]()(implicit tag: ClassTag[T]): T = {
        // Not supporting large frames or high streamId's here for now, throw when we encounter those.
        probe.expectBytes(2) should be(ByteString(0, 0))
        val length = probe.expectByte()
        val _type = FrameType.byId(probe.expectByte()).get
        val flags = new ByteFlag(probe.expectByte())
        probe.expectBytes(3) should be(ByteString(0, 0, 0))
        val streamId = probe.expectByte()
        val payload = probe.expectBytes(length)
        val frame = Http2FrameParsing.parseFrame(_type, flags, streamId, new ByteReader(payload), system.log)
        frame shouldBe a[T]
        frame.asInstanceOf[T]
      }

      def expectDATAFrame(streamId: Int): (Boolean, ByteString) = {
        val (flags, payload) = expectFrameFlagsAndPayload(FrameType.DATA, streamId)
        updateFromServerWindowForConnection(_ - payload.size)
        updateFromServerWindows(streamId, _ - payload.size)
        (Flags.END_STREAM.isSet(flags), payload)
      }

      def expectDATA(streamId: Int, endStream: Boolean, numBytes: Int): ByteString = {
        @tailrec def collectMore(collected: ByteString, remainingBytes: Int): ByteString = {
          val (completed, data) = expectDATAFrame(streamId)
          data.size should be <= remainingBytes // cannot have more data pending
          if (data.size < remainingBytes) {
            completed shouldBe false
            collectMore(collected ++ data, remainingBytes - data.size)
          } else {
            // data.size == remainingBytes, i.e. collection finished
            if (endStream && !completed) // wait for final empty data frame
              expectFramePayload(FrameType.DATA, Flags.END_STREAM, streamId) shouldBe ByteString.empty
            collected ++ data
          }
        }
        collectMore(ByteString.empty, numBytes)
      }

      def expectDATA(streamId: Int, endStream: Boolean, data: ByteString): Unit =
        expectDATA(streamId, endStream, data.length) shouldBe data

      def expectRST_STREAM(streamId: Int, errorCode: ErrorCode): Unit =
        expectRST_STREAM(streamId) shouldBe errorCode

      def expectRST_STREAM(streamId: Int): ErrorCode = {
        val payload = expectFramePayload(FrameType.RST_STREAM, ByteFlag.Zero, streamId)
        ErrorCode.byId(new ByteReader(payload).readIntBE())
      }

      /**
       * If the lastStreamId should not be asserted keep it as a negative value (which is never a real stream id)
       * @return pair of `lastStreamId` and the [[ErrorCode]]
       */
      def expectGOAWAY(lastStreamId: Int = -1): (Int, ErrorCode) = {
        // GOAWAY is always written to stream zero:
        //   The GOAWAY frame applies to the connection, not a specific stream.
        //   An endpoint MUST treat a GOAWAY frame with a stream identifier other than 0x0
        //   as a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
        val payload = expectFramePayload(FrameType.GOAWAY, ByteFlag.Zero, streamId = 0)
        val reader = new ByteReader(payload)
        val incomingLastStreamId = reader.readIntBE()
        if (lastStreamId > 0) incomingLastStreamId should ===(lastStreamId)
        (lastStreamId, ErrorCode.byId(reader.readIntBE()))
      }

      override def expectSETTINGS(): SettingsFrame = {
        // (6.5) The stream identifier for a SETTINGS frame MUST be zero (0x0).
        val payload = expectFramePayload(FrameType.SETTINGS, Flags.NO_FLAGS, 0)
        Http2FrameParsing.parseFrame(FrameType.SETTINGS, Flags.NO_FLAGS, 0, new ByteReader(payload),
          system.log).asInstanceOf[SettingsFrame]
      }

      def expectSettingsAck() = expectFrame(FrameType.SETTINGS, Flags.ACK, 0, ByteString.empty)

      def expectFrame(frameType: FrameType, expectedFlags: ByteFlag, streamId: Int, payload: ByteString) =
        expectFramePayload(frameType, expectedFlags, streamId) should ===(payload)

      def expectFramePayload(frameType: FrameType, expectedFlags: ByteFlag, streamId: Int): ByteString = {
        val (flags, data) = expectFrameFlagsAndPayload(frameType, streamId)
        expectedFlags shouldBe flags
        data
      }
      final def expectFrameFlagsAndPayload(frameType: FrameType, streamId: Int): (ByteFlag, ByteString) = {
        val (flags, gotStreamId, data) = expectFrameFlagsStreamIdAndPayload(frameType)
        gotStreamId shouldBe streamId
        (flags, data)
      }
      final def expectFrameFlagsStreamIdAndPayload(frameType: FrameType): (ByteFlag, Int, ByteString) = {
        val header = expectFrameHeader()
        header.frameType shouldBe frameType
        (header.flags, header.streamId, expectBytes(header.payloadLength))
      }

      def expectFrameHeader(): FrameHeader = {
        val headerBytes = expectBytes(9)

        val reader = new ByteReader(headerBytes)
        val length = reader.readShortBE() << 8 | reader.readByte()
        val tpe = Http2Protocol.FrameType.byId(reader.readByte()).get
        val flags = new ByteFlag(reader.readByte())
        val streamId = reader.readIntBE()

        FrameHeader(tpe, flags, streamId, length)
      }

      /** Collect a header block maybe spanning several frames */
      def expectHeaderBlock(streamId: Int, endStream: Boolean = true): ByteString =
        // FIXME: also collect CONTINUATION frames as long as END_HEADERS is not set
        expectFramePayload(FrameType.HEADERS, Flags.END_STREAM.ifSet(endStream) | Flags.END_HEADERS, streamId)

      def updateFromServerWindows(streamId: Int, update: Int => Int): Unit =
        fromServerWindows = updateWindowMap(streamId, safeUpdate(update))(fromServerWindows)
      def updateFromServerWindowForConnection(update: Int => Int): Unit =
        fromServerWindowForConnection = safeUpdate(update)(fromServerWindowForConnection)

      private var fromServerWindows: Map[Int, Int] = Map.empty.withDefaultValue(Http2Protocol.InitialWindowSize)
      private var fromServerWindowForConnection = Http2Protocol.InitialWindowSize
      // keep counters that are updated for incoming DATA frames and outgoing WINDOW_UPDATE frames
      def remainingFromServerWindowForConnection: Int = fromServerWindowForConnection
      def remainingFromServerWindowFor(streamId: Int): Int =
        fromServerWindows(streamId) min remainingFromServerWindowForConnection

      def updateWindowMap(streamId: Int, update: Int => Int): Map[Int, Int] => Map[Int, Int] =
        map => map.updated(streamId, update(map(streamId)))

      def safeUpdate(update: Int => Int): Int => Int = { oldValue =>
        val newValue = update(oldValue)
        newValue should be >= 0
        newValue
      }

      def expectComplete(): Unit = probe.expectComplete()
    }
}
