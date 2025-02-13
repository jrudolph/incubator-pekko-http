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

package org.apache.pekko.http.impl.engine.ws

import org.apache.pekko
import pekko.http.impl.engine.ws.Protocol.Opcode
import pekko.http.impl.engine.ws.WSTestUtils._
import pekko.util.ByteString

import scala.util.Random
import org.scalatest.matchers.should.Matchers

trait WSTestSetupBase extends Matchers {
  def send(bytes: ByteString): Unit
  def expectBytes(length: Int): ByteString
  def expectBytes(bytes: ByteString): Unit

  def sendWSFrame(
      opcode: Opcode,
      data: ByteString,
      fin: Boolean,
      mask: Boolean = false,
      rsv1: Boolean = false,
      rsv2: Boolean = false,
      rsv3: Boolean = false): Unit = {
    val (theMask, theData) =
      if (mask) {
        val m = Random.nextInt()
        (Some(m), maskedBytes(data, m)._1)
      } else (None, data)
    send(frameHeader(opcode, data.length, fin, theMask, rsv1, rsv2, rsv3) ++ theData)
  }

  def sendWSCloseFrame(closeCode: Int, mask: Boolean = false): Unit =
    send(closeFrame(closeCode, mask))

  def expectWSFrame(
      opcode: Opcode,
      data: ByteString,
      fin: Boolean,
      mask: Option[Int] = None,
      rsv1: Boolean = false,
      rsv2: Boolean = false,
      rsv3: Boolean = false): Unit =
    expectBytes(frameHeader(opcode, data.length, fin, mask, rsv1, rsv2, rsv3) ++ data)

  def expectWSCloseFrame(closeCode: Int, mask: Boolean = false): Unit =
    expectBytes(closeFrame(closeCode, mask))

  def expectNetworkData(length: Int): ByteString = expectBytes(length)
  def expectNetworkData(data: ByteString): Unit = expectBytes(data)

  def expectFrameOnNetwork(opcode: Opcode, data: ByteString, fin: Boolean): Unit = {
    expectFrameHeaderOnNetwork(opcode, data.size, fin)
    expectNetworkData(data)
  }
  def expectMaskedFrameOnNetwork(opcode: Opcode, data: ByteString, fin: Boolean): Unit = {
    val Some(mask) = expectFrameHeaderOnNetwork(opcode, data.size, fin)
    val masked = maskedBytes(data, mask)._1
    expectNetworkData(masked)
  }

  def expectMaskedCloseFrame(closeCode: Int): Unit =
    expectMaskedFrameOnNetwork(Protocol.Opcode.Close, closeFrameData(closeCode), fin = true)

  /** Returns the mask if any is available */
  def expectFrameHeaderOnNetwork(opcode: Opcode, length: Long, fin: Boolean): Option[Int] = {
    val (op, l, f, m) = expectFrameHeaderOnNetwork()
    op shouldEqual opcode
    l shouldEqual length
    f shouldEqual fin
    m
  }
  def expectFrameHeaderOnNetwork(): (Opcode, Long, Boolean, Option[Int]) = {
    val header = expectNetworkData(2)

    val fin = (header(0) & Protocol.FIN_MASK) != 0
    val op = header(0) & Protocol.OP_MASK

    val hasMask = (header(1) & Protocol.MASK_MASK) != 0
    val length7 = header(1) & Protocol.LENGTH_MASK
    val length = length7 match {
      case 126 =>
        val length16Bytes = expectNetworkData(2)
        (length16Bytes(0) & 0xFF) << 8 | (length16Bytes(1) & 0xFF) << 0
      case 127 =>
        val length64Bytes = expectNetworkData(8)
        (length64Bytes(0) & 0xFF).toLong << 56 |
        (length64Bytes(1) & 0xFF).toLong << 48 |
        (length64Bytes(2) & 0xFF).toLong << 40 |
        (length64Bytes(3) & 0xFF).toLong << 32 |
        (length64Bytes(4) & 0xFF).toLong << 24 |
        (length64Bytes(5) & 0xFF).toLong << 16 |
        (length64Bytes(6) & 0xFF).toLong << 8 |
        (length64Bytes(7) & 0xFF).toLong << 0
      case x => x
    }
    val mask =
      if (hasMask) {
        val maskBytes = expectNetworkData(4)
        val mask =
          (maskBytes(0) & 0xFF) << 24 |
          (maskBytes(1) & 0xFF) << 16 |
          (maskBytes(2) & 0xFF) << 8 |
          (maskBytes(3) & 0xFF) << 0
        Some(mask)
      } else None

    (Opcode.forCode(op.toByte), length, fin, mask)
  }
}
