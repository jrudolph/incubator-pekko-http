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

import java.util.Random

import org.apache.pekko
import pekko.NotUsed
import pekko.annotation.InternalApi
import pekko.stream.{ Attributes, FlowShape, Inlet, Outlet }
import pekko.stream.scaladsl.{ BidiFlow, Flow, Keep }
import pekko.stream.stage._

/**
 * Implements WebSocket Frame masking.
 *
 * INTERNAL API
 */
@InternalApi
private[http] object Masking {
  def apply(serverSide: Boolean, maskRandom: () => Random): BidiFlow[/* net in */ FrameEvent,
    /* app out */ FrameEventOrError, /* app in */ FrameEvent, /* net out */ FrameEvent, NotUsed] =
    BidiFlow.fromFlowsMat(unmaskIf(serverSide), maskIf(!serverSide, maskRandom))(Keep.none)

  def maskIf(condition: Boolean, maskRandom: () => Random): Flow[FrameEvent, FrameEvent, NotUsed] =
    if (condition) {
      Flow[FrameEvent]
        .via(new Masking(maskRandom())) // new random per materialization
        .map {
          case f: FrameEvent  => f
          case FrameError(ex) => throw ex
        }
    } else Flow[FrameEvent]

  def unmaskIf(condition: Boolean): Flow[FrameEvent, FrameEventOrError, NotUsed] =
    if (condition) Flow[FrameEvent].via(Unmasking)
    else Flow[FrameEvent]

  private final class Masking(random: Random) extends Masker {
    def extractMask(header: FrameHeader): Int = random.nextInt()
    def setNewMask(header: FrameHeader, mask: Int): FrameHeader = {
      if (header.mask.isDefined) throw new ProtocolException("Frame mustn't already be masked")
      header.copy(mask = Some(mask))
    }
    override def toString: String = s"Masking($random)"
  }

  private object Unmasking extends Masker {
    def extractMask(header: FrameHeader): Int = header.mask match {
      case Some(mask) => mask
      case None       => throw new ProtocolException("Frame wasn't masked")
    }
    def setNewMask(header: FrameHeader, mask: Int): FrameHeader = header.copy(mask = None)
    override def toString: String = "Unmasking"
  }

  /** Implements both masking and unmasking which is mostly symmetric (because of XOR) */
  private abstract class Masker extends GraphStage[FlowShape[FrameEvent, FrameEventOrError]] {
    def extractMask(header: FrameHeader): Int
    def setNewMask(header: FrameHeader, mask: Int): FrameHeader

    val in = Inlet[FrameEvent](s"${toString}-in")
    val out = Outlet[FrameEventOrError](s"${toString}-out")
    override val shape: FlowShape[FrameEvent, FrameEventOrError] = FlowShape(in, out)

    override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
      new GraphStageLogic(shape) with OutHandler with InHandler {

        def onPush(): Unit = grab(in) match {
          case start @ FrameStart(header, data) =>
            try {
              val mask = extractMask(header)

              val (masked, newMask) = FrameEventParser.mask(data, mask)
              if (!start.lastPart) {
                setHandler(in, runningHandler(newMask, this))
              }
              push(out, start.copy(header = setNewMask(header, mask), data = masked))
            } catch {
              case p: ProtocolException => {
                setHandler(in, doneHandler)
                push(out, FrameError(p))
              }
            }
          case _: FrameData => fail(out, new IllegalStateException("unexpected FrameData (need FrameStart first)"))
        }

        private def doneHandler = new InHandler {
          override def onPush(): Unit = pull(in)
        }

        private def runningHandler(initialMask: Int, nextState: InHandler): InHandler = new InHandler {
          var mask = initialMask

          override def onPush(): Unit = {
            val part = grab(in)
            if (part.lastPart) {
              setHandler(in, nextState)
            }

            val (masked, newMask) = FrameEventParser.mask(part.data, mask)
            mask = newMask
            push(out, part.withData(data = masked))
          }
        }

        def onPull(): Unit = pull(in)

        setHandler(in, this)
        setHandler(out, this)

      }
  }
}
