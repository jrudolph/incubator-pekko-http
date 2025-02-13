/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, derived from Akka.
 */

/*
 * Copyright (C) 2017-2022 Lightbend Inc. <https://www.lightbend.com>
 *
 * Copied and adapted from akka-remote
 * https://github.com/akka/akka/blob/c90121485fcfc44a3cee62a0c638e1982d13d812/akka-remote/src/main/scala/akka/remote/artery/StageLogging.scala
 */

package org.apache.pekko.http.impl.util

import org.apache.pekko
import pekko.annotation.InternalApi
import pekko.stream.stage.GraphStageLogic
import pekko.event.LoggingAdapter
import pekko.stream.ActorMaterializer
import pekko.event.NoLogging

// TODO Try to reconcile with what Akka provides in StageLogging.
// We thought this could be removed when https://github.com/akka/akka/issues/18793 had been implemented
// but we need a few more changes to be able to override the default logger. So for now we keep it here.
/**
 * INTERNAL API
 */
@InternalApi
private[pekko] trait StageLoggingWithOverride { self: GraphStageLogic =>
  def logOverride: LoggingAdapter = DefaultNoLogging

  private var _log: LoggingAdapter = null

  protected def logSource: Class[_] = this.getClass

  def log: LoggingAdapter = {
    // only used in StageLogic, i.e. thread safe
    _log match {
      case null =>
        _log =
          logOverride match {
            case DefaultNoLogging =>
              materializer match {
                case a: ActorMaterializer => pekko.event.Logging(a.system, logSource)
                case _                    => NoLogging
              }
            case x => x
          }
      case _ =>
    }
    _log
  }
}

/**
 * INTERNAL API
 *
 * A copy of NoLogging that can be used as a place-holder for "logging not explicitly specified".
 * It can be matched on to be overridden with default behavior.
 */
@InternalApi
private[pekko] object DefaultNoLogging extends LoggingAdapter {

  /**
   * Java API to return the reference to NoLogging
   * @return The NoLogging instance
   */
  def getInstance = this

  final override def isErrorEnabled = false
  final override def isWarningEnabled = false
  final override def isInfoEnabled = false
  final override def isDebugEnabled = false

  final protected override def notifyError(message: String): Unit = ()
  final protected override def notifyError(cause: Throwable, message: String): Unit = ()
  final protected override def notifyWarning(message: String): Unit = ()
  final protected override def notifyInfo(message: String): Unit = ()
  final protected override def notifyDebug(message: String): Unit = ()
}
