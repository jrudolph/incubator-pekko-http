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

package org.apache.pekko.http.javadsl.server.directives

import java.util.concurrent.CompletionException
import java.util.concurrent.CompletionStage
import java.util.function.{ Function => JFunction }
import java.util.function.Supplier

import org.apache.pekko
import pekko.http.javadsl.marshalling.Marshaller
import pekko.http.javadsl.model.RequestEntity

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import pekko.http.javadsl.server.Route
import pekko.http.scaladsl.server.directives.{ CompleteOrRecoverWithMagnet, FutureDirectives => D }
import pekko.pattern.CircuitBreaker

abstract class FutureDirectives extends FormFieldDirectives {

  /**
   * "Unwraps" a `CompletionStage<T>` and runs the inner route after future
   * completion with the future's value as an extraction of type `Try<T>`.
   *
   * @group future
   */
  def onComplete[T](f: Supplier[CompletionStage[T]], inner: JFunction[Try[T], Route]) = RouteAdapter {
    D.onComplete(f.get.toScala.recover(unwrapCompletionException)) { value =>
      inner(value).delegate
    }
  }

  /**
   * "Unwraps" a `CompletionStage<T>` and runs the inner route after future
   * completion with the future's value as an extraction of type `Try<T>`.
   *
   * @group future
   */
  def onComplete[T](cs: CompletionStage[T], inner: JFunction[Try[T], Route]) = RouteAdapter {
    D.onComplete(cs.toScala.recover(unwrapCompletionException)) { value =>
      inner(value).delegate
    }
  }

  /**
   * "Unwraps" a `CompletionStage[T]` and runs the inner route after future
   * completion with the future's value as an extraction of type `T` if
   * the supplied `CircuitBreaker` is closed.
   *
   * If the supplied [[CircuitBreaker]] is open the request is rejected
   * with a [[pekko.http.javadsl.server.CircuitBreakerOpenRejection]].
   *
   * @group future
   */
  def onCompleteWithBreaker[T](breaker: CircuitBreaker, f: Supplier[CompletionStage[T]],
      inner: JFunction[Try[T], Route]) = RouteAdapter {
    D.onCompleteWithBreaker(breaker)(f.get.toScala.recover(unwrapCompletionException)) { value =>
      inner(value).delegate
    }
  }

  /**
   * "Unwraps" a `CompletionStage<T>` and runs the inner route after stage
   * completion with the stage's value as an extraction of type `T`.
   * If the stage fails its failure Throwable is bubbled up to the nearest
   * ExceptionHandler.
   *
   * @group future
   */
  def onSuccess[T](f: Supplier[CompletionStage[T]], inner: JFunction[T, Route]) = RouteAdapter {
    D.onSuccess(f.get.toScala.recover(unwrapCompletionException)) { value =>
      inner(value).delegate
    }
  }

  /**
   * "Unwraps" a `CompletionStage<T>` and runs the inner route after stage
   * completion with the stage's value as an extraction of type `T`.
   * If the stage fails its failure Throwable is bubbled up to the nearest
   * ExceptionHandler.
   *
   * @group future
   */
  def onSuccess[T](cs: CompletionStage[T], inner: JFunction[T, Route]) = RouteAdapter {
    D.onSuccess(cs.toScala.recover(unwrapCompletionException)) { value =>
      inner(value).delegate
    }
  }

  /**
   * "Unwraps" a `CompletionStage<T>` and runs the inner route when the stage has failed
   * with the stage's failure exception as an extraction of type `Throwable`.
   * If the completion stage succeeds the request is completed using the values marshaller
   * (This directive therefore requires a marshaller for the completion stage value type to be
   * provided.)
   *
   * @group future
   */
  def completeOrRecoverWith[T](f: Supplier[CompletionStage[T]], marshaller: Marshaller[T, RequestEntity],
      inner: JFunction[Throwable, Route]): Route = RouteAdapter {
    val magnet = CompleteOrRecoverWithMagnet(f.get.toScala)(Marshaller.asScalaEntityMarshaller(marshaller))
    D.completeOrRecoverWith(magnet) { ex => inner(ex).delegate }
  }

  // TODO: This might need to be raised as an issue to scala-java8-compat instead.
  // Right now, having this in Java:
  //     CompletableFuture.supplyAsync(() -> { throw new IllegalArgumentException("always failing"); })
  // will in fact fail the future with CompletionException.
  private def unwrapCompletionException[T]: PartialFunction[Throwable, T] = {
    case x: CompletionException if x.getCause ne null =>
      throw x.getCause
  }

}
