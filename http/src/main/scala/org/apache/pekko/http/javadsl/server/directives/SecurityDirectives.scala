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

import java.util.Optional
import java.util.concurrent.CompletionStage
import java.util.function.{ Function => JFunction }
import java.util.function.Supplier

import scala.compat.java8.FutureConverters._
import scala.compat.java8.OptionConverters._
import org.apache.pekko
import pekko.http.javadsl.model.headers.HttpChallenge
import pekko.http.javadsl.model.headers.HttpCredentials
import pekko.http.javadsl.server.{ RequestContext, Route }
import pekko.http.scaladsl
import pekko.http.scaladsl.server.{ Directives => D }

import scala.concurrent.{ ExecutionContextExecutor, Future }

object SecurityDirectives {

  /**
   * Represents HTTP Basic or OAuth2 authentication credentials supplied with a request.
   */
  case class ProvidedCredentials(private val asScala: scaladsl.server.directives.Credentials.Provided) {

    /**
     * The username or token provided with the credentials
     */
    def identifier: String = asScala.identifier

    /**
     * Safely compares the passed in `secret` with the received secret part of the Credentials.
     * Use of this method instead of manual String equality testing is recommended in order to guard against timing attacks.
     *
     * See also [[pekko.http.impl.util.EnhancedString#secure_==]], for more information.
     */
    def verify(secret: String): Boolean = asScala.verify(secret)
  }

  private def toJava(cred: scaladsl.server.directives.Credentials): Optional[ProvidedCredentials] = cred match {
    case provided: scaladsl.server.directives.Credentials.Provided => Optional.of(ProvidedCredentials(provided))
    case _                                                         => Optional.empty()
  }
}

abstract class SecurityDirectives extends SchemeDirectives {
  import SecurityDirectives._
  import pekko.http.impl.util.JavaMapping.Implicits._

  /**
   * Extracts the potentially present [[HttpCredentials]] provided with the request's [[pekko.http.javadsl.model.headers.Authorization]] header.
   */
  def extractCredentials(inner: JFunction[Optional[HttpCredentials], Route]): Route = RouteAdapter {
    D.extractCredentials { cred =>
      inner.apply(cred.map(_.asJava).asJava).delegate // TODO attempt to not need map()
    }
  }

  /**
   * Wraps the inner route with Http Basic authentication support using a given `Authenticator[T]`.
   * The given authenticator determines whether the credentials in the request are valid
   * and, if so, which user object to supply to the inner route.
   *
   * Authentication is required in this variant, i.e. the request is rejected if [authenticator] returns Optional.empty.
   */
  def authenticateBasic[T](realm: String, authenticator: JFunction[Optional[ProvidedCredentials], Optional[T]],
      inner: JFunction[T, Route]): Route = RouteAdapter {
    D.authenticateBasic(realm, c => authenticator.apply(toJava(c)).asScala) { t =>
      inner.apply(t).delegate
    }
  }

  /**
   * Wraps the inner route with Http Basic authentication support.
   * The given authenticator determines whether the credentials in the request are valid
   * and, if so, which user object to supply to the inner route.
   *
   * Authentication is required in this variant, i.e. the request is rejected if [authenticator] returns Optional.empty.
   */
  def authenticateBasicPF[T](realm: String, authenticator: PartialFunction[Optional[ProvidedCredentials], T],
      inner: JFunction[T, Route]): Route = RouteAdapter {
    def pf: PartialFunction[scaladsl.server.directives.Credentials, Option[T]] = {
      case c => Option(authenticator.applyOrElse(toJava(c), (_: Any) => null.asInstanceOf[T]))
    }

    D.authenticateBasic(realm, pf) { t =>
      inner.apply(t).delegate
    }
  }

  /**
   * Wraps the inner route with Http Basic authentication support.
   * The given authenticator determines whether the credentials in the request are valid
   * and, if so, which user object to supply to the inner route.
   *
   * Authentication is required in this variant, i.e. the request is rejected if [authenticator] returns Optional.empty.
   */
  def authenticateBasicPFAsync[T](realm: String,
      authenticator: PartialFunction[Optional[ProvidedCredentials], CompletionStage[T]],
      inner: JFunction[T, Route]): Route = RouteAdapter {
    def pf(implicit ec: ExecutionContextExecutor)
        : PartialFunction[scaladsl.server.directives.Credentials, Future[Option[T]]] = {
      case credentials =>
        val jCredentials = toJava(credentials)
        if (authenticator.isDefinedAt(jCredentials)) {
          authenticator(jCredentials).toScala.map(Some(_))
        } else {
          Future.successful(None)
        }
    }

    D.extractExecutionContext { implicit ec =>
      D.authenticateBasicAsync(realm, pf) { t =>
        inner.apply(t).delegate
      }
    }
  }

  /**
   * Wraps the inner route with Http Basic authentication support using a given `Authenticator[T]`.
   * The given authenticator determines whether the credentials in the request are valid
   * and, if so, which user object to supply to the inner route.
   *
   * Authentication is optional in this variant.
   */
  @CorrespondsTo("authenticateBasic")
  def authenticateBasicOptional[T](realm: String, authenticator: JFunction[Optional[ProvidedCredentials], Optional[T]],
      inner: JFunction[Optional[T], Route]): Route = RouteAdapter {
    D.authenticateBasic(realm, c => authenticator.apply(toJava(c)).asScala).optional { t =>
      inner.apply(t.asJava).delegate
    }
  }

  /**
   * Wraps the inner route with Http Basic authentication support.
   * The given authenticator determines whether the credentials in the request are valid
   * and, if so, which user object to supply to the inner route.
   *
   * Authentication is required in this variant, i.e. the request is rejected if [authenticator] returns Optional.empty.
   */
  def authenticateBasicAsync[T](realm: String,
      authenticator: JFunction[Optional[ProvidedCredentials], CompletionStage[Optional[T]]],
      inner: JFunction[T, Route]): Route = RouteAdapter {
    D.extractExecutionContext { implicit ctx =>
      D.authenticateBasicAsync(realm, c => authenticator.apply(toJava(c)).toScala.map(_.asScala)) { t =>
        inner.apply(t).delegate
      }
    }
  }

  /**
   * Wraps the inner route with Http Basic authentication support.
   * The given authenticator determines whether the credentials in the request are valid
   * and, if so, which user object to supply to the inner route.
   *
   * Authentication is optional in this variant.
   */
  @CorrespondsTo("authenticateBasicAsync")
  def authenticateBasicAsyncOptional[T](realm: String,
      authenticator: JFunction[Optional[ProvidedCredentials], CompletionStage[Optional[T]]],
      inner: JFunction[Optional[T], Route]): Route = RouteAdapter {
    D.extractExecutionContext { implicit ctx =>
      D.authenticateBasicAsync(realm, c => authenticator.apply(toJava(c)).toScala.map(_.asScala)).optional { t =>
        inner.apply(t.asJava).delegate
      }
    }
  }

  /**
   * A directive that wraps the inner route with OAuth2 Bearer Token authentication support.
   * The given authenticator determines whether the credentials in the request are valid
   * and, if so, which user object to supply to the inner route.
   *
   * Authentication is required in this variant, i.e. the request is rejected if [authenticator] returns Optional.empty.
   */
  def authenticateOAuth2[T](realm: String, authenticator: JFunction[Optional[ProvidedCredentials], Optional[T]],
      inner: JFunction[T, Route]): Route = RouteAdapter {
    D.authenticateOAuth2(realm, c => authenticator.apply(toJava(c)).asScala) { t =>
      inner.apply(t).delegate
    }
  }

  /**
   * A directive that wraps the inner route with OAuth2 Bearer Token authentication support.
   * The given authenticator determines whether the credentials in the request are valid
   * and, if so, which user object to supply to the inner route.
   *
   * Authentication is optional in this variant.
   */
  @CorrespondsTo("authenticateOAuth2")
  def authenticateOAuth2Optional[T](realm: String, authenticator: JFunction[Optional[ProvidedCredentials], Optional[T]],
      inner: JFunction[Optional[T], Route]): Route = RouteAdapter {
    D.authenticateOAuth2(realm, c => authenticator.apply(toJava(c)).asScala).optional { t =>
      inner.apply(t.asJava).delegate
    }
  }

  /**
   * A directive that wraps the inner route with OAuth2 Bearer Token authentication support.
   * The given authenticator determines whether the credentials in the request are valid
   * and, if so, which user object to supply to the inner route.
   *
   * Authentication is required in this variant, i.e. the request is rejected if [authenticator] returns Optional.empty.
   */
  def authenticateOAuth2Async[T](realm: String,
      authenticator: JFunction[Optional[ProvidedCredentials], CompletionStage[Optional[T]]],
      inner: JFunction[T, Route]): Route = RouteAdapter {
    D.extractExecutionContext { implicit ctx =>
      D.authenticateOAuth2Async(realm, c => authenticator.apply(toJava(c)).toScala.map(_.asScala)) { t =>
        inner.apply(t).delegate
      }
    }
  }

  /**
   * A directive that wraps the inner route with OAuth2 Bearer Token authentication support.
   * The given authenticator determines whether the credentials in the request are valid
   * and, if so, which user object to supply to the inner route.
   *
   * Authentication is optional in this variant.
   */
  @CorrespondsTo("authenticateOAuth2Async")
  def authenticateOAuth2AsyncOptional[T](realm: String,
      authenticator: JFunction[Optional[ProvidedCredentials], CompletionStage[Optional[T]]],
      inner: JFunction[Optional[T], Route]): Route = RouteAdapter {
    D.extractExecutionContext { implicit ctx =>
      D.authenticateOAuth2Async(realm, c => authenticator.apply(toJava(c)).toScala.map(_.asScala)).optional { t =>
        inner.apply(t.asJava).delegate
      }
    }
  }

  /**
   * Lifts an authenticator function into a directive. The authenticator function gets passed in credentials from the
   * [[pekko.http.javadsl.model.headers.Authorization]] header of the request. If the function returns `Right(user)` the user object is provided
   * to the inner route. If the function returns `Left(challenge)` the request is rejected with an
   * [[pekko.http.javadsl.server.AuthenticationFailedRejection]] that contains this challenge to be added to the response.
   */
  def authenticateOrRejectWithChallenge[T](
      authenticator: JFunction[Optional[HttpCredentials], CompletionStage[Either[HttpChallenge, T]]],
      inner: JFunction[T, Route]): Route = RouteAdapter {
    D.extractExecutionContext { implicit ctx =>
      val scalaAuthenticator = { cred: Option[scaladsl.model.headers.HttpCredentials] =>
        authenticator.apply(cred.map(_.asJava).asJava).toScala.map(_.left.map(_.asScala))
      }

      D.authenticateOrRejectWithChallenge(scalaAuthenticator) { t =>
        inner.apply(t).delegate
      }
    }
  }

  /**
   * Lifts an authenticator function into a directive. Same as `authenticateOrRejectWithChallenge`
   * but only applies the authenticator function with a certain type of credentials.
   */
  def authenticateOrRejectWithChallenge[C <: HttpCredentials, T](
      c: Class[C],
      authenticator: JFunction[Optional[C], CompletionStage[Either[HttpChallenge, T]]],
      inner: JFunction[T, Route]): Route = RouteAdapter {
    D.extractExecutionContext { implicit ctx =>
      val scalaAuthenticator = { cred: Option[scaladsl.model.headers.HttpCredentials] =>
        authenticator.apply(cred.filter(c.isInstance).map(_.asJava).asJava.asInstanceOf[Optional[C]]).toScala.map(
          _.left.map(_.asScala)) // TODO make sure cast is safe
      }

      D.authenticateOrRejectWithChallenge(scalaAuthenticator) { t =>
        inner.apply(t).delegate
      }
    }
  }

  /**
   * Applies the given authorization check to the request.
   * If the check fails the route is rejected with an [[pekko.http.javadsl.server.AuthorizationFailedRejection]].
   */
  def authorize(check: Supplier[Boolean], inner: Supplier[Route]): Route = RouteAdapter {
    D.authorize(check.get()) {
      inner.get().delegate
    }
  }

  /**
   * Applies the given authorization check to the request.
   * If the check fails the route is rejected with an [[pekko.http.javadsl.server.AuthorizationFailedRejection]].
   */
  @CorrespondsTo("authorize")
  def authorizeWithRequestContext(
      check: pekko.japi.function.Function[RequestContext, Boolean], inner: Supplier[Route]): Route = RouteAdapter {
    D.authorize(rc => check(RequestContext.wrap(rc)))(inner.get().delegate)
  }

  /**
   * Applies the given authorization check to the request.
   * If the check fails the route is rejected with an [[pekko.http.javadsl.server.AuthorizationFailedRejection]].
   */
  def authorizeAsync(check: Supplier[CompletionStage[Boolean]], inner: Supplier[Route]): Route = RouteAdapter {
    D.authorizeAsync(check.get().toScala) {
      inner.get().delegate
    }
  }

  /**
   * Asynchronous version of [[authorize]].
   * If the [[CompletionStage]] fails or is completed with `false`
   * authorization fails and the route is rejected with an [[pekko.http.javadsl.server.AuthorizationFailedRejection]].
   */
  @CorrespondsTo("authorizeAsync")
  def authorizeAsyncWithRequestContext(check: pekko.japi.function.Function[RequestContext, CompletionStage[Boolean]],
      inner: Supplier[Route]): Route = RouteAdapter {
    D.authorizeAsync(rc => check(RequestContext.wrap(rc)).toScala)(inner.get().delegate)
  }
}
