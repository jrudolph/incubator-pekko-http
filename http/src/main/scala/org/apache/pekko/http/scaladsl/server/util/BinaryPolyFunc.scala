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

package org.apache.pekko.http.scaladsl.server.util

/**
 * Allows the definition of binary poly-functions (e.g. for folding over tuples).
 *
 * Note: the poly-function implementation seen here is merely a stripped down version of
 * what Miles Sabin made available with his awesome shapeless library. All credit goes to him!
 */
trait BinaryPolyFunc {
  def at[A, B] = new CaseBuilder[A, B]
  class CaseBuilder[A, B] {
    def apply[R](f: (A, B) => R) = new BinaryPolyFunc.Case[A, B, BinaryPolyFunc.this.type] {
      type Out = R
      def apply(a: A, b: B) = f(a, b)
    }
  }
}

object BinaryPolyFunc {
  sealed trait Case[A, B, Op] {
    type Out
    def apply(a: A, b: B): Out
  }
}
