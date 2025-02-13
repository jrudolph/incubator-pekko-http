/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, derived from Akka.
 */

/*
 * Copyright (C) 2015-2022 Lightbend Inc. <https://www.lightbend.com>
 */

package docs.http.javadsl.server.directives;

import org.junit.Test;

import org.apache.pekko.http.javadsl.model.HttpMethods;
import org.apache.pekko.http.javadsl.model.HttpRequest;
import org.apache.pekko.http.javadsl.model.StatusCodes;
import org.apache.pekko.http.javadsl.server.Route;
import org.apache.pekko.http.javadsl.testkit.JUnitRouteTest;

//#delete
import static org.apache.pekko.http.javadsl.server.Directives.complete;
import static org.apache.pekko.http.javadsl.server.Directives.delete;

//#delete
//#head
import static org.apache.pekko.http.javadsl.server.Directives.complete;
import static org.apache.pekko.http.javadsl.server.Directives.head;

//#head
//#options
import static org.apache.pekko.http.javadsl.server.Directives.complete;
import static org.apache.pekko.http.javadsl.server.Directives.options;

//#options
//#patch
import static org.apache.pekko.http.javadsl.server.Directives.complete;
import static org.apache.pekko.http.javadsl.server.Directives.patch;

//#patch
//#post
import static org.apache.pekko.http.javadsl.server.Directives.complete;
import static org.apache.pekko.http.javadsl.server.Directives.post;

//#post
//#put
import static org.apache.pekko.http.javadsl.server.Directives.complete;
import static org.apache.pekko.http.javadsl.server.Directives.put;

//#put
//#method-example
import static org.apache.pekko.http.javadsl.server.Directives.complete;
import static org.apache.pekko.http.javadsl.server.Directives.method;

//#method-example
//#extractMethod
import static org.apache.pekko.http.javadsl.server.Directives.complete;
import static org.apache.pekko.http.javadsl.server.Directives.get;
import static org.apache.pekko.http.javadsl.server.Directives.extractMethod;

//#extractMethod
//#overrideMethodWithParameter
import static org.apache.pekko.http.javadsl.server.Directives.complete;
import static org.apache.pekko.http.javadsl.server.Directives.get;
import static org.apache.pekko.http.javadsl.server.Directives.post;
import static org.apache.pekko.http.javadsl.server.Directives.overrideMethodWithParameter;

//#overrideMethodWithParameter

public class MethodDirectivesExamplesTest extends JUnitRouteTest {
  @Test
  public void testDelete() {
    //#delete
    final Route route = delete(() -> complete("This is a DELETE request."));

    testRoute(route).run(HttpRequest.DELETE("/")).assertEntity(
        "This is a DELETE request.");
    //#delete
  }

  @Test
  public void testGet() {
    //#get
    final Route route = get(() -> complete("This is a GET request."));

    testRoute(route).run(HttpRequest.GET("/")).assertEntity(
        "This is a GET request.");
    //#get
  }

  @Test
  public void testHead() {
    //#head
    final Route route = head(() -> complete("This is a HEAD request."));

    testRoute(route).run(HttpRequest.HEAD("/")).assertEntity(
        "This is a HEAD request.");
    //#head
  }

  @Test
  public void testOptions() {
    //#options
    final Route route = options(() -> complete("This is a OPTIONS request."));

    testRoute(route).run(HttpRequest.OPTIONS("/")).assertEntity(
        "This is a OPTIONS request.");
    //#options
  }

  @Test
  public void testPatch() {
    //#patch
    final Route route = patch(() -> complete("This is a PATCH request."));

    testRoute(route).run(HttpRequest.PATCH("/").withEntity("patch content"))
        .assertEntity("This is a PATCH request.");
    //#patch
  }

  @Test
  public void testPost() {
    //#post
    final Route route = post(() -> complete("This is a POST request."));

    testRoute(route).run(HttpRequest.POST("/").withEntity("post content"))
        .assertEntity("This is a POST request.");
    //#post
  }

  @Test
  public void testPut() {
    //#put
    final Route route = put(() -> complete("This is a PUT request."));

    testRoute(route).run(HttpRequest.PUT("/").withEntity("put content"))
        .assertEntity("This is a PUT request.");
    //#put
  }

  @Test
  public void testMethodExample() {
    //#method-example
    final Route route = method(HttpMethods.PUT,
        () -> complete("This is a PUT request."));

    testRoute(route).run(HttpRequest.PUT("/").withEntity("put content"))
        .assertEntity("This is a PUT request.");

    testRoute(route).run(HttpRequest.GET("/")).assertStatusCode(
        StatusCodes.METHOD_NOT_ALLOWED);
    //#method-example
  }

  @Test
  public void testExtractMethodExample() {
    //#extractMethod

    final Route route = concat(
        get(() ->
            complete("This is a GET request.")
        ),
        extractMethod(method ->
            complete("This " + method.value() + " request, clearly is not a GET!")
        )
    );

    testRoute(route).run(HttpRequest.GET("/")).assertEntity(
        "This is a GET request.");

    testRoute(route).run(HttpRequest.PUT("/").withEntity("put content"))
        .assertEntity("This PUT request, clearly is not a GET!");

    testRoute(route).run(HttpRequest.HEAD("/")).assertEntity(
        "This HEAD request, clearly is not a GET!");
    //#extractMethod
  }

  @Test
  public void testOverrideMethodWithParameter() {
    //#overrideMethodWithParameter

    final Route route = concat(
        overrideMethodWithParameter("method", () ->
          concat(
            get(() -> complete("This looks like a GET request.")),
            post(() -> complete("This looks like a POST request."))
          )
        )
    );


    // tests:
    testRoute(route).run(HttpRequest.GET("/?method=POST")).assertEntity(
        "This looks like a POST request.");

    testRoute(route).run(HttpRequest.POST("/?method=get"))
        .assertEntity("This looks like a GET request.");

    testRoute(route).run(HttpRequest.GET("/?method=hallo")).assertEntity(
        "The server either does not recognize the request method, or it lacks the ability to fulfill the request.");

    //#overrideMethodWithParameter
  }
}
