package akka

import sbt._
import sbt.testing.Status

import java.nio.file.{Files, Path}

class GithubAnnotationTestsListener(baseDir: File, sourceDirs: Seq[File]) extends TestsListener {
  println(s"GHA Test listener created with bas:${baseDir} sources: [${sourceDirs.mkString(", ")}]")
  override def doInit(): Unit = ()//println("doInit")

  override def doComplete(finalResult: TestResult): Unit = ()//println(s"doComplete: $finalResult")

  override def startGroup(name: String): Unit = println(s"::group::$name")

  override def testEvent(event: TestEvent): Unit = {
    //println(s"testEvent(${event.result}, ${event.detail.map(eventString).mkString("\n")})")

    if (event.result.contains(TestResult.Failed)) {
      val failed = event.detail.filter(_.status() == Status.Failure)
      //println(s"Test failed: ${event.det}")
      failed.foreach { t =>
        if (t.throwable().isDefined) {
          val throwable =t.throwable().get()
          val ele = throwable.getStackTrace.takeWhile(!_.getClassName.endsWith("OutcomeOf")).last
          val firstLine = throwable.getMessage.split("\n").head
          println(s"::error file=${findFile(ele.getFileName).getOrElse(ele.getFileName)},line=${ele.getLineNumber}::$firstLine")
          //s"${ele.getClassName} ${ele.getFileName} ${ele.getMethodName} ${ele.getLineNumber} ${findFile(ele.getFileName)}"
        }
        //println(s"Test failed: ${t.fullyQualifiedName()} ${t.selector()} $line")

      }
    }
  }

  def eventString(event: sbt.testing.Event): String = {
    import event._
    if (throwable.isDefined) throwable.get.printStackTrace()
    s"$fullyQualifiedName -> $status -> $throwable}"
  }

  def findFile(fileName: String): Option[File] = {
    def findIn(base: File): Seq[File] = {
      Files.find(base.toPath, 20, (p, _) => p.toFile.getName == fileName).toArray.toSeq.map(p => baseDir.toPath.relativize(p.asInstanceOf[Path]).toFile)
    }
    sourceDirs.filter(_.exists()).flatMap(findIn(_)).headOption
  }

  override def endGroup(name: String, t: Throwable): Unit = println("::endgroup::")

  override def endGroup(name: String, result: TestResult): Unit = println("::endgroup::")

  def println(str: String): Unit = {
    scala.Console.println(str)
    scala.Console.flush()
  }
}
