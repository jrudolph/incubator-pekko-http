package sbt

import Keys._
import xsbti.{Position, Severity}

import java.nio.file.{Files, Path}
import java.util.Optional

object TestPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  object autoImport {
    val savedReporter = settingKey[xsbti.Reporter]("Saved reporter that collects compilation failures.")
    val problems = taskKey[Array[xsbti.Problem]]("Problems reported during compilation.")
  }
  import autoImport._
  override def projectSettings = Seq(
    savedReporter := new CollectingReporter((ProjectRef(file("."), "pekko-http") / baseDirectory).value, (Test / sourceDirectories).value),
    Compile / compile / compilerReporter  := savedReporter.value,
    problems := savedReporter.value.problems
  )
}

class CollectingReporter(baseDir: File, sourceDirs: Seq[File]) extends xsbti.Reporter {
  val buffer = collection.mutable.ArrayBuffer.empty[xsbti.Problem]

  def reset(): Unit = {
    System.err.println(s"DEBUGME: Clearing errors: $buffer")
    buffer.clear()
  }
  def hasErrors: Boolean = buffer.exists(_.severity == Severity.Error)
  def hasWarnings: Boolean = buffer.exists(_.severity == Severity.Warn)
  def printSummary(): Unit = ()
  def problems: Array[xsbti.Problem] = buffer.toArray

  def log(problem: xsbti.Problem): Unit =
    log(problem.position, problem.message, problem.severity)

  /** Logs a message. */
  def log(pos: xsbti.Position, msg: String, sev: xsbti.Severity): Unit = {
    object MyProblem extends xsbti.Problem {
      def category: String = ""
      def severity: Severity = sev
      def message: String = msg
      def position: Position = pos
      override def toString = s"$position:$severity: $message"
    }
    System.err.println(s"DEBUGME: Logging: $MyProblem")
    buffer.append(MyProblem)

    if (sev == Severity.Warn && pos.sourceFile.isPresent) {
      val file = baseDir.toPath.relativize(pos.sourceFile.get().toPath).toFile
      val message = msg.split("\n").head
      def e(key: String, value: Optional[Integer]): String =
        value.map[String](v => s",$key=$v").orElse("")

      println(s"::warning file=${file}${e("line", pos.line())}${e("col", pos.startColumn())}${e("endColumn", pos.endColumn())}}::$message")
    }
  }

  def findFile(fileName: String): Option[File] = {
    def findIn(base: File): Seq[File] = {
      Files.find(base.toPath, 20, (p, _) => p.toFile.getName == fileName).toArray.toSeq.map(p => baseDir.toPath.relativize(p.asInstanceOf[Path]).toFile)
    }
    sourceDirs.filter(_.exists()).flatMap(findIn(_)).headOption
  }

  /** Reports a comment. */
  def comment(pos: xsbti.Position, msg: String): Unit = ()

  override def toString = "CollectingReporter"
  }