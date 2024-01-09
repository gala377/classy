import scala.util.CommandLineParser
import scala.sys.process._

import java.io.File

val command = Seq(
  "cargo",
  "run",
  "-p=classyc",
  "--"
)

@main
def main(file: String, forward_args: String*) = {
  println(s"Running example: $file")
  println(s"Forwarded args: $forward_args")
  val projectPath =
    System
      .getProperty("script.path")
      .split("/")
      .dropRight(2)
      .mkString("/")
  val examplesDir = s"$projectPath/examples"
  println(s"File to run: --file=$examplesDir/$file.clss")
  val process =
    Process(
      Seq(
        "cargo",
        "run",
        "-p=classyc",
        "--",
        s"--file=$examplesDir/$file.clss"
      ) ++ forward_args,
      java.io.File(projectPath),
      ("RUST_BACKTRACE", "1")
    ).!
  println(s"Process exited with code $process")
}
