package womtool.validate

import cromwell.core.path.Path
import womtool.WomtoolMain.{SuccessfulTermination, Termination, UnsuccessfulTermination}
import womtool.input.WomGraphMaker

object Validate {
  def validate(main: Path, inputs: Option[Path], listDependencies: Boolean): Termination = if (inputs.isDefined) {
    WomGraphMaker.fromFiles(main, inputs, listDependencies) match {
      case Right(v) => {
        val msg = s"Successfully Validated! The list of workflow dependencies are: ${v._2.mkString("\\n")}"
        SuccessfulTermination(msg)
      }
      case Left(errors) => UnsuccessfulTermination(errors.toList.mkString(System.lineSeparator))
    }
  } else {
    WomGraphMaker.getBundle(main, listDependencies) match {
      case Right(v) => {
        val msg = s"Successfully Validated! The list of workflow dependencies are: ${v._2.mkString("\\n")}"
        SuccessfulTermination(msg)
      }
      case Left(errors) => UnsuccessfulTermination(errors.toList.mkString(System.lineSeparator))
    }
  }
}
