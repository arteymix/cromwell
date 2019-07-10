package womtool.validate

import cromwell.core.path.Path
import cromwell.languages.util.ImportResolver.RootWorkflowResolvedImports
import womtool.WomtoolMain.{SuccessfulTermination, Termination, UnsuccessfulTermination}
import womtool.input.WomGraphMaker

object Validate {

  def validate(main: Path, inputs: Option[Path], listDependencies: Boolean): Termination = {

    def validationSuccessMsg(workflowResolvedImports: Option[RootWorkflowResolvedImports]): String = {
      val successMsg = "Successfully Validated!"
      val dependenciesMsg = if (listDependencies) {
        val msgPrefix = "\nList of Workflow dependencies are:\n"
        val dependenciesList = workflowResolvedImports match {
          case Some(imports) => imports.getResolvedImportsList.mkString("\n")
          case None => "None"
        }
        msgPrefix + dependenciesList
      } else ""

      successMsg + dependenciesMsg
    }

    if (inputs.isDefined) {
      WomGraphMaker.fromFiles(main, inputs, listDependencies) match {
        case Right(v) => SuccessfulTermination(validationSuccessMsg(v._2))
        case Left(errors) => UnsuccessfulTermination(errors.toList.mkString(System.lineSeparator))
      }
    } else {
      WomGraphMaker.getBundle(main, listDependencies) match {
        case Right(v) => SuccessfulTermination(validationSuccessMsg(v._2))
        case Left(errors) => UnsuccessfulTermination(errors.toList.mkString(System.lineSeparator))
      }
    }
  }
}
