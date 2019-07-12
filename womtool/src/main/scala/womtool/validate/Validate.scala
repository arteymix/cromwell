package womtool.validate

import cromwell.core.path.Path
import cromwell.languages.util.ImportResolver.RootWorkflowResolvedImports
import womtool.WomtoolMain.{SuccessfulTermination, Termination, UnsuccessfulTermination}
import womtool.input.WomGraphMaker

object Validate {

  def validate(main: Path, inputs: Option[Path], listDependencies: Boolean): Termination = {

    def workflowDependenciesMsg(workflowResolvedImports: Option[RootWorkflowResolvedImports]) = {
      val msgPrefix = "\nList of Workflow dependencies are:\n"
      val dependenciesList = workflowResolvedImports match {
        case Some(imports) => {
          val importsList = imports.getResolvedImportsList
          if (importsList.nonEmpty) importsList.mkString("\n")
          else "None"
        }
        case None => "None"
      }
      msgPrefix + dependenciesList
    }

    def validationSuccessMsg(workflowResolvedImports: Option[RootWorkflowResolvedImports]): String = {
      val successMsg = "Success!"
      val dependenciesMsg = if (listDependencies) workflowDependenciesMsg(workflowResolvedImports) else ""
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
