package womtool.validate

import cromwell.core.path.Path
import cromwell.languages.util.ImportResolver.ResolvedImportsStore
import womtool.WomtoolMain.{SuccessfulTermination, Termination, UnsuccessfulTermination}
import womtool.input.WomGraphMaker

object Validate {

  def validate(main: Path, inputs: Option[Path], listDependencies: Boolean): Termination = {

    def workflowDependenciesMsg(workflowResolvedImports: Option[ResolvedImportsStore]) = {
      val msgPrefix = "\nList of Workflow dependencies is:\n"
      val dependenciesList = workflowResolvedImports match {
        case Some(imports) => {
          val importsList = imports.getResolvedImportsSet
          if (importsList.nonEmpty) importsList.mkString("\n")
          else "None"
        }
        case None => "None"
      }
      msgPrefix + dependenciesList
    }

    def validationSuccessMsg(workflowResolvedImports: Option[ResolvedImportsStore]): String = {
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
