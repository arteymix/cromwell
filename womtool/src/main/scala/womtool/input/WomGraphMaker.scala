package womtool.input

import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import common.Checked
import common.validation.Validation._
import cromwell.core.path.Path
import cromwell.languages.LanguageFactory
import cromwell.languages.util.ImportResolver._
import languages.cwl.CwlV1_0LanguageFactory
import languages.wdl.biscayne.WdlBiscayneLanguageFactory
import languages.wdl.draft2.WdlDraft2LanguageFactory
import languages.wdl.draft3.WdlDraft3LanguageFactory
import wom.executable.WomBundle
import wom.expression.NoIoFunctionSet
import wom.graph._

import scala.collection.JavaConverters._
import scala.util.Try

object WomGraphMaker {

  def getBundle(mainFile: Path, listDependencies: Boolean = false): Checked[(WomBundle, Option[RootWorkflowResolvedImports])] =
    getBundleAndFactory(mainFile, listDependencies).map(x => (x._1, x._3))

  private def getBundleAndFactory(mainFile: Path, listDependencies: Boolean): Checked[(WomBundle, LanguageFactory, Option[RootWorkflowResolvedImports])] = {
    val rootWfResolvedImportsObj = new RootWorkflowResolvedImports

    lazy val importResolvers: List[ImportResolver] =
      DirectoryResolver.localFilesystemResolvers(Some(mainFile), rootWfResolvedImportsObj) :+ HttpResolver(rootWfResolvedImportsObj, relativeTo = None)

    readFile(mainFile.toAbsolutePath.pathAsString) flatMap { mainFileContents =>
      val languageFactory =
        List(
          new WdlDraft3LanguageFactory(ConfigFactory.empty()),
          new WdlBiscayneLanguageFactory(ConfigFactory.empty()),
          new CwlV1_0LanguageFactory(ConfigFactory.empty()))
          .find(_.looksParsable(mainFileContents))
          .getOrElse(new WdlDraft2LanguageFactory(ConfigFactory.empty()))

      val bundleWithImports = languageFactory.getWomBundleWithImports(mainFileContents, "{}", importResolvers, List(languageFactory), listDependencies)
      // Return the pair with the languageFactory
      bundleWithImports.map(bd => (bd._1, languageFactory, bd._2))
    }
  }

  def fromFiles(mainFile: Path, inputs: Option[Path], listDependencies: Boolean = false): Checked[(Graph, Option[RootWorkflowResolvedImports])] = {
    getBundleAndFactory(mainFile, listDependencies) flatMap { case (womBundle, languageFactory, workflowDependencies) =>
      inputs match {
        case None =>
          for {
            executableCallable <- womBundle.toExecutableCallable
          } yield (executableCallable.graph, workflowDependencies)
        case Some(inputsFile) =>
          for {
            inputsContents <- readFile(inputsFile.toAbsolutePath.pathAsString)
            validatedWomNamespace <- languageFactory.createExecutable(womBundle, inputsContents, NoIoFunctionSet)
          } yield (validatedWomNamespace.executable.graph, workflowDependencies)
      }
    }
  }

  private def readFile(filePath: String): Checked[String] = Try(Files.readAllLines(Paths.get(filePath)).asScala.mkString(System.lineSeparator())).toChecked

}
