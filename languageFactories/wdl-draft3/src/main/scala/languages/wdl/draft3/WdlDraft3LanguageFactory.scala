package languages.wdl.draft3

import cats.data.EitherT.fromEither
import cats.effect.IO
import cats.instances.either._
import com.typesafe.config.Config
import common.Checked
import common.transforms.CheckedAtoB
import common.validation.IOChecked.IOChecked
import cromwell.core._
import cromwell.languages.util.ImportResolver._
import cromwell.languages.util.LanguageFactoryUtil
import cromwell.languages.{LanguageFactory, ValidatedWomNamespace}
import wdl.draft3.transforms.ast2wdlom._
import wdl.draft3.transforms.parsing._
import wdl.draft3.transforms.wdlom2wom._
import wdl.model.draft3.elements.FileElement
import wdl.transforms.base.wdlom2wom.WomBundleToWomExecutable._
import wdl.transforms.base.wdlom2wom._
import wom.core.{WorkflowJson, WorkflowOptionsJson, WorkflowSource}
import wom.executable.WomBundle
import wom.expression.IoFunctionSet
import wom.transforms.WomExecutableMaker.ops._

import cats.syntax.apply._

class WdlDraft3LanguageFactory(override val config: Config) extends LanguageFactory {

  var workflowDependenciesList = Seq.empty[String]

  override val languageName: String = "WDL"
  override val languageVersionName: String = "1.0"


  override def validateNamespace(source: WorkflowSourceFilesCollection,
                                 workflowSource: WorkflowSource,
                                 workflowOptions: WorkflowOptions,
                                 importLocalFilesystem: Boolean,
                                 workflowIdForLogging: WorkflowId,
                                 ioFunctions: IoFunctionSet,
                                 importResolvers: List[ImportResolver]): IOChecked[ValidatedWomNamespace] = {

    val factories: List[LanguageFactory] = List(this)

    val checked: Checked[ValidatedWomNamespace] = for {
      _ <- enabledCheck
      bundleWithDependencies <- getWomBundle(workflowSource, source.workflowOptions.asPrettyJson, importResolvers, factories)
      executable <- createExecutable(bundleWithDependencies._1, source.inputsJson, ioFunctions)
    } yield executable

    fromEither[IO](checked)

  }

  override def getWomBundle(workflowSource: WorkflowSource,
                            workflowOptionsJson: WorkflowOptionsJson,
                            importResolvers: List[ImportResolver],
                            languageFactories: List[LanguageFactory],
                            listDependencies: Boolean = false): Checked[(WomBundle, Option[Seq[String]])] = {
//    val checkEnabled: CheckedAtoB[FileStringParserInput, FileStringParserInput] = CheckedAtoB.fromCheck(x => enabledCheck map(_ => x))
//    val converter: CheckedAtoB[FileStringParserInput, WomBundle] = checkEnabled andThen
//      stringToAst andThen
//      wrapAst andThen
//      astToFileElement.map( x => FileElementToWomBundleInputs(
//        x,
//        workflowOptionsJson,
//        importResolvers,
//        languageFactories,
//        workflowDefinitionElementToWomWorkflowDefinition,
//        taskDefinitionElementToWomTaskDefinition)) andThen
//      fileElementToWomBundle


//    converter.run(FileStringParserInput(workflowSource, "input.wdl"))


    println("############# INSIDE getWomBundle ##############")

    val checkEnabled: CheckedAtoB[FileStringParserInput, FileStringParserInput] = CheckedAtoB.fromCheck(x => enabledCheck map(_ => x))
    val commonConverter: CheckedAtoB[FileStringParserInput, FileElement] = checkEnabled andThen stringToAst andThen wrapAst andThen astToFileElement

    val womBundleConverter: CheckedAtoB[FileStringParserInput, WomBundle] = commonConverter.map(FileElementToWomBundleInputs(
        _,
        workflowOptionsJson,
        importResolvers,
        languageFactories,
        workflowDefinitionElementToWomWorkflowDefinition,
        taskDefinitionElementToWomTaskDefinition)) andThen
      fileElementToWomBundle

    val workflowDependenciesConverter: CheckedAtoB[FileStringParserInput, Seq[String]] = commonConverter.map(_.imports.map(_.importUrl))

    val abc = workflowDependenciesConverter.run(FileStringParserInput(workflowSource, "input.wdl"))

    println(s"Common Converter: $abc")

    val a: Checked[WomBundle] = womBundleConverter.run(FileStringParserInput(workflowSource, "input.wdl"))

    val b = workflowDependenciesConverter.run(FileStringParserInput(workflowSource, "input.wdl"))

    (a, b) mapN { (c, d) => {
      println(s"Inside mapN: $d")
      println(s"Workflow source: $workflowSource")
      workflowDependenciesList = workflowDependenciesList ++ d
      (c, Option(workflowDependenciesList))
    } }
  }

  override def createExecutable(womBundle: WomBundle, inputsJson: WorkflowJson, ioFunctions: IoFunctionSet): Checked[ValidatedWomNamespace] = {
    for {
      _ <- enabledCheck
      executable <- womBundle.toWomExecutable(Option(inputsJson), ioFunctions, strictValidation)
      validated <- LanguageFactoryUtil.validateWomNamespace(executable, ioFunctions)
    } yield validated
  }

  override def looksParsable(content: String): Boolean = LanguageFactoryUtil.simpleLooksParseable(List("version 1.0"), List("#"))(content)
}
