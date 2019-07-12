package womtool

import java.nio.file.Path

import better.files.File
import cromwell.core.path.DefaultPathBuilder
import org.scalatest.{FlatSpec, Matchers}
import womtool.WomtoolMain.{SuccessfulTermination, UnsuccessfulTermination}

import scala.collection.immutable

class WomtoolValidateSpec extends FlatSpec with Matchers {

  private def mustExist(file: java.io.File): java.io.File = if (file.exists) file else fail(s"No such file: ${file.getAbsolutePath}")
  private def ifExists(file: java.io.File): Option[java.io.File] = if (file.exists) Option(file) else None

  private def httpImportTestCaseImport(language: String) =
    s"https://raw.githubusercontent.com/broadinstitute/cromwell/5e0197d1c016d4c802ef3c2890f0ca4e0ca542c1/womtool/src/test/resources/validate/$language/valid/task_only/task_only.wdl"

  // The filterNot(_.contains(".DS")) stuff prevents Mac 'Desktop Services' hidden directories from accidentally being picked up:
  private def listFilesAndFilterDSFile(path: Path): immutable.Seq[String] =  Option(path.toFile.list).toList.flatten.filterNot(_.contains(".DS"))

  private val presentWorkingDirectoryName = DefaultPathBuilder.get(".").toAbsolutePath.name
  val validationTestCases = File("womtool/src/test/resources/validate")

  // The filterNot(_.contains(".DS")) stuff prevents Mac 'Desktop Services' hidden directories from accidentally being picked up:
  val languageVersions = Option(validationTestCases.list.filterNot(f => f.name.contains(".DS"))).toList.flatten

  val dependenciesMsgPrefix = "Success!\nList of Workflow dependencies are:\n"
  val testNameSuffixForDependenciesFlag = " with --list-dependencies flag"

  def draft2And3TestsWithDependenciesFlag(language: String) = Set(
    ("http_import", httpImportTestCaseImport(language)),
    ("relative_local_import", "../task_only/task_only.wdl"),
    ("subworkflow_input", "subworkflow.wdl"),
    ("task_only", "None")
  ).map(x => (x._1, dependenciesMsgPrefix + x._2))

  val biscayneTestsWithDependenciesFlag = Set (
    ("afters_and_scatters", "None"),
    ("http_relative_imports", "https://raw.githubusercontent.com/broadinstitute/cromwell/develop/womtool/src/test/resources/validate/biscayne/valid/relative_imports/sub_wfs/foo.wdl\n../structs/my_struct.wdl\ntasks/add5.wdl\n../../structs/my_struct.wdl"),
    ("relative_imports", "sub_wfs/foo.wdl\n../structs/my_struct.wdl\ntasks/add5.wdl\n../../structs/my_struct.wdl")
  ).map(x => (x._1, dependenciesMsgPrefix + x._2))

  val testExpectationsWithDependenciesFlag = Set (
    ("biscayne", biscayneTestsWithDependenciesFlag),
    ("wdl_draft2", draft2And3TestsWithDependenciesFlag("wdl_draft2")),
    ("wdl_draft3", draft2And3TestsWithDependenciesFlag("wdl_draft3"))
  )

  behavior of "womtool validate"

  it should "test at least one version" in {
    languageVersions.isEmpty should be(false)
  }

  languageVersions foreach { versionDirectory =>
    val versionName = versionDirectory.name

    val validTestCases = versionDirectory.path.resolve("valid")
    val invalidTestCases = versionDirectory.path.resolve("invalid")
    val ignoredTestCases = versionDirectory.path.resolve("ignored")


    def runValidTestCase(versionName: String,
                         validCase: String,
                         withInputsAddition: String,
                         inputsFile: Option[java.io.File],
                         expectedMsg: String,
                         commandPrefix: Seq[String],
                         testNameSuffix: String = "") = {
      it should s"successfully validate $versionName workflow: '$validCase'$withInputsAddition$testNameSuffix" in {
        val wdl = mustExist(validTestCases.resolve(validCase).resolve(validCase + ".wdl").toFile)
        val inputsArgs = inputsFile match {
          case Some(path) => Seq("-i", path.getAbsolutePath)
          case None => Seq.empty[String]
        }

        WomtoolMain.runWomtool(commandPrefix ++ Seq(wdl.getAbsolutePath) ++ inputsArgs) should be(SuccessfulTermination(expectedMsg))
      }
    }


    def runInvalidTestCase(versionName: String,
                           invalidCase: String,
                           withInputsAddition: String,
                           inputsFile: Option[java.io.File],
                           commandPrefix: Seq[String],
                           testNameSuffix: String = "") = {
      it should s"fail to validate $versionName workflow: '$invalidCase'$withInputsAddition$testNameSuffix" in {
        val wdl = mustExist(invalidTestCases.resolve(invalidCase).resolve(invalidCase + ".wdl").toFile)
        val errorFile = ifExists(invalidTestCases.resolve(invalidCase).resolve("error.txt").toFile).map(f => File(f.getAbsolutePath).contentAsString)
        val inputsArgs = inputsFile match {
          case Some(path) => Seq("-i", path.getAbsolutePath)
          case None => Seq.empty[String]
        }

        WomtoolMain.runWomtool(Seq("validate", wdl.getAbsolutePath) ++ inputsArgs) match {
          case UnsuccessfulTermination(msg) => errorFile match {
            case Some(expectedError) =>
              msg should include(expectedError.trim.replace(s"$${PWD_NAME}", presentWorkingDirectoryName))
            case None => succeed
          }
          case other => fail(s"Expected UnsuccessfulTermination but got $other")
        }
      }
    }

    // Don't bother checking that the 'ignored' directory exists:
    List(validTestCases, invalidTestCases) foreach { path =>
      it should s"be set up for testing $versionName in '${versionDirectory.relativize(path).toString}'" in {
        if (!path.toFile.exists) fail(s"Path doesn't exist: ${path.toAbsolutePath.toString}")
        if (Option(path.toFile.list).toList.flatten.isEmpty) fail(s"No test cases found in: ${path.toAbsolutePath.toString}")
        versionDirectory.list.nonEmpty shouldBe true
      }
    }

    Option(ignoredTestCases.toFile.list).toList.flatten foreach { ignoredCase =>
      it should s"run $versionName test '$ignoredCase'" ignore {}
    }

    listFilesAndFilterDSFile(validTestCases) foreach { validCase =>
      val inputsFile = ifExists(validTestCases.resolve(validCase).resolve(validCase + ".inputs.json").toFile)
      val withInputsAddition = if (inputsFile.isDefined) " and inputs file" else ""

      runValidTestCase(versionName, validCase, withInputsAddition, inputsFile, "Success!", Seq("validate"))

      //run validate with --list-dependencies flag if success message for the test exists in `testExpectationsWithDependenciesFlag`
      val languageTestExpectations = testExpectationsWithDependenciesFlag.collect { case (l, s) if l.equalsIgnoreCase(versionName) => s}.flatten
      val expectedMsgOption = languageTestExpectations.find(_._1 == validCase).map(_._2)
      expectedMsgOption.foreach(runValidTestCase(versionName, validCase, withInputsAddition, inputsFile, _, Seq("validate", "-l"), testNameSuffixForDependenciesFlag))
    }

    listFilesAndFilterDSFile(invalidTestCases) foreach { invalidCase =>
      val inputsFile = ifExists(invalidTestCases.resolve(invalidCase).resolve(invalidCase + ".inputs.json").toFile)
      val withInputsAddition = if (inputsFile.isDefined) " and inputs file" else ""

      runInvalidTestCase(versionName, invalidCase, withInputsAddition, inputsFile, Seq("validate"))

      //run same test case with --list-dependencies flag
      runInvalidTestCase(versionName, invalidCase, withInputsAddition, inputsFile, Seq("validate", "-l"), testNameSuffixForDependenciesFlag)
    }
  }
}
