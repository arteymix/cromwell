package cromwell.backend.google.pipelines.v2alpha1.api

import com.google.api.services.genomics.v2alpha1.model.Mount
import cromwell.backend.google.pipelines.common.PipelinesApiConfigurationAttributes.LocalizationConfiguration
import cromwell.backend.google.pipelines.common.api.PipelinesApiRequestFactory.CreatePipelineParameters
import cromwell.backend.google.pipelines.v2alpha1.PipelinesParameterConversions
import cromwell.backend.google.pipelines.v2alpha1.api.ActionBuilder.Labels.Value

trait Localization {
  def localizeActions(createPipelineParameters: CreatePipelineParameters, mounts: List[Mount])(implicit localizationConfiguration: LocalizationConfiguration) = {
    val jobInputLocalization = PipelinesParameterConversions.groupedLocalizationActions(createPipelineParameters.inputOutputParameters.fileInputParameters, mounts)
    ActionBuilder.annotateTimestampedActions("localization", Value.Localization)(jobInputLocalization)
  }
}
