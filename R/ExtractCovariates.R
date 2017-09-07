
# studyFolder <- "/Users/yang/Desktop/RareDisease"

# connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                              server = "localhost/ohdsi",
#                                              port = "",
#                                              user = "joe",
#                                              password = "supersecret")
# cdmDatabaseSchema <- "my_cdm_data"
# resultsDatabaseSchema <- cdmDatabaseSchema
# exposureTable <- "cohort"
# outcomeTable <- "cohort"
# cdmVersion <- "5"

createAnalysesDetails <- function(connectionDetails, cdmDatabaseSchema, studyFolder){
    # Define which types of covariates must be constructed ----
    covariateSettings <- FeatureExtraction::covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                                                                         useCovariateDemographicsGender = TRUE,
                                                                                         useCovariateDemographicsRace = TRUE,
                                                                                         useCovariateDemographicsEthnicity = TRUE,
                                                                                         useCovariateDemographicsAge = TRUE, useCovariateDemographicsYear = TRUE,
                                                                                         useCovariateDemographicsMonth = TRUE,
                                                                                         useCovariateConditionOccurrence = TRUE,
                                                                                         useCovariateConditionOccurrenceLongTerm = TRUE,
                                                                                         useCovariateConditionOccurrenceShortTerm = TRUE,
                                                                                         useCovariateConditionOccurrenceInptMediumTerm = TRUE,
                                                                                         useCovariateConditionEra = TRUE, useCovariateConditionEraEver = TRUE,
                                                                                         useCovariateConditionEraOverlap = TRUE,
                                                                                         useCovariateConditionGroup = TRUE,
                                                                                         useCovariateConditionGroupMeddra = TRUE,
                                                                                         useCovariateConditionGroupSnomed = TRUE,
                                                                                         useCovariateDrugExposure = TRUE,
                                                                                         useCovariateDrugExposureLongTerm = TRUE,
                                                                                         useCovariateDrugExposureShortTerm = TRUE, useCovariateDrugEra = TRUE,
                                                                                         useCovariateDrugEraLongTerm = TRUE, useCovariateDrugEraShortTerm = TRUE,
                                                                                         useCovariateDrugEraOverlap = TRUE, useCovariateDrugEraEver = TRUE,
                                                                                         useCovariateDrugGroup = TRUE, useCovariateProcedureOccurrence = TRUE,
                                                                                         useCovariateProcedureOccurrenceLongTerm = TRUE,
                                                                                         useCovariateProcedureOccurrenceShortTerm = TRUE,
                                                                                         useCovariateProcedureGroup = TRUE, useCovariateObservation = TRUE,
                                                                                         useCovariateObservationLongTerm = TRUE,
                                                                                         useCovariateObservationShortTerm = TRUE,
                                                                                         useCovariateObservationCountLongTerm = TRUE,
                                                                                         useCovariateMeasurement = TRUE, useCovariateMeasurementLongTerm = TRUE,
                                                                                         useCovariateMeasurementShortTerm = TRUE,
                                                                                         useCovariateMeasurementCountLongTerm = TRUE,
                                                                                         useCovariateMeasurementBelow = TRUE,
                                                                                         useCovariateMeasurementAbove = TRUE, useCovariateConceptCounts = TRUE,
                                                                                         useCovariateRiskScores = TRUE, useCovariateRiskScoresCharlson = TRUE,
                                                                                         useCovariateRiskScoresDCSI = TRUE, useCovariateRiskScoresCHADS2 = TRUE,
                                                                                         useCovariateRiskScoresCHADS2VASc = TRUE,
                                                                                         excludedCovariateConceptIds = c(),
                                                                                         deleteCovariatesSmallCount = 100)

    plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                                  cohortDatabaseSchema = resultsDatabaseSchema,
                                                  cohortTable = "cohort",
                                                  cohortId = 4450,
                                                  washoutPeriod = 183,
                                                  covariateSettings = covariateSettings,
                                                  outcomeDatabaseSchema = resultsDatabaseSchema,
                                                  outcomeTable = "cohort",
                                                  outcomeIds = 4450,
                                                  cdmVersion = cdmVersion)

    setwd(file.path(paste0(studyFolder,"/",disease,"/hsan_CCAEwhole_plp_data")))
    PatientLevelPrediction::savePlpData(plpData, "hsan_CCAEwhole_plp_data")


}


