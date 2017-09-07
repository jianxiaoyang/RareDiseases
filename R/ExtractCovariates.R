# Diseases <- c("HSAN","Craniopharyngioma","Brain neoplasm","Kabuki syndrome","GNB-5 mutation")
# cohortsId <- c("4445","4450","4452","4453","4454")

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

createAnalysesDetails <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  studyFolder,
                                  cdmVersion,
                                  disease,
                                  cohortId){
    # Define which types of covariates must be constructed ----
    covariateSettings <- FeatureExtraction::covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                                                                         useCovariateDemographicsGender = TRUE,
                                                                                         useCovariateDemographicsRace = TRUE,
                                                                                         useCovariateDemographicsEthnicity = TRUE,
                                                                                         useCovariateDemographicsAge = TRUE,
                                                                                         useCovariateDemographicsYear = TRUE,
                                                                                         useCovariateDemographicsMonth = TRUE,
                                                                                         useCovariateConditionOccurrence = TRUE,
                                                                                         useCovariateConditionOccurrenceLongTerm = TRUE,
                                                                                         useCovariateConditionOccurrenceShortTerm = TRUE,
                                                                                         useCovariateConditionOccurrenceInptMediumTerm = TRUE,
                                                                                         useCovariateConditionEra = TRUE,
                                                                                         useCovariateConditionEraEver = TRUE,
                                                                                         useCovariateConditionEraOverlap = TRUE,
                                                                                         useCovariateConditionGroup = TRUE,
                                                                                         useCovariateConditionGroupMeddra = TRUE,
                                                                                         useCovariateConditionGroupSnomed = TRUE,
                                                                                         useCovariateDrugExposure = TRUE,
                                                                                         useCovariateDrugExposureLongTerm = TRUE,
                                                                                         useCovariateDrugExposureShortTerm = TRUE,
                                                                                         useCovariateDrugEra = TRUE,
                                                                                         useCovariateDrugEraLongTerm = TRUE,
                                                                                         useCovariateDrugEraShortTerm = TRUE,
                                                                                         useCovariateDrugEraOverlap = TRUE,
                                                                                         useCovariateDrugEraEver = TRUE,
                                                                                         useCovariateDrugGroup = TRUE,
                                                                                         useCovariateProcedureOccurrence = TRUE,
                                                                                         useCovariateProcedureOccurrenceLongTerm = TRUE,
                                                                                         useCovariateProcedureOccurrenceShortTerm = TRUE,
                                                                                         useCovariateProcedureGroup = TRUE,
                                                                                         useCovariateObservation = TRUE,
                                                                                         useCovariateObservationLongTerm = TRUE,
                                                                                         useCovariateObservationShortTerm = TRUE,
                                                                                         useCovariateObservationCountLongTerm = TRUE,
                                                                                         useCovariateMeasurement = TRUE,
                                                                                         useCovariateMeasurementLongTerm = TRUE,
                                                                                         useCovariateMeasurementShortTerm = TRUE,
                                                                                         useCovariateMeasurementCountLongTerm = TRUE,
                                                                                         useCovariateMeasurementBelow = TRUE,
                                                                                         useCovariateMeasurementAbove = TRUE,
                                                                                         useCovariateConceptCounts = TRUE,
                                                                                         useCovariateRiskScores = TRUE,
                                                                                         useCovariateRiskScoresCharlson = TRUE,
                                                                                         useCovariateRiskScoresDCSI = TRUE,
                                                                                         useCovariateRiskScoresCHADS2 = TRUE,
                                                                                         useCovariateRiskScoresCHADS2VASc = TRUE,
                                                                                         excludedCovariateConceptIds = c(),
                                                                                         deleteCovariatesSmallCount = 100)

    plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                                  cohortDatabaseSchema = cdmDatabaseSchema,
                                                  cohortTable = "cohort",
                                                  cohortId = cohortId,
                                                  washoutPeriod = 183,
                                                  covariateSettings = covariateSettings,
                                                  outcomeDatabaseSchema = cdmDatabaseSchema,
                                                  outcomeTable = "cohort",
                                                  outcomeIds = cohortId,
                                                  cdmVersion = cdmVersion)

    setwd(file.path(paste0(studyFolder,"/",disease,"/hsan_CCAEwhole_plp_data")))
    PatientLevelPrediction::savePlpData(plpData, "hsan_CCAEwhole_plp_data")


}


