# library(PatientLevelPrediction)
# library(dplyr)
# library(ggplot2)

# studyFolder <- "/Users/yang/Desktop/RareDisease"

# Diseases <- c("HSAN","Craniopharyngioma","Brain neoplasm","Kabuki syndrome","GNB-5 mutation")
# cohortsId <- c("4445","4450","4452","4453","4454")

# setwd("D:/Users/msuchard/Documents/PatientLevelPrediction-master/HSAN/Truven_CCAE")
# plpData <- loadPlpData("D:/Users/msuchard/Documents/PatientLevelPrediction-master/vignettes/hsan_CCAEwhole_plp_data")

# plpData <- loadPlpData("/Users/yang/Downloads/hsan_CCAEwhole_plp_data")


childrencov <- function(disease,studyFolder){
    setwd(file.path(paste0(studyFolder,"/",disease)))

    plpData <- PatientLevelPrediction::loadPlpData(file.path(paste0(studyFolder,"/",disease,"/hsan_CCAEwhole_plp_data")))

    covariatesinfo <- as.data.frame(plpData$covariateRef[,1:3])
    nameid <- as.data.frame(covariatesinfo[,1:2])
    covariateswhole <- as.data.frame(plpData$covariates[,1:2])

    childrenage <- c(11:13)
    childrenrowid <- dplyr::filter(covariateswhole,covariateswhole$covariateId %in% childrenage)
    childrencovariate <- dplyr::merge(childrenrowid,covariateswhole,by = "rowId",all.x = TRUE, sort = TRUE)
    names(childrencovariate) <- c("rowId","Age group","covariateId")

    mergecovariate <- dplyr::merge(childrencovariate,covariatesinfo, by = "covariateId",all.x = TRUE, sort = TRUE)

    # Demographics
    summaryDemo <- function(mergecovariate){

        ## Age group
        age_group <- dplyr::filter(mergecovariate,mergecovariate$analysisId==5)

        agegroup <- as.data.frame(table(age_group$covariateId))
        agegroup <- dplyr::arrange(agegroup,desc(Freq))
        agegroup$prevalence <- agegroup$Freq/length(childrenrowid[,1])
        names(agegroup) <- c("covariateId","Person Count","Prevalence")

        agegroup <- dplyr::merge(agegroup,nameid, by = "covariateId",all.x = TRUE, sort = TRUE)
        agegroup <- dplyr::arrange(agegroup,desc(agegroup$`Person Count`))

        age_group$covariateName <- factor(age_group$covariateName, levels=c("Age group: 5-9","Age group: 10-14","Age group: 15-19"))

        plotage <- ggplot2::ggplot(age_group,aes(x=age_group$covariateName))+geom_bar(width=0.5,fill=c("royalblue1","royalblue3","royalblue4"))+labs(x="Age group",y="Person Count")+geom_text(aes(label=..count..),
                                                                                                                                                                                      stat="count", color = "black",
                                                                                                                                                                                      vjust = 0, size = 4)
        ggsave(filename="Age Group.png",plot=plotage,device="png",path = file.path(paste0(studyFolder,"/",disease)))
        # write.csv(age_group,file="age_group.csv")
        write.csv(agegroup,file="agegroup.csv")


        ## Gender

        Gender<- dplyr::filter(mergecovariate,mergecovariate$analysisId==2)

        gender <- as.data.frame(table(Gender$covariateId))
        g <- c("MALE","FEMALE")
        counts <- c(gender[1,2],length(childrenrowid[,1])-gender[1,2])
        gender <- data.frame(g,counts)
        names(gender) <- c("Gender","Patient Counts")

        # write.csv(Gender,file="Gender.csv")
        write.csv(gender,file="gender.csv")


        ## demo

        age_gender <- dplyr::merge(age_group, Gender, by = "rowId",all.x = TRUE, sort = TRUE)
        new <- c("rowId","Age group.x","covariateName.x","covariateId.y","covariateName.y")
        age_gender <- age_gender[new]
        names(age_gender) <- c("rowId","age group.x","Age Group","covariateId.y","Gender")
        age_gender$covariateId.y[is.na(age_gender$covariateId.y)] <- 8532
        age_gender$Gender[is.na(age_gender$Gender)] <- "Gender = FEMALE"

        plotgender <- ggplot2::ggplot(age_gender,aes(x=age_gender$Gender))+geom_bar(width=0.4,fill=c("lightcoral","royalblue3"))+labs(x="Gender",y="Person Count")+geom_text(aes(label=..count..),stat="count", color = "black",
                                                                                                                                                                    vjust = 0, size = 4)
        ggsave(filename="Gender.png",plot=plotgender,device="png",path = file.path(paste0(studyFolder,"/",disease)))

        write.csv(age_gender,file="age_gender.csv")
    }

    summaryDemo(mergecovariate)


    # Rank covariates
    rankcov <- function(mergecovariate){

        ## Condition Occurrence

        condition_occurrence <- dplyr::filter(mergecovariate,mergecovariate$analysisId>100 & mergecovariate$analysisId<200)

        conditionoccur <- as.data.frame(table(condition_occurrence$covariateId))
        conditionoccur <- dplyr::arrange(conditionoccur,desc(Freq))
        conditionoccur$prevalence <- conditionoccur$Freq/length(childrenrowid[,1])
        names(conditionoccur) <- c("covariateId","Person Count","Prevalence")

        conditionoccur <- dplyr::merge(conditionoccur,nameid, by = "covariateId",all.x = TRUE, sort = TRUE)
        conditionoccur <- dplyr::arrange(conditionoccur,desc(conditionoccur$`Person Count`))

        # write.csv(condition_occurrence,file="condition_occurence.csv")
        write.csv(conditionoccur,file="conditionoccur.csv")


        ## Condition Era

        condition_era <- dplyr::filter(mergecovariate,mergecovariate$analysisId>200 & mergecovariate$analysisId<300)

        conditionera <- as.data.frame(table(condition_era$covariateId))
        conditionera <- dplyr::arrange(conditionera,desc(Freq))
        conditionera$prevalence <- conditionera$Freq/length(childrenrowid[,1])
        names(conditionera) <- c("covariateId","Person Count","Prevalence")

        conditionera <- dplyr::merge(conditionera,nameid, by = "covariateId",all.x = TRUE, sort = TRUE)
        conditionera <- dplyr::arrange(conditionera,desc(conditionera$`Person Count`))

        # write.csv(condition_era,file="condition_era.csv")
        write.csv(conditionera,file="conditionera.csv")


        ## Drug Exposure

        drug_exposure <- dplyr::filter(mergecovariate,mergecovariate$analysisId>400 & mergecovariate$analysisId<500)

        drugexpo <- as.data.frame(table(drug_exposure$covariateId))
        drugexpo <- dplyr::arrange(drugexpo,desc(Freq))
        drugexpo$prevalence <- drugexpo$Freq/length(childrenrowid[,1])
        names(drugexpo) <- c("covariateId","Person Count","Prevalence")

        drugexpo <- dplyr::merge(drugexpo,nameid, by = "covariateId",all.x = TRUE, sort = TRUE)
        drugexpo <- dplyr::arrange(drugexpo,desc(drugexpo$`Person Count`))

        # write.csv(drug_exposure,file="drug_exposure.csv")
        write.csv(drugexpo,file="drugexpo.csv")


        ## Drug Era

        drug_era <- dplyr::filter(mergecovariate,mergecovariate$analysisId>500 & mergecovariate$analysisId<600)

        drugera <- as.data.frame(table(drug_era$covariateId))
        drugera <- dplyr::arrange(drugera,desc(Freq))
        drugera$prevalence <- drugera$Freq/length(childrenrowid[,1])
        names(drugera) <- c("covariateId","Person Count","Prevalence")

        drugera <- dplyr::merge(drugera,nameid, by = "covariateId",all.x = TRUE, sort = TRUE)
        drugera <- dplyr::arrange(drugera,desc(drugera$`Person Count`))

        # write.csv(drug_era,file="drug_era.csv")
        write.csv(drugera,file="drugera.csv")


        ## number of drugs within each ATC3 groupings all time prior

        drug_num <- dplyr::filter(mergecovariate,mergecovariate$analysisId>600 & mergecovariate$analysisId<700)

        drugnum <- as.data.frame(table(drug_num$covariateId))
        drugnum <- dplyr::arrange(drugnum,desc(Freq))
        drugnum$prevalence <- drugnum$Freq/length(childrenrowid[,1])
        names(drugnum) <- c("covariateId","Person Count","Prevalence")

        drugnum <- dplyr::merge(drugnum,nameid, by = "covariateId",all.x = TRUE, sort = TRUE)
        drugnum <- dplyr::arrange(drugnum,desc(drugnum$`Person Count`))

        # write.csv(drug_num,file="drug_num.csv")
        write.csv(drugnum,file="drugnum.csv")


        ## Procedure Occurrence

        procedure_occurrence <- dplyr::filter(mergecovariate,mergecovariate$analysisId>700 & mergecovariate$analysisId<800)

        procedureoccur <- as.data.frame(table(procedure_occurrence$covariateId))
        procedureoccur <- dplyr::arrange(procedureoccur,desc(Freq))
        procedureoccur$prevalence <- procedureoccur$Freq/length(childrenrowid[,1])
        names(procedureoccur) <- c("covariateId","Person Count","Prevalence")

        procedureoccur <- dplyr::merge(procedureoccur,nameid, by = "covariateId",all.x = TRUE, sort = TRUE)
        procedureoccur <- dplyr::arrange(procedureoccur,desc(procedureoccur$`Person Count`))

        # write.csv(procedure_occurrence,file="procedure_occurrence.csv")
        write.csv(procedureoccur,file="procedureoccur.csv")


        ## Observation

        Observation <- dplyr::filter(mergecovariate,mergecovariate$analysisId>900 & mergecovariate$analysisId<1000)

        Ob <- as.data.frame(table(Observation$covariateId))
        Ob <- dplyr::arrange(Ob,desc(Freq))
        Ob$prevalence <- Ob$Freq/length(childrenrowid[,1])
        names(Ob) <- c("covariateId","Person Count","Prevalence")

        Ob <- dplyr::merge(Ob,nameid, by = "covariateId",all.x = TRUE, sort = TRUE)
        Ob <- dplyr::arrange(Ob,desc(Ob$`Person Count`))

        # write.csv(Observation,file="Observation.csv")
        write.csv(Ob,file="Ob.csv")

    }
    rankcov(mergecovariate)
}



