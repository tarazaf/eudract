usethis::use_package("sas7bdat")
usethis::use_package("dplyr")
usethis::use_package("xml2")

#######################################################

# BASELINE CHARACTERISTICS -  AGE CATECORICAL & GENDER #

#####################################################


#' Add the nodes of the reporting model in the category Baseline characteristics
#'
#' @param xml takes as parameter a XML file
#' @param reportingModelType takes as parameter the type of baseline reporting model
#'
#' @return xml file
#' @export
#'
#' @examples
addNodeReportModel <- function(xml, reportingModelType){
  positionBaselineCharacteristics <- xml2::xml_find_first(xml,".//baselineCharacteristics/studyCategoricalCharacteristics")

  reportingModel <- xml2::xml_new_document()%>%
    xml_add_child(.value = "reportingModel") %>%
    xml_add_child(.value = "value", getReportingModelType(reportingModelType))%>%
    xml_root()

  xml2::xml_add_sibling(positionBaselineCharacteristics, reportingModel, .where = "before")
}


#' Replace the element of the node "Replace For Value" in in the category Baseline characteristics by true
#'
#' @param xml takes as parameter a XML file
#'
#' @return
#' @export
#'
#' @examples
replaceReadyforValue <- function(xml){
  positionReadValue <- xml2::xml_find_first(xml,".//baselineCharacteristics/ageCategoricalCharacteristic/readyForValues")
  xml2::xml_replace(positionReadValue, .value = "readyForValues", 'true')
}


#' Get the age categorical in the xml file and put in the dictionnary which contain the id of the age categorical and the name of the age catogrical
#' in the category Baseline characteristics
#'
#' @param xml takes as parameter a XML file
#'
#' @return data frame which is the dictionnary
#' @export
#'
#' @examples
getAgeCategory_baselineChar <- function(xml){
  positionCategory <- xml2::xml_find_all(xml,".//baselineCharacteristics/ageCategoricalCharacteristic/categories/category")

  idAndContent <- lapply(positionCategory, function(category){
    idCategory <- xml2::xml_attr(category, "id")
    name <- xml2::xml_child(category)
    nameContain <- as.character(xml2::xml_contents(name))

    dicoAgeCategorical <- data.frame(idCategory = idCategory, age_categorical = nameContain)
    return(dicoAgeCategorical)
  })

  idAndContent <- dplyr::bind_rows(idAndContent)
  return (idAndContent)

}


#' Get the nodes of the countable value of the age categorical in the XML file, in the category Baseline characteristics
#'
#' @param xml takes as parameter a XML file
#'
#' @return
#' @export
#'
#' @examples
getContainCountableValue <- function(xml){

  positionTotalBRG <- xml2::xml_find_first(xml,".//baselineCharacteristics/ageCategoricalCharacteristic/totalBaselineGroup")

  positioncountableValue <- xml2::xml_find_first(positionTotalBRG, "countableValues")
  return (xml_contents(positioncountableValue))
}


### Add age categorical specific EudraCT database in the data set

#' Add the age categorical in the data set
#'
#' @param data data set
#'
#' @return
#' @export
#'
#' @examples
add_AgeCategorical <- function(data){
  age <- 'age'
  data <- data %>%
    dplyr::mutate(
      age_categorical = dplyr::case_when(
        age <= -0.05 ~ 'In utero', #supposition
        age >= 0.05 & age <= 0.06 ~ 'Preterm newborn infants (gestational age &lt; 37 wks)', #supposition
        age <= 0.07392197 ~ 'Newborns (0-27 days)',      ## 27 / 365.25
        age >= 0.07665982 & age <= 1.916667 ~ ' Infants and toddlers (28 days-23 months)',
        age >= 2 & age <= 12 ~ 'Children (2-11 years)',
        age <= 17 ~ 'Adolescents (12-17 years)',
        age <= 64 ~ 'Adults (18-64 years)',
        age <= 84 ~ 'From 65-84 years',
        age >= 85 ~ '85 years and over',
        TRUE ~ 'category not found'
      )
    )
  return(data)
}

#' Count the number of subject per arm and per age categorical in the cateogry Baseline characteristics
#'
#' @param data takes as parameter the data set
#'
#' @return
#' @export
#'
#' @examples
countAgeCategorical_per_arm <- function(data){
  data <- data %>%
    dplyr::group_by(arm, age_categorical) %>%
    dplyr::summarise(nb_AgeCat_per_arm= n(),.groups = 'drop') %>%
    as.data.frame()
  return(data)
}


#' Add the ID of the age categorical which is in the dictionnary of the category in the data set
#'
#' @param data the data set
#' @param dictionnary the dictionnary of the age categorical
#'
#' @return
#' @export
#'
#' @examples
addIdCategory_AgeCategorical <- function(data, dictionnary){
  return(dplyr::inner_join(data,dictionnary))
}

#' Add the nodes of the reporting group in the XML file in the category Baseline characteristics
#'
#' @param xml takes as parameter a XML file
#' @param reportingGroupId the id of the reporting group
#'
#' @return
#' @export
#'
#' @examples
add_reportingGroup <- function(xml, reportingGroupId){
  positionReportingGroup <- xml2::xml_find_first(xml, ".//baselineCharacteristics/ageCategoricalCharacteristic/reportingGroups")

  reportingGroup  <- xml_new_document() %>%
    xml_add_child(.value = "reportingGroup", baselineReportingGroupId = reportingGroupId) %>%
    xml_add_child(.value = "countableValues") %>%
    xml2::xml_root()

  positionCountableValueS <- xml2::xml_find_first(reportingGroup, ".//countableValues")

  listContable <- getContainCountableValue(xml)
  for(i in 1:length(listContable)){
    xml2::xml_add_child(positionCountableValueS, listContable[[i]])
  }

  xml2::xml_add_child(positionReportingGroup,reportingGroup)
}


### Add the nodes of the number of subject per age categorical and per arm
add_Value_AgeCategorical <- function(xml, data){
  positionCountableValue <- xml2::xml_find_all(xml,".//baselineCharacteristics/ageCategoricalCharacteristic/reportingGroups/reportingGroup/countableValues/countableValue")

  for (i in 1:length(positionCountableValue)){
    currentCountableValue <- positionCountableValue[[i]]

    categoryId_xml <- xml_attr(currentCountableValue, "categoryId")

    nb_sbjAgeCategorical <- data %>%
      dplyr::filter(idCategory == categoryId_xml) %>%
      dplyr::pull(nb_AgeCat_per_arm)

    positionValue <- xml2::xml_find_first(currentCountableValue, ".//value")
    xml2::xml_replace(positionValue, .value = "value", nb_sbjAgeCategorical)

    # if (is.na(nb_subAgeCategorical)){
    #   nb_subAgeCategorical[i] = 0
    # }
  }
}

