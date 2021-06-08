usethis::use_package("dplyr")
usethis::use_package("xml2")

#####################################################

# BASELINE CHARACTERISTICS -  SUBJECT ANALYSIS SET  #

###################################################


#' Add the nodes of the subject analysis set which is in the category Baseline characteristics
#'
#' @param xml takes as parameter a XML file
#' @param id takes as parameter an ID of the subject analysis set
#' @param title takes as parameter the name of the subject analysis set
#' @param description takes as parameter the description of the subject analysis set
#' @param sbjAnalysisType takes as parameter the type of the subject analysis set
#'
#' @return xml file
#' @export
#'
#' @examples
addsubjectAnalysisset <- function(xml, sbj_id, title, description = '', sbjAnalysisType){
  positionTrialChanges <- xml2::xml_find_first(xml, "subjectAnalysisSets")


  subjectAnalysisSet  <- xml_new_document() %>%
    xml_add_child(.value = "subjectAnalysisSet", id = sbj_id) %>%
    xml_add_child(.value = "title", title) %>%
    xml_add_sibling(.value = "description", description) %>%
    xml_add_sibling(.value = "type") %>%
    xml_add_child(.value = "value", eudract::getsbjAnalysisset(sbjAnalysisType)) %>%
    xml_root()
  xml2::xml_add_child(positionTrialChanges , subjectAnalysisSet)


}

#' Count the number of subjects analysed per group
#'
#' @param data takes as parameter a data frame with the ID of the patient, the center code, the age, the sex,
#' the randomisation group and the subject analysis group
#'
#' @return a data frame
#' @export
#'
#' @examples
countPerSubjAnaSet <- function(data){
  data <- data %>%
    dplyr::group_by(grp_sj_analyses) %>%
    dplyr::summarise(sbj_ana_set = n(),.groups = 'drop') %>%
    as.data.frame()
  return(data)
}


#' Convert the data frame into a dictionnary with the id of the subject analysis set
#'
#' @param dictionnary takes as parameter a dictionnary of the subject analysis set
#'
#' @return data frame which is the new dictionnary
#' @export
#'
#' @examples
dictionnary_sbAnaSet <- function(dictionnary){
  dictionnary <- dictionnary %>%
    rowwise() %>%
    dplyr::mutate(id = paste("SubjectAnalysisSet",getId(),sep = "-")) %>%
    as.data.frame()
  return(dictionnary)
}
#' Create the subject analysis set based on the dictionnary of the subject analysis set
#'
#' @param xml takes as parameter a xml file
#' @param dictionnary takes as parameter a dictionnary of the subject analysis set
#'
#' @return xml file
#' @export
#'
#' @examples
addSbjAnaSetBasedOnDictionnary <- function(xml, dictionnary){
  for(i in 1:nrow(dictionnary)){
    currentData <- dictionnary[i, ]
    print(glue::glue('Insert subject analysis set {currentData[, "id"]}'))
    addsubjectAnalysisset(xml = xml,
                          sbj_id = currentData[, 'id'],
                          title = currentData[, 'title'],
                          description = currentData[, 'description'],
                          sbjAnalysisType = currentData[, 'type'])
  }
}

#' Add the node of the subject per analysis set in the xml file which is in the category Baseline characteristics
#'
#' @param xml takes as parameter a xml file
#' @param data takes as parameter a data frame with the ID of the patient,the center code, the age,
#' the sex, the randomisation group and the subject analysis group
#'
#' @return xml file
#' @export
#'
#' @examples
add_node_SbjAnaSet<- function(xml,data){
  positionsbjAnaSet <- xml_find_all(xml,"subjectAnalysisSets/subjectAnalysisSet")

  for (i in 1:length(positionsbjAnaSet)){
    currentSbjAnaSet <- positionsbjAnaSet[[i]]

    id_xml <- xml_attr(currentSbjAnaSet, "id")

    nbSubject <- data %>%
      dplyr::filter(id == id_xml) %>%
      dplyr::pull(sbjAnaSet)

    positionsbjAnalysisSet <- xml2::xml_find_first(currentSbjAnaSet, ".//title")
    subject <-  xml_new_document() %>%
      xml_add_child("subjects",  nbSubject) %>%
      xml_root()
    xml2::xml_add_sibling(positionsbjAnalysisSet, subject, .where = "before")
  }
}
