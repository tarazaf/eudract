usethis::use_package("dplyr")
usethis::use_package("xml2")
#################################

# SUBJECT DISPOSITION - PERIODS

################################


#' Add the nodes of the Period in the XML file which is in the category Subject disposition
#'
#' @param xml takes as parameter a XML file
#' @param mutuallyExclusiveArms takes as parameter a boolean type
#' @param baselinePeriod takes as parameter a boolean type
#' @param blinded takes as parameter a boolean type
#' @param allocation_type takes as parameter the allocation type of the trial
#' @param blinding_type takes as parameter the blinding type of the trial
#'
#' @return XML file
#' @export
#'
#' @examples
addPeriod <- function (xml, mutuallyExclusiveArms, baselinePeriod, blinded, allocation_type, blinding_type){
  position <- xml2::xml_find_first(xml,".//postAssignmentPeriods")

  if(mutuallyExclusiveArms == TRUE){
    mutuallyExclusiveArms_txt = 'true'
  } else {
    mutuallyExclusiveArms_txt = 'false'
  }
  if(baselinePeriod == TRUE){
    baselinePeriod_txt = 'true'
  } else {
    baselinePeriod_txt = 'false'
  }
  if(blinded == TRUE){
    blinded_txt = 'true'
  } else {
    blinded_txt = 'false'
  }

  postAssignmentPeriod <- xml_new_document() %>%
    xml_add_child(.value = "postAssignmentPeriod", id = paste("PostAssignmentPeriod",getId(),sep="-")) %>%
    xml_add_child(.value = "completedMilestone", id = paste("CompletedMilestone","10000", sep = "-")) %>%
    xml_add_sibling(.value = "startedMilestone" , id = paste("StartedMilestone","10001", sep = "-"))  %>%
    xml_add_sibling(.value = "otherMilestones") %>%
    xml_add_sibling(.value = "title", "Overall trial") %>%
    xml_add_sibling(.value = "mutuallyExclusiveArms", mutuallyExclusiveArms_txt) %>%
    xml_add_sibling(.value = "baselinePeriod", baselinePeriod_txt) %>%
    xml_add_sibling(.value = "blinded", blinded_txt)

  if(blinded == TRUE){
    #add the clinical roles and the blinding type
    postAssignmentPeriod <- postAssignmentPeriod %>%
      xml_add_sibling(.value = "clinicalTrialRoles") %>%
      xml_add_sibling(.value = "blindingType") %>%
      xml_add_child(.value = "value",getblinding_type(blinding_type)) %>%
      xml_root()

    positionBlindingType <- xml2::xml_find_first(postAssignmentPeriod,".//blindingType")

    allocation <- xml_new_document() %>%
      xml_add_child(.value = "allocation") %>%
      xml_add_child(.value = "value", getallocation_type(allocation_type)) %>%
      xml_root()

    xml2::xml_add_sibling(positionBlindingType, allocation)

  }

  xml2::xml_add_child(position, postAssignmentPeriod)
}


#' Add arms in the XML file which is in the category Subject disposition
#'
#' @param xml takes as parameter a XML file
#' @param arm_id takes as parameter an ID of the arm
#' @param title takes as parameter a name of the arm
#' @param description takes as parameter a description of the arm
#' @param arm_type takes as parameter a type of the arm
#'
#' @return XML file
#' @export
#'
#' @examples
addArm <- function(xml, arm_id, title, description = '', arm_type){

  # Position of the arms
  positionArms <- xml2::xml_find_first(xml, ".//arms")

  if(is.na(positionArms)){

    # Add arms to the xml blank file
    arms <- xml_new_document() %>%
      xml_add_child(.value = "arms") %>%
      xml_root()

    allocationPosition <- xml2::xml_find_first(xml, ".//allocation")
    xml2::xml_add_sibling(allocationPosition, arms)
  }

  # Create arm and add it to arms
  arm <- getArm(id = arm_id,
                title = title,
                description = description,
                arm_type = arm_type)

  positionArms <- xml2::xml_find_first(xml, ".//arms")
  xml2::xml_add_child(positionArms, arm)
}

###################
# Subject per arm
##################

#' Count the number of the subject per arm
#'
#' @param data takes as parameter a data frame with the ID of the patient,the center code,
#' age, sex, the randomisation group and the subject analysis group
#'
#' @return data frame
#' @export
#'
#' @examples
countPerArm <- function(data){
  data <- data %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(subject_per_arm = n(),.groups = 'drop') %>%
    as.data.frame()
  return(data)
}

#' Convert the data frame into a dictionnary with the id of the arm
#'
#' @param dictionnary takes as parameter a dictionnary of the arm
#'
#' @return data frame which is the new dictionnary
#' @export
#'
#' @examples
formatDictionnary <- function(dictionnary){
  dictionnary <- dictionnary %>%
    rowwise() %>%
    dplyr::mutate(id = paste("Arm", getId(),sep = "-")) %>%
    as.data.frame()
  return(dictionnary)
}

#' Fusionning two datas frame
#'
#' @param dataA takes as a parameter a data frame with an key column "title"
#' @param dataB takes as a parameter a data frame with an key column "title"
#'
#' @return a data frame
#' @export
#'
#' @examples
fusiontable <- function(dataA, dataB){
  dplyr::left_join(dataA, dataB, by = "title")
}

#' Create the arms based on the dictionnary of the arms in the category Subject disposition
#'
#' @param xml takes as a parameter a XML file
#' @param dictionnary takes as parameter a dictionnary of the arm
#'
#' @return
#' @export
#'
#' @examples
addArmBasedOnDictionnary <- function(xml, dictionnary){
  for(i in 1:nrow(dictionnary)){
    currentData <- dictionnary[i, ]
    print(glue::glue('Insert arm {currentData[, "id"]}'))
    addArm(xml = xml,
           arm_id = currentData[, 'id'],
           title = currentData[, 'title'],
           description = currentData[, 'description'],
           arm_type = currentData[, 'type'])
  }
}

#' Add the node of the subject per arm in the xml file which is in the category Subject disposition
#'
#' @param xml takes as a parameter xml file
#' @param data takes as parameter a data frame with the ID of the patient,the center code,the age,
#' the sex, the randomisation group and the subject analysis group
#'
#' @return xml file
#' @export
#'
#' @examples
add_node_SubjectPerArm <- function(xml,data){
  positionArms <- xml_find_all(xml,".//arms/arm")

  for (i in 1:length(positionArms)){
    currentArm <- positionArms[[i]]

    id_xml <- xml_attr(currentArm, "id")

    nbSubject <- data %>%
      dplyr::filter(id == id_xml) %>%
      dplyr::pull(subjectPerArm)

    positionStaMilestone <- xml2::xml_find_all(currentArm,".//startedMilestoneAchievement")
    subject <-  xml_new_document() %>%
      xml_add_child("subjects",  nbSubject) %>%
      xml_root()
    xml2::xml_add_child(positionStaMilestone, subject)
  }
}
