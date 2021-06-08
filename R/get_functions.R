
#' Get the allocation (type of the randomisation) of the trial
#'
#' @param alloc_type takes as parameter a type of randomization among the following list: 'Randomised-controlled',
#' 'Non-randomised-controlled', 'Not applicable'
#'
#' @return
#' @export
#'
#' @examples
getallocation_type <- function(alloc_type = NULL){
  if (is.null(alloc_type)){
    stop('Please enter Randomised-controlled OR Non-randomised-controlled OR Not applicable')
  }
  if(!alloc_type %in% c('Randomised-controlled','Non-randomised-controlled', 'Not applicable')){
    stop('type must be Randomised-controlled OR Non-randomised-controlled OR Not applicable')
  }
  if(alloc_type == 'Randomised-controlled'){
    return('ALLOCATION.randControlled')
  }else if (alloc_type == 'Non-randomised-controlled'){
    return ('ALLOCATION.nonRandControlled')
  }else if ('Not applicable'){
    return ('ALLOCATION.na')
  }else{
    return ()
  }
}


#' Get the blinding type of the trial
#'
#' @param blinding_type takes as parameter a type of bliniding among the following list: 'Double blind', 'Single blind', 'Not blinded'
#'
#' @return
#' @export
#'
#' @examples
getblinding_type <- function(blinding_type = NULL){
  if (is.null(blinding_type)){
    stop('Please enter Double blind OR Single blind OR Not blinded')
  }

  if(!blinding_type %in% c('Double blind', 'Single blind', 'Not blinded')){
    stop('type must be Double blind OR Single blind OR Not blinded')
  }
  if(blinding_type == 'Double blind'){
    return('BLINDING.double')
  } else if (blinding_type == 'Single blind') {
    return('BLINDING.single')
  } else if (blinding_type == 'Not blinded'){
    return('BLINDING.not')
  }else{
    return()
  }
}


#' Get the type of the arms
#'
#' @param arm_type takes as parameter a type of randomization among the following list: 'Experimental', 'Active comparator',
#' 'Placebo', 'No IMP', 'Other'
#'
#'
#' @return
#' @export
#'
#' @examples
getarm_type <- function(arm_type = NULL){
  if (is.null(arm_type)){
    stop('Please enter Experimental OR Active comparator OR Placebo OR No IMP OR Other')
  }
  if(!arm_type %in% c('Experimental', 'Active comparator', 'Placebo', 'No IMP', 'Other')){
    stop('type must be Experimental OR Active comparator OR Placebo OR No IMP OR Other')
  }
  if(arm_type == 'Experimental'){
    return('ARM_TYPE.experimental')
  }else if (arm_type == 'Active comparator'){
    return('ARM_TYPE.activeComp')
  }else if (arm_type == 'Placebo'){
    return('ARM_TYPE.placeboComp')
  }else if (arm_type == 'No IMP'){
    return('ARM_TYPE.noImp')
  }else if (arm_type == 'Other'){
    return('ARM_TYPE.other')
  }else{
    return()
  }
}

#' Get the structure of the arm
#'
#' @param id takes as parameter an ID of the arm
#' @param title takes as parameter a name of the arm
#' @param description takes as parameter a description of the arm
#' @param arm_type takes as parameter a type of the arm
#'
#' @return
#' @export
#'
#' @examples
getArm <- function(id, title, description = '', arm_type){
  # create arm and return to root node
  arm <- xml_new_document()  %>%
    xml_add_child(.value = "arm", id = id) %>%
    xml_add_child(.value = "title", title) %>%
    xml_add_sibling(.value = "description", description) %>%
    xml_add_sibling(.value = "type") %>%
    xml_add_child(.value = "value", getarm_type(arm_type)) %>%
    xml_root()

  # based on root node of arm, create all siblings
  arm <- arm %>%
    xml_add_child(.value = "armProducts") %>%
    xml_add_sibling(.value = "startedMilestoneAchievement", startedMilestoneId = paste("StartedMilestone", "10001", sep = "-")) %>%
    xml_add_sibling(.value = "completedMilestoneAchievement", completedMilestoneId = paste("CompletedMilestone", "10000", sep = "-")) %>%
    xml_add_sibling(.value = "otherMilestoneAchievements") %>%
    xml_add_sibling(.value = "notCompletedReasonDetails") %>%
    xml_add_sibling(.value = "joinedReasonDetails") %>%
    xml_root()

  return(arm)
}


#' Get the type of the subject analysis set
#'
#' @param sbj_type takes as parameter a type of randomization among the following list: 'Intention-to-treat', 'Per protocol', 'Full analysis', 'Safety analysis',
#' 'Sub-group analysis', 'Modified intention-to-treat'
#'
#' @return
#' @export
#'
#' @examples
getsbjAnalysisset <- function(sbj_type = NULL){
  if (is.null(sbj_type)){
    stop('Please enter Intention-to-treat OR Per protocol OR Full analysis OR Safety analysis OR Sub-group analysis OR Modified intention-to-treat')
  }
  if(!sbj_type %in% c('Intention-to-treat', 'Per protocol', 'Full analysis', 'Safety analysis', 'Sub-group analysis', 'Modified intention-to-treat')){
    stop('type must be Intention-to-treat OR Per protocol OR Full analysis OR Safety analysis OR Sub-group analysis OR Modified intention-to-treat')
  }
  if(sbj_type == 'Intention-to-treat'){
    return('SBJ_ANALYSIS_SET_TYPE.intentToTreat')
  }else if (sbj_type == 'Per protocol'){
    return('SBJ_ANALYSIS_SET_TYPE.perProtocol')
  }else if (sbj_type == 'Full Analysis'){
    return('SBJ_ANALYSIS_SET_TYPE.fullAnalysisSet')
  }else if (sbj_type == 'Safety analysis'){
    return('SBJ_ANALYSIS_SET_TYPE.safetyPopulation')
  }else if (sbj_type == 'Sub-group analysis'){
    return('SBJ_ANALYSIS_SET_TYPE.subgroup')
  }else if (sbj_type == 'Modified intention-to-treat'){
    return('SBJ_ANALYSIS_SET_TYPE.modIntentToTreat')
  }else{
    return()
  }
}


#' Get the type of the baseline reporting model in the category Baseline characteristics
#'
#' @param baselineReportingModel_type takes as parameter a type of the baseline reporting model: "Arm" or "Period"
#'
#' @return
#' @export
#'
#' @examples
getReportingModelType <- function(baselineReportingModel_type = NULL){

  if(!baselineReportingModel_type %in% c('Arm','Period')){
    stop('type must be Arm OR Period')
  }
  if(baselineReportingModel_type == 'Arm'){
    return('BASELINE_REPORTING_MODEL.arms')
  }else if (baselineReportingModel_type == 'Period'){
    return('BASELINE_REPORTING_MODEL.period')
  }else{
    return ()
  }
}

