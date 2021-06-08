usethis::use_package("dplyr")
usethis::use_package("xml2")

#####################################################

# TRIAL INFORMATION - POPULATION OF TRIAL SUBJECTS #

###################################################


#############################
# Subject number per country
#############################

#' Recode the number of the centre with the code in the dictionnary of the country
#'
#' @param data takes as parameter a data frame with the ID of the patient,the center code,
#' the age, the sex, the randomisation group and the subject analysis group
#' @param dictionnary takes as parameter a dictionnary of the country
#'
#' @return data frame
#' @export
#'
#' @examples
recode_country <- function(data, dictionnary){
  data <- data %>%
    dplyr::mutate(
      country_code = dplyr::recode_factor(num_cen, !!!dictionnary))
}


#' Count the number of the subject per country
#'
#' @param data takes as parameter a data frame with the ID of the patient,the center code,
#' age, sex, the randomisation group and the subject analysis group
#' @param dictionnary takes as parameter a dictionnary of the country
#'
#' @return a data frame with the number of the subject per country
#' @export
#'
#' @examples
countSubjectCountry <- function(data, dictionnary){
  var_centre <- 'num_cen'
  if(!all(names(dictionnary) %in% data[, var_centre])){
    diff <- setdiff(names(dictionnary), data[, var_centre])
    stop ("the name of the centre in the data frame is not the same in the dictionnary")
  }
  recode_Country(data, dictionnary) %>%
    dplyr::group_by(country_code) %>%
    dplyr::summarise(subjects = n(),.groups = 'drop') %>%
    as.data.frame()
}

#' Add nodes "subjects" in "countSubjectCountry" in the category Trial information
#'
#' @param xml takes as parameter a xml file
#' @param x takes as parameter a function
#'
#' @return xml file
#' @export
#'
#' @examples
add_node_SubPerCountry <- function(xml,x) {
  position_countrySub <- xml2::xml_find_first(xml,".//countrySubjectCounts")

  doc <- xml_new_document()
  for (i in seq_along(x)) {
    xml_add_child(position_countrySub, unname(x)[[i]], .copy = TRUE) %>%
      xml_root()
  }
}


###############################
# Subject number per Age Range
##############################

#' Count the number of subjects per age range
#'
#' @param data takes as parameter a data frame with the ID of the patient,the center code,
#' age, sex, the randomisation group and the subject analysis group
#' @return a data frame
#' @export
#'
#' @examples
countAgeCat_per_years <- function(data){
  age <- 'age'
  data <- data %>%
    dplyr::mutate(
      age_categorical = dplyr::case_when(
        age >= 2 & age <= 12 ~ 'children',
        age <= 17 ~ 'adolescents',
        age <= 64 ~ 'adults',
        age <= 84 ~ 'from 65-84 years',
        age >= 85 ~ '85 years and over',
        TRUE ~ 'category not found or category is in the infant category'
      )
    ) %>%
    dplyr::group_by(age_categorical) %>%
    dplyr::summarise(nb_per_age= n(),.groups = 'drop') %>%
    as.data.frame()
  return(data)
}


#' Replace the nodes of the age range with a new nodes which contain their values in the category Trial information
#'
#' @param xml takes as parameter XML file
#' @param x takes as parameter a function which count the number of the subject per age range
#'
#' @return xml file
#' @export
#'
#' @examples
replaceAgeRange <- function(xml, x){

  positionChild <- xml2::xml_find_first(xml, ".//children")
  positionAdo <- xml2::xml_find_first(xml,".//adolescents")
  positionAdult <- xml2::xml_find_first(xml,".//adults")
  positionelderly65To84 <- xml2::xml_find_first(xml,".//elderly65To84")
  positionelderlyOver85 <- xml2::xml_find_first(xml,".//elderlyOver85")

  for(i in 1:nrow(x)){
    currentRow <- x[i, ]

    if(currentRow[1] == 'children'){
      xml_replace(positionChild, .value = "children", unname(currentRow)[[2]])
    } else if(currentRow[1] == 'adolescents'){
      xml_replace(positionAdo, .value = "adolescents", unname(currentRow)[[2]])
    } else if(currentRow[1] == 'adults'){
      xml_replace(positionAdult, .value = "adults", unname(currentRow)[[2]])
    } else if (currentRow[1] == 'from 65-84 years'){
      xml_replace(positionelderly65To84 , .value = "elderly65To84", unname(currentRow)[[2]])
    }else if (currentRow[1] == '85 years and over'){
      xml_replace(positionelderlyOver85, .value = "elderlyOver85",unname(currentRow)[[2]])
    }
  }
}
