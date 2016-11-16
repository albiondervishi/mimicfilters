#' List of admission diagnoses to exclude for a severe sepsis cohort.
#'
#' A list of admission diagnoses that do not indicate a manifestation of
#' severe sepsis and should be excluded from the filtered cohort.
#'
#' @format
#' \describe{
#'   \item{sepsis_admdx_exclusion}{list of exclusion criter}
#' }
#' @source Monique Tanna
"sepsis_admdx_exclusion"

#' List of admission diagnoses to include for a severe sepsis cohort.
#'
#' A list of admission diagnoses that do indicate a manifestation of
#' severe sepsis and should be included in the filtered cohort.
#'
#' @format
#' \describe{
#'   \item{sepsis_admdx_inclusion}{list of inclusion criter}
#' }
#' @source Monique Tanna
"sepsis_admdx_inclusion"


#' Data.frame of ICD-9 diagnosis codes for organ failure and their meanings.
#'
#' A list of ICD-9 diagnosis codes that indicate some kind of major organ
#' failure - one essential part of severe sepsis.
#'
#' @format
#' \describe{
#'   \item{sepsis_icd9_organfailure}{data.frame of organ failure ICD-9 codes and their meanings}
#' }
#' @source Monique Tanna
"sepsis_icd9_organfailure"

#' Data.frame of ICD-9 diagnosis codes for systemic infection and their meanings.
#'
#' A list of ICD-9 diagnosis codes that indicate some kind of systemic
#' infection - one essential part of severe sepsis.
#'
#' @format
#' \describe{
#'   \item{sepsis_icd9_infection}{data.frame of systemic infection ICD-9 codes and their meanings}
#' }
#' @source Monique Tanna
"sepsis_icd9_infection"


