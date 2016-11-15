context("testing functions for the admission diagnosis filtering function")

test_that("inc_or_not_exc", {

  ## Test the null for exclusion criteria
  expect_true(inc_or_not_exc('abc', 'abc', NULL))

  ## Test the in and out case
  expect_false(inc_or_not_exc('abc', 'abc', 'abc'))

  expect_false(inc_or_not_exc('abc', 'abcd', 'abc'))
  expect_true(inc_or_not_exc('abc', 'abc', 'abcd'))

  expect_equal(inc_or_not_exc('abc', 'abcd', 'abcde'), NA)
})

inclusion = c('PNEUMONIA','PNA','SEPSIS','FEVER','ALTERED MENTAL STATUS')
exclusion = c('CHF','CONGESTIVE HEART FAILURE','INTRACRANIAL HEMORRHAGE','SUBARACHNOID HEMORRHAGE', 'blunt trauma')
temp_str = c('CHF - FEVER / test')
#temp_str = c("BACTEREMIA; TELEMETRY; PNEUMONIA; DYSRHYTHMIA; FEVER; BLUNT TRAUMA")
temp_df = data.frame(HADM_ID = c('1', '2', '3', '4'),
                     diagnosis = c('CHF - FEVER / test', 'pneumonia',
                                   'hemorrhage', 'intracranial hemorrhage'))

test_that("filter_admissiondiagnosis_string", {
  ## Check the dimensions
  expect_equal(dim(filter_admissiondiagnosis_string(temp_str, inclusion, exclusion)), c(3, 2))
  ## Check the mask
  expect_equal(filter_admissiondiagnosis_string(temp_str, inclusion, exclusion)$mask, c(FALSE, TRUE, NA))
  ## this also checks the str_to_lower and splitting on punctuation parts
})

test_that("filter_admissiondiagnosis_df", {
  ## Check the dimensions
  expect_equal(dim(filter_admissiondiagnosis_df(temp_df, 'diagnosis', inclusion, exclusion)), c(6, 4))
  ## Check the mask
  expect_equal(filter_admissiondiagnosis_df(temp_df, 'diagnosis', inclusion, exclusion)$mask, c(FALSE, TRUE, NA, TRUE, NA, FALSE))
  ## this also checks the str_to_lower and splitting on punctuation parts
})

test_that("filter_admissiondiagnosis_string_for_sepsis", {
  ## Check the dimensions
  expect_equal(dim(filter_admissiondiagnosis_string_for_sepsis(temp_str)), c(3, 2))
  ## Check the mask
  expect_equal(filter_admissiondiagnosis_string_for_sepsis(temp_str)$mask, c(FALSE, TRUE, NA))
  ## this also checks the str_to_lower and splitting on punctuation parts
})

test_that("filter_admissiondiagnosis_df_for_sepsis", {
  ## Check the dimensions
  expect_equal(dim(filter_admissiondiagnosis_df_for_sepsis(temp_df, 'diagnosis')), c(6, 4))
  ## Check the mask
  expect_equal(filter_admissiondiagnosis_df_for_sepsis(temp_df, 'diagnosis')$mask, c(FALSE, TRUE, NA, TRUE, NA, FALSE))
  ## this also checks the str_to_lower and splitting on punctuation parts
})
