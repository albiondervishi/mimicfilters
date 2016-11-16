context("testing functions for the past medical history extraction function")

temp_str = "The quick brown fox jumps over the lazy dog.\nPast medical history: pt history of smoking related emphysema. Social history: test"

temp_df = data.frame(id = c(1,2,3),
                     text = c("blah, blah\nPast medical history: pt history of smoking related emphysema. Social history: test",
                              "blah, blah PRIOR MEDICAL HISTORY: excessive etoh, compromised liver function. blah: test",
                              "blah, blah history of present illness: obesity, hypertension, HF etc blah: test"))
exclusion = c('etoh', 'smok')

test_that("extract_pmh_string", {

  expect_equal(length(extract_pmh_string(temp_str)), 1)
  expect_true(nchar(extract_pmh_string(temp_str)) <= nchar(temp_str))
  expect_equal(extract_pmh_string(temp_str), "Past medical history: pt history of smoking related emphysema. Social h")
})

test_that("extract_pmhflag_string", {
  expect_equal(length(extract_pmhflag_string(temp_str, exclusion)), 1)
  expect_true(extract_pmhflag_string(temp_str, exclusion))
  expect_true(extract_pmhflag_string(temp_df$text[1], exclusion))
  expect_true(extract_pmhflag_string(temp_df$text[2], exclusion))
  expect_false(extract_pmhflag_string(temp_df$text[3], exclusion))

})

test_that("extract_pmhflag_string_for_hf", {
  expect_equal(length(extract_pmhflag_string_for_hf(temp_str)), 1)
  expect_false(extract_pmhflag_string_for_hf(temp_str))

  expect_false(extract_pmhflag_string_for_hf(temp_df$text[1]))
  expect_false(extract_pmhflag_string_for_hf(temp_df$text[2]))
  expect_true(extract_pmhflag_string_for_hf(temp_df$text[3]))

})

test_that("extract_pmhflag_df", {
  expect_error(extract_pmhflag_df(temp_df, 'test', exclusion), "The supplied diagnosis_column argument does not match a column name in the supplied data frame.")
  expect_equal(dim(extract_pmhflag_df(temp_df, 'text', exclusion)), c(3,3))
  expect_equal(extract_pmhflag_df(temp_df, 'text', exclusion)$pmhflag, list(TRUE, TRUE, FALSE))
})

test_that("extract_pmhflag_df_for_hf", {
  expect_error(extract_pmhflag_df_for_hf(temp_df, 'test'), "The supplied diagnosis_column argument does not match a column name in the supplied data frame.")
  expect_equal(dim(extract_pmhflag_df_for_hf(temp_df, 'text')), c(3,3))
  expect_equal(extract_pmhflag_df_for_hf(temp_df, 'text')$pmhflag, list(FALSE, FALSE, TRUE))
})

# temp_str = "blah past medical history: smoking related emphysema. blah: test"
# extract_pastmedicalhistory_string(temp_str)
# extract_pmhflag_string(temp_str, 'smok') # TRUE
# extract_pmhflag_string(temp_str, 'etoh') # FALSE
#
# temp_df = data.frame(id = c(1,2,3),
#   text = c("blah\nPast medical history: smoking related emphysema. blah: test",
#   "blah PRIOR MEDICAL HISTORY: excessive etoh. blah: test",
#   "blah history of present illness: obesity, hypertension, HF etc blah: test"))
#
# extract_pmhflag_df(temp_df, 'text', c('smok', 'etoh'))
# extract_pmhflag_df_for_hf(temp_df, 'text')
