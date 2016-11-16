context("testing functions for the EF parsing code functions")

test_that("ef_detect_TEE", {
  expect_false(ef_detect_TEE("Test: TTE"))
  expect_false(ef_detect_TEE("\nTest: TTE (Complete)\nDoppler: Full Doppler"))
  expect_true(ef_detect_TEE("Test: TEE"))
  expect_true(ef_detect_TEE("\nTest: TEE (Complete)\nDoppler: Full Doppler"))

  expect_false(is.na(extract_ejectionfraction_from_string("\nTest: TTE (Complete)\nDoppler: Full Doppler, LVEF = 55% blah")))
  expect_true(is.na(extract_ejectionfraction_from_string("\nTest: TEE (Complete)\nDoppler: Full Doppler, LVEF = 55% blah")))
})

test_that("ef_detect_extract_numeric", {
  expect_true(is.na(ef_detect_extract_numeric("hello_world%")))
  expect_true(is.na(ef_detect_extract_numeric("ef blah %")))
  expect_true(is.na(ef_detect_extract_numeric("ef blah blah blah blah%")))
  # Greater than 15 characters between ef and %

  expect_equal(ef_detect_extract_numeric("lvef > 55%"), 55)
  expect_equal(ef_detect_extract_numeric("lvef = 50-70%"), c(50, 70))
  expect_equal(ef_detect_extract_numeric("lvef=50-70%"), c(50, 70))
  expect_equal(ef_detect_extract_numeric("lvef = 50 - 70%"), c(50, 70))
  expect_equal(ef_detect_extract_numeric("lvef = 5-30%"), c(5, 30))
  # check for the special case of single digit 5

  long_test_string = str_to_lower("LVEF = 55% blah blah blah blah LVEF = 20-30% blah blah blah blah LVEF = 50 - 70% blah")
  expect_length(ef_detect_extract_numeric(long_test_string), 5)
  expect_equal(ef_detect_extract_numeric(long_test_string), c(55, 20, 30, 50, 70))
})

test_that("ef_string_to_numeric", {
  expect_equal(ef_string_to_numeric(list('severe')), c(5, 29))
  expect_equal(ef_string_to_numeric(list('severely')), c(5, 29))

  expect_equal(ef_string_to_numeric(list('moderate')), c(31, 39))
  expect_equal(ef_string_to_numeric(list('moderately')), c(31, 39))

  expect_equal(ef_string_to_numeric(list('mild')), c(41, 49))
  expect_equal(ef_string_to_numeric(list('mildly')), c(41, 49))

  expect_equal(ef_string_to_numeric(list('normal')), c(51, 99))
  expect_equal(ef_string_to_numeric(list('preserved')), c(51, 99))
})


test_that("ef_detect_coerce_string", {

  expect_equal(ef_detect_coerce_string('LV systolic function is mildly depressed'), c(41, 49))
  expect_equal(ef_detect_coerce_string('left ventricle systolic function is mildly depressed'), c(41, 49))
  expect_equal(ef_detect_coerce_string('left ventricular systolic function is mildly depressed'), c(41, 49))

  expect_equal(ef_detect_coerce_string('LV systolic function is moderately depressed'), c(31, 39))
  expect_equal(ef_detect_coerce_string('left ventricle systolic function is moderately depressed'), c(31, 39))
  expect_equal(ef_detect_coerce_string('left ventricular systolic function is moderately depressed'), c(31, 39))

  expect_equal(ef_detect_coerce_string('LV systolic function is severely depressed'), c(5, 29))
  expect_equal(ef_detect_coerce_string('left ventricle systolic function is severely depressed'), c(5, 29))
  expect_equal(ef_detect_coerce_string('left ventricular systolic function is severely depressed'), c(5, 29))

  expect_equal(ef_detect_coerce_string('mildly blah \n\n blah depressed LV systolic function'), c(41, 49))
  expect_equal(ef_detect_coerce_string('moderately blah blah depressed LV systolic function'), c(31, 39))
  expect_equal(ef_detect_coerce_string('severely blah blah depressed LV systolic function'), c(5, 29))

})

## For the usage example
# temp_list = list('EF 20-25 %', 'EF=20%','LVEF \n\n\n > 35%', 'EF = 35 - 50%',
#                  'EF<35% blah blah EF < 35%', ' TEE EF>55%', 'LVEF = 5%',
#                  ' LEFT blah 2%. \n blah LVEF = 9%', 'LEFT blah blah blah blah 50%',
#                  'blah 5% blah blah EF = 70%')
# expect_equal(lapply(temp_list, extract_ejectionfraction_from_string), list(c(20, 25), 20, 35, c(35, 50), c(35,35), 55, 5, c(2, 9), NA, 70))
#
# temp_str = "severely depressed left ventricular systolic function"
# extract_ejectionfraction_from_string(temp_str)
# echo_list = list( "LVEF > 55%", "LVEF = 20 - 30%")
# lapply(echo_list, extract_ejectionfraction_from_string)
