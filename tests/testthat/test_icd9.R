context("testing functions for the ICD-9 code function")

test_that("remove_icd9_periods", {
  expect_equal(remove_icd9_periods("123.45"), "12345")
  expect_equal(remove_icd9_periods("123.4"), "1234")
  expect_equal(remove_icd9_periods("123"), "123")

  expect_equal(remove_icd9_periods(c("123.45", "234.56")), c("12345", "23456"))
})

test_that("pad_icd9", {
  expect_equal(pad_icd9("1", 3), "001")
  expect_equal(pad_icd9("12", 3), "012")
  expect_equal(pad_icd9("123", 3), "123")
  expect_equal(pad_icd9(c("1","12", "234"), 3), c("001", "012", "234"))

  expect_equal(pad_icd9("123.4", 3), "123.4")
  expect_equal(pad_icd9("123.45", 3), "123.45")
  expect_equal(pad_icd9(c("123","123.4", "123.45"), 3), c("123", "123.4", "123.45"))
})

test_that("check_icd.test", {
  #expect_equal(check_icd9.test("123"), "123")
  #expect_equal(check_icd9.test("123.4"), "01234")
  # do not expect coded ICD-9s to be non-leaf values, that is should have two digits after the "."
  expect_equal(check_icd9.test("123.45"), "12345")
  expect_equal(check_icd9.test(123.45), "12345")

  expect_equal(check_icd9.test(list("123.45")), "12345")
  expect_equal(check_icd9.test(list(123.45)), "12345")

  expect_equal(check_icd9.test(list("123.45", "234.56")), c("12345", "23456"))
  expect_equal(check_icd9.test(list(123.45, 234.56)), c("12345", "23456"))

})


test_that("check_icd9.reference", {
  expect_equal(check_icd9.reference("1"), "001")
  expect_equal(check_icd9.reference("12"), "012")
  expect_equal(check_icd9.reference("123"), "123")
  expect_equal(check_icd9.reference("123.4"), "1234")
  expect_equal(check_icd9.reference("123.45"), "12345")
  expect_equal(check_icd9.reference(123.45), "12345")

  expect_equal(check_icd9.reference(list("123.45")), "12345")
  expect_equal(check_icd9.reference(list(123.45)), "12345")

  expect_equal(check_icd9.reference(list("123.45", "234.56")), c("12345", "23456"))
  expect_equal(check_icd9.reference(list(123.45, 234.56)), c("12345", "23456"))

})

test_that("filter_icd9_by_codes", {
  expect_true(filter_icd9_by_codes("1.23", '1'))
  expect_true(filter_icd9_by_codes("1.23", '1.2'))
  expect_true(filter_icd9_by_codes("1.23", '1.23'))

  expect_false(filter_icd9_by_codes("12.34", "1"))
  expect_true(filter_icd9_by_codes("12.34", "12"))
  expect_true(filter_icd9_by_codes("12.34", "12.3"))
  expect_true(filter_icd9_by_codes("12.34", "12.34"))

  expect_false(filter_icd9_by_codes("123.45", '1'))
  expect_false(filter_icd9_by_codes("123.45", '12'))
  expect_true(filter_icd9_by_codes("123.45", '123'))
  expect_true(filter_icd9_by_codes("123.45", '123.4'))
  expect_true(filter_icd9_by_codes("123.45", '123.45'))
  expect_false(filter_icd9_by_codes("123.45", c('1', '12', '23', '45')))

  expect_true(all(filter_icd9_by_codes(list("123.45", "234.56"), c("123", "234", "111"))))
  expect_length(filter_icd9_by_codes(list("123.45", "234.56"), c("123", "234", "111")), 2)

  expect_true(all(filter_icd9_by_codes(c("123.45", "234.56"), c("123", "234", "111"))))
  expect_length(filter_icd9_by_codes(c("123.45", "234.56"), c("123", "234", "111")), 2)

  ## no strings
  expect_true(filter_icd9_by_codes(1.23, 1))
  expect_true(filter_icd9_by_codes(1.23, 1.2))
  expect_true(filter_icd9_by_codes(1.23, 1.23))

  expect_false(filter_icd9_by_codes(12.34, 1))
  expect_true(filter_icd9_by_codes(12.34, 12))
  expect_true(filter_icd9_by_codes(12.34, 12.3))
  expect_true(filter_icd9_by_codes(12.34, 12.34))

  expect_false(filter_icd9_by_codes(123.45, 1))
  expect_false(filter_icd9_by_codes(123.45, 12))
  expect_true(filter_icd9_by_codes(123.45, 123))
  expect_true(filter_icd9_by_codes(123.45, 123.4))
  expect_true(filter_icd9_by_codes(123.45, 123.45))
  expect_false(filter_icd9_by_codes(123.45, c(1, 12, 23, 45)))

  expect_true(all(filter_icd9_by_codes(list(123.45, 234.56), c(123, 234, 111))))
  expect_length(filter_icd9_by_codes(list(123.45, 234.56), c(123, 234, 111)), 2)

  expect_true(all(filter_icd9_by_codes(c(123.45, 234.56), c(123, 234, 111))))
  expect_length(filter_icd9_by_codes(c(123.45, 234.56), c(123, 234, 111)), 2)

  data = data.frame(id = c(1, 2, 3, 4, 5, 6), ICD9 = c('123.45', '234.56', '345.67', '456.78','567.89', '678.90'))
  expect_equal(filter_icd9_by_codes(data$ICD9, c('123', '345', '567')), c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
})

