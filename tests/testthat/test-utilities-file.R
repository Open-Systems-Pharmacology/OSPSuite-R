context(".homogenizePath")

test_that("Homogenizing an R path returns an R Path", {
  expect_equal(.homogenizePath("C:/temp/test.xml"), "C:/temp/test.xml")
})

test_that("Homogenizing a windows path should return an R Path", {
  expect_equal(.homogenizePath("C:\\temp\\test.xml"), "C:/temp/test.xml")
})

test_that("Homogenizing a windows path ending with \ should return an R Path", {
  expect_equal(.homogenizePath("C:\\temp\\"), "C:/temp")
})
