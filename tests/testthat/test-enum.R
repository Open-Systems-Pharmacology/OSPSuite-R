context("Enum")

test_that("It creates an enum from keys only", {
  myEnum <- enum(c("Red", "Blue", "Green"))
  expect_equal(names(myEnum), c("Red", "Blue", "Green"))
})

test_that("It creates an enum from keys and values", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_equal(names(myEnum), c("Diamond", "Triangle", "Circle"))
})

test_that("It throws an error when not all values are provided", {
  expect_error(myEnum <- enum(c(Diamond = 1, 2, Circle = 2)), regexp = messages$errorEnumNotAllNames)
})

test_that("getEnumKey returns the correct key", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_equal(getEnumKey(enum = myEnum, value = 2), c("Triangle", "Circle" ))
  expect_equal(getEnumKey(enum = myEnum, value = 1), c("Diamond" ))
})

test_that("getEnumKey returns NULL if the value is not present", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_null(getEnumKey(enum = myEnum, value = 3), c("Triangle", "Circle" ))
})

test_that("enumGetValue returns the correct value", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_equal(enumGetValue(enum = myEnum, key = "Triangle"), 2)
})

test_that("enumGetValue throws an error if the key is not present", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_error(enumGetValue(enum = myEnum, key = "Square"), messages$errorKeyNotInEnum("Square"))
})

test_that("enumKeys returns the keys of an enum", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_equal(enumKeys(myEnum), c("Diamond", "Triangle", "Circle"))
})

test_that("enumHasKey returns TRUE if a key is present", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_true(enumHasKey(myEnum, key = "Diamond"))
})

test_that("enumHasKey returns FALSE if a key is not present", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_false(enumHasKey(myEnum, key = "DDiamond"))
})

test_that("enumPut adds one key that is not present", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  myEnum <- enumPut(enum = myEnum, keys = "Square", values = 3)
  expect_equal(myEnum$Square, 3)
})

test_that("enumPut throws an error if the key is present and overwrite = FALSE", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_error(enumPut(enum = myEnum, keys = "Diamond", values = 3), regexp = messages$errorKeyInEnumPresent("Diamond"))
})

test_that("enumPut adds one key that is present if overwrite is TRUE", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  myEnum <- enumPut(enum = myEnum, keys = "Diamond", values = 3, overwrite = TRUE)
  expect_equal(myEnum$Diamond, 3)
})

test_that("enumPut adds multiple keys that are not present", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  myEnum <- enumPut(enum = myEnum, keys = c("Square", "Cross"), values = c(3, 4))
  expect_equal(myEnum$Square, 3)
  expect_equal(myEnum$Cross, 4)
})

test_that("enumPut throws an error if one of the keys is present and overwrite = FALSE", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_error(enumPut(enum = myEnum, keys = c("Square", "Diamond"), values = c(3, 4)), regexp = messages$errorKeyInEnumPresent("Diamond"))
})

test_that("enumPut adds multiple keys with one already present if overwrite is TRUE", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  myEnum <- enumPut(enum = myEnum, keys = c("Square", "Diamond"), values = c(3, 4), overwrite = TRUE)
  expect_equal(myEnum$Square, 3)
  expect_equal(myEnum$Diamond, 4)
})

test_that("enumRemove removes one key", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  myEnum <- enumRemove(keys = "Diamond", enum = myEnum)
  expect_equal(names(myEnum), c("Triangle", "Circle" ))
})

test_that("enumRemove removes multiple keys", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  myEnum <- enumRemove(keys = c("Diamond", "Circle"), enum = myEnum)
  expect_equal(names(myEnum), c("Triangle"))
})

test_that("enumRemove does nothing if the key is not present", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  myEnum <- enumRemove(keys = "Sun", enum = myEnum)
  expect_equal(names(myEnum), c("Diamond", "Triangle", "Circle"))
})

test_that("enumRemove removes present keys if on of the keys is not present", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  myEnum <- enumRemove(keys = c("Diamond", "Sun", "Circle"), enum = myEnum)
  expect_equal(names(myEnum), c("Triangle"))
})

test_that("enumValues returns the values", {
  myEnum <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
  expect_equal(enumValues(myEnum), c(1, 2, 2))
})
