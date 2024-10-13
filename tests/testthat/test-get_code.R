test_that("state codes codes load correctly", {
  code <- get_code()
  state_code <- get_code("state")
  states_code <- get_code("states")

  expect_identical(code, state_code)
  expect_identical(code, states_code)
  expect_identical(state_code, states_code)

  expect_equal(length(code), 3)
  expect_equal(length(code[[1]]), 37)

  expect_equal(code[[1, "abbr"]], "AN")
  expect_equal(code[[1, "stcode11"]], "35")
  expect_equal(code[[1, "stname"]], "ANDAMAN & NICOBAR")

  expect_equal(code[[37, "abbr"]], "WB")
  expect_equal(code[[37, "stcode11"]], "19")
  expect_equal(code[[37, "stname"]], "WEST BENGAL")
})

test_that("district codes load correctly", {
  district_code <- get_code("district")
  districts_code <- get_code("districts")

  expect_identical(district_code, districts_code)

  expect_equal(length(district_code), 5)
  expect_equal(length(district_code[[1]]), 755)

  expect_equal(district_code[[1, "dtname"]], "Nicobars")
  expect_equal(district_code[[1, "abbr"]], "AN")
  expect_equal(district_code[[1, "stname"]], "ANDAMAN & NICOBAR")
  expect_equal(district_code[[1, "dtcode11"]], "638")

  expect_equal(district_code[[755, "dtname"]], "Uttar Dinajpur")
  expect_equal(district_code[[755, "abbr"]], "WB")
  expect_equal(district_code[[755, "stname"]], "WEST BENGAL")
  expect_equal(district_code[[755, "dtcode11"]], "330")
})
