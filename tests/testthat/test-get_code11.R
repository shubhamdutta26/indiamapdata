test_that("state codes load correctly", {
  code <- get_code11()
  state_code <- get_code11("state")
  states_code <- get_code11("states")

  expect_identical(code, state_code)
  expect_identical(code, states_code)
  expect_identical(state_code, states_code)

  expect_equal(length(code), 3)
  expect_equal(length(code[[1]]), 37)

  expect_equal(code[[1, "abbr"]], "AN")
  expect_equal(code[[1, "code11"]], "35")
  expect_equal(code[[1, "stname"]], "ANDAMAN & NICOBAR")

  expect_equal(code[[37, "abbr"]], "WB")
  expect_equal(code[[37, "code11"]], "19")
  expect_equal(code[[37, "stname"]], "WEST BENGAL")
})

test_that("district codes load correctly", {
  district_code <- get_code11("district")
  districts_code <- get_code11("districts")

  expect_identical(district_code, districts_code)

  expect_equal(length(district_code), 6)
  expect_equal(length(district_code[[1]]), 755)

  expect_equal(district_code[[1, "dtname"]], "Nicobars")
  expect_equal(district_code[[1, "abbr"]], "AN")
  expect_equal(district_code[[1, "stname"]], "ANDAMAN & NICOBAR")
  expect_equal(district_code[[1, "code11"]], "35638")

  expect_equal(district_code[[755, "dtname"]], "Uttar Dinajpur")
  expect_equal(district_code[[755, "abbr"]], "WB")
  expect_equal(district_code[[755, "stname"]], "WEST BENGAL")
  expect_equal(district_code[[755, "code11"]], "19330")
})
