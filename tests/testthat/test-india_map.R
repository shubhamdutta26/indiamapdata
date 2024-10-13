states_map <- india_map(regions = "states")
districts_map <- india_map(regions = "districts")

test_that("structure of states df is correct", {
  expect_equal(length(unique(states_map$stname)), 37)
})

test_that("structure of districts df is correct", {
  expect_equal(length(unique(districts_map$stname)), 36)
})

test_that("correct states are included", {
  include_abbr <- c("GJ", "WB", "NL")
  map_abbr <- india_map(regions = "states", include = include_abbr)

  include_full <- c("GUJARAT", "WEST BENGAL", "NAGALAND")
  map_full <- india_map(regions = "states", include = include_full)

  include_code <- c(24, 19, 13)
  map_code <- india_map(regions = "states", include = include_code)

  expect_equal(length(unique(map_abbr$stname)), length(include_abbr))
  expect_equal(length(unique(map_full$stname)), length(include_full))
  expect_equal(length(unique(map_code$stname)), length(include_code))
})

test_that("correct states are excluded", {
  full_map <- india_map(regions = "states")

  exclude_abbr <- c("GJ", "WB", "NL")
  map_abbr <- india_map(regions = "states", exclude = exclude_abbr)

  exclude_full <- c("GUJARAT", "WEST BENGAL", "NAGALAND")
  map_full <- india_map(regions = "states", exclude = exclude_full)

  exclude_code <- c(24, 19, 13)
  map_code <- india_map(regions = "states", exclude = exclude_code)

  expect_equal(length(unique(full_map$stname)) - length(unique(map_abbr$stname)), length(exclude_abbr))
  expect_equal(length(unique(full_map$stname)) - length(unique(map_full$stname)), length(exclude_full))
  expect_equal(length(unique(full_map$stname)) - length(unique(map_code$stname)), length(exclude_code))
})


test_that("structure of districts df is correct", {
  expect_equal(length(unique(districts_map$dtname)), 749)
})

test_that("singular regions value returns same data frames as plural", {
  state_map <- india_map(regions = "state")
  district_map <- india_map(regions = "district")

  expect_identical(states_map, state_map)
  expect_identical(districts_map, district_map)
})

test_that("error occurs for invalid region", {
  expect_error(india_map(regions = "cities"))
})

test_that("centroid labels are loaded", {
  expect_equal(length(centroid_labels("states")[[1]]), 37)
  #expect_equal(length(centroid_labels("districts")[[1]]), 672)
})
