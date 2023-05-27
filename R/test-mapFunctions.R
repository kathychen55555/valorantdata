test_that("case1 getMapInfo", {
  expect_error(getMapInfo(12), "Function must take in the map name in form of a string")
  expect_error(getMapInfo("disneyland"), "Function must take in a map from the game VALORANT")
})


