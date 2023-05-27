test_that("simple case", {
  my_result <- getAgentInfo("breach")

  pred_result <- "The bionic Swede Breach fires powerful, targeted kinetic blasts to aggressively clear a path through enemy ground. The damage and disruption he inflicts ensures no fight is ever fair."

  expect_equal(my_result, pred_result)
})

test_that("error cases agentInfo", {
  expect_error(getAgentInfo(12), "Function must take in agent name in the form of a string")
  expect_error(getAgentInfo("Cameron"), "Function must take in an agent from the game VALORANT.")
})

test_that("simple case", {
  my_result <- getAgentAbilities("sova")

  expect_true(class(my_result) == "data.frame")

  expect_true(names(my_result)[1] == "slot")

  expect_true(names(my_result)[2] == "displayName")

  expect_true(names(my_result)[3] == "description")
})
