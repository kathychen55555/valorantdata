test_that("simple case", {
  my_result <- getAgentInfo("breach")

  pred_result <- "The bionic Swede Breach fires powerful, targeted kinetic blasts to aggressively clear a path through enemy ground. The damage and disruption he inflicts ensures no fight is ever fair."

  expect_equal(my_result, pred_result)
})

test_that("error case1 agentInfo", {
  my_result <- getAgentInfo(12)

  pred_result <- "Function must take in agent name in the form of a string"

  expect_equal(my_result, pred_result)
})

test_that("error case2 agentInfo", {
  my_result <- getAgentInfo("Cameron")

  pred_result <- "Function must take in an agent from the game VALORANT"

  expect_equal(my_result, pred_result)
})
