test_that("error case1 getGunStats", {
  my_result <- getGunStats(12)

  pred_result <- "Function must take in the gun name in form of a string"

  expect_equal(my_result, pred_result)
})

test_that("error case2 getGunStats", {
  my_result <- getGunStats("Bodwin")

  pred_result <- "Function must take in a gun from the game VALORANT"

  expect_equal(my_result, pred_result)
})





test_that("error case1 getGunDamage", {
  my_result <- getGunDamage(12)

  pred_result <- "Function must take in the gun name in form of a string"

  expect_equal(my_result, pred_result)
})

test_that("error case2 getGunDamage", {
  my_result <- getGunDamage("Bodwin")

  pred_result <- "Function must take in a gun from the game VALORANT"

  expect_equal(my_result, pred_result)
})





test_that("error case1 allGunStats", {
  my_result <- allGunStats(12)

  pred_result <- "Function must take in the gun statistic in form of a string"

  expect_equal(my_result, pred_result)
})

test_that("error case2 allGunStats", {
  my_result <- allGunStats("Fire_rate")

  pred_result <- "Function must take in a property a gun has. This includes fireRate, magazineSize, equipTimeSeconds, and reloadTimeSeconds."

  expect_equal(my_result, pred_result)
})



test_that("error case1 gunFireLevel", {
  my_result <- allGunStats(12)

  pred_result <- "Function must take in a gun penetration level in the form of a string"

  expect_equal(my_result, pred_result)
})

test_that("error case2 gunFireLevel", {
  my_result <- allGunStats("very low")

  pred_result <- "Function must take in a wall penetration level that a gun has. This includes Low, Medium, High"

  expect_equal(my_result, pred_result)
})



