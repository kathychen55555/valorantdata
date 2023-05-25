test_that("simple case gunStats", {
  my_result <- getGunStats("vandal")

  expect_equal(names(my_result),
               c("gunName", "fireRate", "magazineSize", "runSpeedMultiplier",
                 "equipTimeSeconds", "reloadTimeSeconds", "firstBulletAccuracy",
                 "shotgunPelletCount", "wallPenetration"))

  expect_equal(class(my_result), "data.frame")
})

test_that("error case1 gunStats", {
  my_result <- getGunStats(1)

  expect_equal(my_result, "Function must take in the gun name in form of a string")
})

test_that("error case2 gunStats", {
  my_result <- getGunStats("Kathy")

  expect_equal(my_result, "Function must take in a gun from the game VALORANT")
})

test_that("simple case gunDamage", {
  my_result <- getGunDamage("phantom")

  expect_equal(names(my_result), c("rangeStartMeters",
  "rangeEndMeters", "headDamage", "bodyDamage", "legDamage"))
})

test_that("error case1 gunDamage", {
  my_result <- getGunDamage(123123)

  expect_equal(my_result, "Function must take in the gun name in form of a string")

})

test_that("error case2 gunDamage", {
  my_result <- getGunDamage("BRANDON")

  expect_equal(my_result, "Function must take in a gun from the game VALORANT")
})
