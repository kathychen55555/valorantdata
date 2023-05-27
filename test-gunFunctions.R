test_that("case1 getGunStats", {
  expect_error(getGunStats("knife"), "Function must take in a gun from the game VALORANT")
  expect_error(getGunStats(12), "Function must take in the gun name in form of a string")
})




test_that("case1 getGunDamage", {
  expect_error(getGunDamage(12), "Function must take in the gun name in form of a string")
})
test_that("case2 getGunDamage", {
  expect_error(getGunDamage("pistol"), "Function must take in a gun from the game VALORANT")
})
test_that("case3 getGunDamage", {
  my_result <- getGunDamage("Odin")
  expect_true(class(my_result)== "data.frame")
})



test_that("case1 allGunStats", {
  expect_error(allGunStats(12), "Function must take in the gun statistic in form of a string")
})
test_that("case2 allGunStats", {
  expect_error(allGunStats("Fire_rate"), "Function must take in a property a gun has. This includes fireRate, magazineSize, equipTimeSeconds, and reloadTimeSeconds.")
})



test_that("case1 gunFireLevel", {
  expect_error(gunFireLevel(12), "Function must take in a gun penetration level in the form of a string")
})
test_that("case2 gunFireLevel", {
  expect_error(gunFireLevel("very low"), "Function must take in a wall penetration level that a gun has. This includes Low, Medium, High")
})



