donnees_brute <- olympicsWeather::perform_request(48.85,2.35)
result <-olympicsWeather::unnest_response(donnees_brute)

testthat::test_that("unnest_response renvoie le bon nombre de lignes", {
  expect_equal(nrow(result), length(donnees_brute$hourly[[1]]))
})

testthat::test_that("le nom des colonnes en sortie est correct", {
  expect_equal(names(result), c("date_heure", "temperature_celsius", "temperature_ressentie_celsius", "precipitation_proba", "precipitation"))
})

testthat::test_that("le nombre de colonnes en sortie est correct", {
  expect_equal(ncol(result), 5)
})
