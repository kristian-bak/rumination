test_that(desc = "Show version number works", code = {
  
  ## Deliberately picking fixed elements (from 32 to 37) of div output to test if div output
  ## has changed
  x <- show_version_number() %>% 
    substring(32, 37)
  
  ## Expects "v" to be included in the version number
  expect_true(grepl("v", x))
  
  ## Expects version number to contain at least one digit
  expect_true(grepl("[[:digit:]]", x))
  
  ## Expect version number to contain at least one dot
  expect_true(grepl(".", x))
  
})
