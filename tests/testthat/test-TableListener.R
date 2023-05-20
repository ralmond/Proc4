test_that("Table Listener Constructor", {

})

test_that("Table Listener name", {
  tl <- TableListener("tester")
  expect_true(isListener(tl))
  expect_equal(listenerName(tl),"tester")
})

test_that("Table Listener Receive Message", {

})

test_that("Table Listener ignore Message", {

})
test_that("Table Listener reset", {

})
