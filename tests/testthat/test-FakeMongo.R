test_that("Fake Mongo Utilities", {
  fm1 <- fake_mongo()
  c1 <- rlang::call2(fm1$remove,'{}')
  expect_equal(call_name2(c1), "remove")
  expect_equal(call_args2(c1), list(query='{}', just_one=FALSE))
  fm1$setTransactions(
          list(call(fm1$count),call(fm1$disconnect),
               call(fm1$disconnect,FALSE),
               call(fm1$count,'{}')),
          list(1,NULL,NULL,1)
      )
  expect_equal(fm1$position,0L)
  expect_success(out <- fm1$count())
  expect_equal(out,1)
  expect_equal(fm1$position,1L)
  expect_failure(fm1$drop()) ## Name missmatch
  expect_failure(fm1$disconnect(TRUE)) ## Arg mismatch
  expect_success(out <- fm1$count())
  expect_equal(out,1)
  expect_failure(fm1$count()) ## Too many transactions
})
