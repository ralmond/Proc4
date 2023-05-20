test_that("Capture Listener Constructor", {

})

test_that("Capture Listener name", {
    cl <- CaptureListener("tester")
    expect_true(isListener(cl))
    expect_equal(listenerName(cl),"tester")
})

test_that("Capture Listener Receive Message", {

})

test_that("Capture Listener ignore Message", {

})

test_that("Capture Listener reset",{

})

test_that("Capture Listener lastMessage",{

})

test_that("Capture Listener buildListener",{

})
