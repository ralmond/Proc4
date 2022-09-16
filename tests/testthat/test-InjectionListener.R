


mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                   sender="EIEvent",mess="New Observables",
                   details=list(trophy="gold",solvedtime=10))
test_that("multiplication works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed(mongolite)
  col = mongolite::mongo()
  ilwind <- InjectionListener(sender="EIEvent",messSet="New Observables")
  receiveMessage(ilwind,mess1)
})
