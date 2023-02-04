##' Skip tests if mongo collection cannot be opened.
##'
##' Opens a mongo connection, and generates a skip if there is an error.
##'
##' Args follow \code{\link[mongolite]{mongo}}.
skip_if_no_mongo <- function(collection="test", db="test",
                             url="mongodb://localhost",
                             verbose=FALSE, options=mongolite::ssl_options()) {
  
  try (con <- mongo(collection=collection, db=db, url=url, verbose=verbose,
                    options=options))
  if (is(con, "try-error"))
    skip("Could not open mongo collection")
  return(invisible(TRUE))
}
