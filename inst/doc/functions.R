## ----include = FALSE----------------------------------------------------------
path <- system.file("functions.json", package = "odin", mustWork = TRUE)
functions <- jsonlite::fromJSON(path, FALSE)
vcapply <- odin:::vcapply

render <- function(x) {
  render1 <- function(el) {
    if (!is.null(el$description)) {
      ret <- sprintf("**%s**: %s", el$title, el$description)
    } else {
      ret <- sprintf("**%s**", el$title)
    }
    if (!is.null(el$examples)) {
      examples <- vcapply(el$examples, function(ex)
        sprintf("`%s` &rarr; `%s`", ex[[1]], ex[[2]]))
      ret <- sprintf("%s (e.g., %s)", ret, paste(examples, collapse = "; "))
    }
    ret
  }
  value <- vapply(x, render1, "", USE.NAMES = FALSE)
  paste(sprintf("* `%s` -- %s\n", names(x), value), collapse = "\n")
}

## ----echo = FALSE, results = "asis"-------------------------------------------
writeLines(render(functions$basic))

## ----echo = FALSE, results = "asis"-------------------------------------------
writeLines(render(functions$arrays))

## ----echo = FALSE, results = "asis"-------------------------------------------
writeLines(render(functions$operators))

## -----------------------------------------------------------------------------
sqrt(3)^2 == 3

## ----echo = FALSE, results = "asis"-------------------------------------------
writeLines(render(functions$maths))

## ----echo = FALSE, results = "asis"-------------------------------------------
writeLines(render(functions$trig))

## ----echo = FALSE, results = "asis"-------------------------------------------
writeLines(render(functions$stochastic))

## ----echo = FALSE, results = "asis"-------------------------------------------
writeLines(render(functions$inplace))

