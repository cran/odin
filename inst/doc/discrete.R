## ----setup, echo = FALSE, results = "hide"------------------------------------
lang_output <- function(x, lang) {
  cat(c(sprintf("```%s", lang), x, "```"), sep = "\n")
}
c_output <- function(x) lang_output(x, "cc")
r_output <- function(x) lang_output(x, "r")
plain_output <- function(x) lang_output(x, "plain")
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 5)
options(odin.verbose = FALSE)

## ----load_sir-----------------------------------------------------------------
path_sir_model <- system.file("examples/discrete_deterministic_sir.R", package = "odin")

## ----echo = FALSE, results = "asis"-------------------------------------------
r_output(readLines(path_sir_model))

## -----------------------------------------------------------------------------
sir_generator <- odin::odin(path_sir_model)
sir_generator

## -----------------------------------------------------------------------------
x <- sir_generator$new()
x

## ----sir-deterministic, fig.cap = "<i>An example of deterministic, discrete-time SIR model</i><br>"----
sir_col <- c("#8c8cd9", "#cc0044", "#999966")
x$run(0:10)
x_res <- x$run(0:200)
par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(x_res[, 1], x_res[, -1], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = sir_col, lty = 1)
legend("topright", lwd = 1, col = sir_col, legend = c("S", "I", "R"), bty = "n")

## ----load_sir_s---------------------------------------------------------------
path_sir_model_s <- system.file("examples/discrete_stochastic_sir.R", package = "odin")

## ----echo = FALSE, results = "asis"-------------------------------------------
r_output(readLines(path_sir_model_s))

## -----------------------------------------------------------------------------
sir_s_generator <- odin::odin(path_sir_model_s)
sir_s_generator
x <- sir_s_generator$new(I_ini = 10)

## ----sir-stochastic_1, fig.cap = "<i>An example of stochastic, discrete-time SIR model</i><br>"----
set.seed(1)
x_res <- x$run(0:100)
par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(x_res[, 1], x_res[, -1], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = sir_col, lty = 1)
legend("topright", lwd = 1, col = sir_col, legend = c("S", "I", "R"), bty = "n")

## -----------------------------------------------------------------------------
path_sir_model_s_a <- system.file("examples/discrete_stochastic_sir_arrays.R", package = "odin")

## ----echo = FALSE, results = "asis"-------------------------------------------
r_output(readLines(path_sir_model_s_a))

## ----echo = TRUE--------------------------------------------------------------
sir_s_a_generator <- odin::odin(path_sir_model_s_a)
sir_s_a_generator
x <- sir_s_a_generator$new()

## ----sir-stochastic_100, fig.cap = "<i>100 replicates of a stochastic, discrete-time SIR model</i><br>"----
set.seed(1)
sir_col_transp <- paste0(sir_col, "66")
x_res <- x$run(0:100)
par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(x_res[, 1], x_res[, -1], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = rep(sir_col_transp, each = 100), lty = 1)
legend("left", lwd = 1, col = sir_col, legend = c("S", "I", "R"), bty = "n")

## ----load_seirds--------------------------------------------------------------
path_seirds_model <- system.file("examples/discrete_stochastic_seirds.R", package = "odin")

## ----echo = FALSE, results = "asis"-------------------------------------------
r_output(readLines(path_seirds_model))

## ----seirds-------------------------------------------------------------------
seirds_generator <- odin::odin(path_seirds_model)
seirds_generator
x <- seirds_generator$new()


seirds_col <- c("#8c8cd9", "#e67300", "#d279a6", "#ff4d4d", "#999966", "#660000")

set.seed(1)
x_res <- x$run(0:365)
par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(x_res[, 1], x_res[, -1], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = seirds_col, lty = 1)
legend("left", lwd = 1, col = seirds_col, legend = c("S", "E", "Ir", "Id", "R", "D"), bty = "n")

## ----seirds_100, fig.cap = "<i>100 replicates of a stochastic, discrete-time SEIRDS model</i><br>"----
x_res <- as.data.frame(replicate(100, x$run(0:365)[, -1]))
dim(x_res)
x_res[1:6, 1:10]

seirds_col_transp <- paste0(seirds_col, "1A")
par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(0:365, x_res, xlab = "Time", ylab = "Number of individuals",
        type = "l", col = rep(seirds_col_transp, 100), lty = 1)
legend("right", lwd = 1, col = seirds_col,
       legend = c("S", "E", "Ir", "Id", "R", "D"), bty = "n")

## ----custom_function----------------------------------------------------------
check_model <- function(n = 50, t = 0:365, alpha = 0.2, ...,
                        legend_pos = "topright") {
  model <- seirds_generator(...)
  col <- paste0(seirds_col, "33")

  res <- as.data.frame(replicate(n, model$run(t)[, -1]))
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
  matplot(t, res, xlab = "Time", ylab = "",  type = "l",
          col = rep(col, n), lty = 1)
  mtext("Number of individuals", side = 2, line = 3.5, las = 3, cex = 1.2)
  legend(legend_pos, lwd = 1, col = seirds_col,
         legend = c("S", "E", "Ir", "Id", "R", "D"), bty = "n")
}

## ----fig.cap = "<i>Stochastic SEIRDS model: sanity check with no infections</i><br>"----
check_model(beta = 0, epsilon = 0)

## ----fig.cap = "<i>Stochastic SEIRDS model: no importation or waning immunity</i><br>"----
check_model(epsilon = 0, omega = 0)

## ----fig.cap = "<i>Stochastic SEIRDS model: endemic state in a larger population</i><br>"----
check_model(t = 0:(365 * 3), epsilon = 0.1, beta = .2, omega = .01,
            mu = 0.005, S_ini = 1e5)

