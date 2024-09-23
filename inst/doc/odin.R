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

## -----------------------------------------------------------------------------
path_logistic <- system.file("examples/logistic.R", package = "odin")

## ----echo = FALSE, results = "asis"-------------------------------------------
r_output(readLines(path_logistic))

## -----------------------------------------------------------------------------
generator <- odin::odin(path_logistic)

## -----------------------------------------------------------------------------
mod <- generator$new()
mod

## -----------------------------------------------------------------------------
mod$init

## -----------------------------------------------------------------------------
mod$deriv(0, mod$initial(0))

## -----------------------------------------------------------------------------
mod$deriv(0, 50)

## -----------------------------------------------------------------------------
mod$contents()

## -----------------------------------------------------------------------------
tt <- seq(0, 30, length.out = 101)
y <- mod$run(tt)
plot(y, xlab = "Time", ylab = "N", las = 1, main = "")

## -----------------------------------------------------------------------------
y2 <- mod$run(tt, 50)
plot(y, xlab = "Time", ylab = "N", las = 1, main = "")
lines(y2, col = "red")

## -----------------------------------------------------------------------------
generator <- odin::odin({
  deriv(N) <- r * N * (1 - N / K)
  initial(N) <- N0

  N0 <- user(1)
  K <- user(100)
  r <- user()
})

## -----------------------------------------------------------------------------
mod <- generator$new(r = 1)

## -----------------------------------------------------------------------------
mod$contents()$r

## -----------------------------------------------------------------------------
y3 <- mod$run(tt)
plot(y, xlab = "Time", ylab = "N", las = 1, main = "")
lines(y3, col = "red")

## -----------------------------------------------------------------------------
mod$set_user(r = 0.25, K = 75, N0 = 10)
y4 <- mod$run(tt)
plot(y, xlab = "Time", ylab = "N", las = 1, main = "")
lines(y3, col = "red")
lines(y4, col = "blue")

## -----------------------------------------------------------------------------
path_lorenz <- system.file("examples/lorenz.R", package = "odin")

## ----echo = FALSE, results = "asis"-------------------------------------------
r_output(readLines(path_lorenz))

## -----------------------------------------------------------------------------
generator <- odin::odin(path_lorenz)
mod <- generator$new()

## -----------------------------------------------------------------------------
tt <- seq(0, 100, length.out = 20000)
system.time(y <- mod$run(tt))
pairs(y[, -1L], panel = lines, lwd = .2, col = "#00000055")

## -----------------------------------------------------------------------------
gen <- odin::odin({
  ylag <- delay(y, tau)
  initial(y) <- 0.5
  deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
  tau <- user(10)
  output(ylag) <- ylag
})
dde <- gen$new()

## -----------------------------------------------------------------------------
t <- seq(0, 300, length.out = 301)
y1 <- dde$run(t)
plot(y1, ylab = "y", mfrow = c(1, 2), which = 1)
plot(y1[, -1L], xlab = "y", ylab = "ylag", mfrow = NULL, type = "l")

## -----------------------------------------------------------------------------
dde$set_user(tau = 20)
y2 <- dde$run(t)
plot(y2, ylab = "y", mfrow = c(1, 2), which = 1)
plot(y2[, -1L], xlab = "y", ylab = "ylag", mfrow = NULL, type = "l")

## -----------------------------------------------------------------------------
gen <- odin::odin({
  deriv(y[]) <- r[i] * y[i] * (1 - sum(ay[i, ]))
  initial(y[]) <- y0[i]

  y0[] <- user()
  r[] <- user()
  a[, ] <- user()
  ay[, ] <- a[i, j] * y[j]

  dim(r) <- user()
  n_spp <- length(r)

  dim(y) <- n_spp
  dim(y0) <- n_spp
  dim(a) <- c(n_spp, n_spp)
  dim(ay) <- c(n_spp, n_spp)

  config(base) <- "lv4"
})

## -----------------------------------------------------------------------------
pars <- list(r = c(1.00, 0.72, 1.53, 1.27),
             a = rbind(c(1.00, 1.09, 1.52, 0.00),
                       c(0.00, 1.00, 0.44, 1.36),
                       c(2.33, 0.00, 1.00, 0.47),
                       c(1.21, 0.51, 0.35, 1.00)),
             y0 = c(0.3013, 0.4586, 0.1307, 0.3557))

## -----------------------------------------------------------------------------
mod <- gen$new(user = pars)

t <- seq(0, 2000, length.out = 10001)
y <- mod$run(t)
pairs(y[, -1], panel = lines, col = "#00000055", lwd = 0.2)

## -----------------------------------------------------------------------------
flux_model <- odin::odin({
  deriv(C) <- flux - kk * C
  initial(C) <- C0
  flux <- interpolate(flux_t, flux_y, "linear")
  C0 <- user()
  kk <- user()
  output(deposition) <- kk * C
  ## Fair bit of boilerplate here that may be removed in future
  ## versions:
  flux_t[] <- user()
  flux_y[] <- user()
  dim(flux_t) <- user()
  dim(flux_y) <- user()
})

## -----------------------------------------------------------------------------
flux_t <- c(1, 11, 21, 41, 73, 83, 93, 103, 113, 123, 133, 143, 153,
            163, 173, 183, 194, 204, 214, 224, 234, 244, 254, 264,
            274, 284, 294, 304, 315, 325, 335, 345, 355, 365)
flux_y <- c(0.654, 0.167, 0.06, 0.07, 0.277, 0.186, 0.14, 0.255, 0.231,
            0.309, 1.127, 1.923, 1.091, 1.001, 1.691, 1.404, 1.226, 0.767,
            0.893, 0.737, 0.772, 0.726, 0.624, 0.439, 0.168, 0.28, 0.202,
            0.193, 0.286, 0.599, 1.889, 0.996, 0.681, 1.135)
plot(flux_t, flux_y, type = "l", ylab = "Flux", xlab = "Time")

## -----------------------------------------------------------------------------
k <- 0.01
C0 <- mean(approx(flux_t, flux_y, xout = 1:365)$y) / k

## -----------------------------------------------------------------------------
mod <- flux_model$new(kk = k, C0 = C0, flux_t = flux_t, flux_y = flux_y)

## -----------------------------------------------------------------------------
t <- seq(1, 365)
y <- mod$run(t, tcrit = 365)

## -----------------------------------------------------------------------------
plot(flux_t, flux_y, type = "l", col = "red", ylab = "Flux & deposition")
lines(t, y[, 3], col = "blue")

## -----------------------------------------------------------------------------
flux_model_c <- odin::odin({
  deriv(C) <- flux - kk * C
  initial(C) <- C0
  flux <- interpolate(flux_t, flux_y, "constant")
  C0 <- user()
  kk <- user()
  output(flux) <- flux
  output(deposition) <- kk * C
  ## Fair bit of boilerplate here that may be removed in future
  ## versions:
  flux_t[] <- user()
  flux_y[] <- user()
  dim(flux_t) <- user()
  dim(flux_y) <- user()
})

mod_c <- flux_model_c$new(kk = k, C0 = C0, flux_t = flux_t, flux_y = flux_y)
y_c <- mod_c$run(t, tcrit = 365)

mod_c$output_order
plot(t, y_c[, 3], type = "s", col = "red", ylab = "Flux & deposition")
lines(t, y_c[, 4], col = "blue")

## -----------------------------------------------------------------------------
flux_model_s <- odin::odin({
  deriv(C) <- flux - kk * C
  initial(C) <- C0
  flux <- interpolate(flux_t, flux_y, "spline")
  C0 <- user()
  kk <- user()
  output(flux) <- flux
  output(deposition) <- kk * C
  ## Fair bit of boilerplate here that may be removed in future
  ## versions:
  flux_t[] <- user()
  flux_y[] <- user()
  dim(flux_t) <- user()
  dim(flux_y) <- user()
})

mod_s <- flux_model_s$new(kk = k, C0 = C0, flux_t = flux_t, flux_y = flux_y)
y_s <- mod_s$run(t, tcrit = 365)
plot(t, y_s[, 3], type = "l", col = "red", ylab = "Flux & deposition")
lines(t, y_s[, 4], col = "blue")

