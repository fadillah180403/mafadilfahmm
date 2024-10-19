# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

viterbimultobs <- function(transisi, emisi, p0, seq_observasi) {
  pengamatan <- length(seq_observasi)
  state <- length(p0)
  deltas <- matrix(0, nrow = pengamatan, ncol = state)
  psi <- matrix(0, nrow = pengamatan, ncol = state)
  pmult <- function(t, obs, j, ems) {
    prob <- 1
    for (i in seq_along(obs[[t]])) {
      prob <- prob * ems[[i]][j, obs[[t]][i]]
    }
    return(prob)
  }
  for (i in 1:state) {
    deltas[1, i] <- p0[i] * pmult(1, seq_observasi, i, emisi)
  }
  for (t in 2:pengamatan) {
    for (j in 1:state) {
      maxvector1 <- numeric(state)
      for (i in 1:state) {
        cekmax1 <- transisi[i, j] * deltas[t - 1, i]
        maxvector1[i] <- cekmax1
      }
      deltas[t, j] <- max(maxvector1) * pmult(t, seq_observasi, j, emisi)
      psi[t, j] <- which.max(maxvector1)
    }
  }
  qstar <- numeric(pengamatan)
  qstar[pengamatan] <- which.max(deltas[pengamatan, ])

  for (i in (pengamatan - 1):1) {
    qstar[i] <- psi[i + 1, qstar[i + 1]]
  }

  return(qstar)
}
