suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  library(scales)
})

# ── colour palette ─────────────────────────────────────────────────────────────
COL_REWARD      <- "#DC143C"   # crimson  – true rewards
COL_PARTICIPANT <- "#001F5B"   # navy     – participant responses
COL_MODEL       <- "#00B4B4"   # turquoise– model predictions

# ── shared ggplot2 theme ───────────────────────────────────────────────────────
theme_pres <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle     = element_text(size = base_size - 2, colour = "#555555"),
      axis.title        = element_text(size = base_size - 1),
      legend.position   = "top",
      legend.title      = element_blank(),
      legend.key.width  = unit(1.8, "cm")
    )
}

# ── task data (seed = 42, 80 trials, 4 reversals) ─────────────────────────────
set.seed(42)
N_TRIALS    <- 80
block_means <- rep(c(70, 30, 70, 30), each = 20)
outcomes    <- round(pmin(pmax(rnorm(N_TRIALS, block_means, 10), 0), 100), 1)

# ── synthetic participant data (RW α = 0.3 + Gaussian noise σ = 8) ─────────────
set.seed(123)
.v <- 50; preds_true <- numeric(N_TRIALS)
for (.t in seq_len(N_TRIALS)) {
  preds_true[.t] <- .v
  .v <- .v + 0.3 * (outcomes[.t] - .v)
}
participant_ratings <- round(
  pmin(pmax(preds_true + rnorm(N_TRIALS, 0, 8), 0), 100), 1
)
rm(.v, .t, preds_true)

trials <- seq_len(N_TRIALS)

# ── reversal positions ─────────────────────────────────────────────────────────
reversal_x <- c(20.5, 40.5, 60.5)

# ── RW model functions ─────────────────────────────────────────────────────────
rw1 <- function(o, alpha, v0 = 50) {
  v <- numeric(length(o) + 1); v[1] <- v0
  for (t in seq_along(o)) v[t + 1] <- v[t] + alpha * (o[t] - v[t])
  v[seq_along(o)]
}

rw2 <- function(o, alpha, v0) {
  v <- numeric(length(o) + 1); v[1] <- v0
  for (t in seq_along(o)) v[t + 1] <- v[t] + alpha * (o[t] - v[t])
  v[seq_along(o)]
}

rw3 <- function(o, alpha_b, alpha_w, v0) {
  v <- numeric(length(o) + 1); v[1] <- v0
  for (t in seq_along(o)) {
    pe <- o[t] - v[t]
    a  <- if (pe > 0) alpha_b else alpha_w
    v[t + 1] <- v[t] + a * pe
  }
  v[seq_along(o)]
}

# RW₁ with threshold jump (for likelihood landscape demo)
rw1_thresh <- function(o, alpha, v0 = 50) {
  v <- numeric(length(o) + 1); v[1] <- v0
  for (t in seq_along(o)) {
    pe <- o[t] - v[t]
    v[t + 1] <- if (abs(pe) > 20) {
      if (pe > 0) 100 else 0
    } else {
      v[t] + alpha * pe
    }
  }
  v[seq_along(o)]
}

# Gaussian NLL  (σ in same units as outcomes, i.e. 0–100)
gauss_nll <- function(y_obs, mu_pred, sigma = 10) {
  -sum(dnorm(y_obs, mean = mu_pred, sd = sigma, log = TRUE))
}
