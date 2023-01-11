## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, error = FALSE, warning = FALSE,
  comment = NA
)

## ----load-packages, include = FALSE-------------------------------------------
# Load packages and color palette
library(ggplot2)
theme_set(theme_bw())
library(hmmTMB)
pal <- hmmTMB:::hmmTMB_cols
set.seed(34564529)

## ----sim-data, fig.width = 6, fig.height = 3, out.width="80%", fig.align = "center"----
# Create empty data set to specify number of observations
n <- 500
data_sim <- data.frame(z = rep(NA, n))

# Hidden state process
hid_sim <- MarkovChain$new(data = data_sim, n_states = 2, 
                           tpm = matrix(c(0.95, 0.2, 0.05, 0.8), 2, 2))

# Observation process
par_sim <- list(z = list(mean = c(0, 5), sd = c(3, 1)))
obs_sim <- Observation$new(data = data_sim, dists = list(z = "norm"), 
                           n_states = 2, par = par_sim)

# Create HMM and simulate observations
hmm_sim <- HMM$new(hid = hid_sim, obs = obs_sim)
data_sim <- hmm_sim$simulate(n = n, silent = TRUE)

head(data_sim)

# Plot simulated time series
state_sim <- factor(attr(data_sim, "state"))
ggplot(data_sim, aes(1:nrow(data_sim), z, col = state_sim)) +
  geom_point() +
  labs(x = "time", y = "observation") +
  scale_color_manual(values = pal, name = "state")

## ----mod-spec1----------------------------------------------------------------
# Hidden state model
hid <- MarkovChain$new(data = data_sim, n_states = 2, 
                       initial_state = "stationary")

# Initial parameters for observation process
par <- list(z = list(mean = c(2, 7), sd = c(4, 0.5)))
obs <- Observation$new(data = data_sim, dists = list(z = "norm"), 
                       n_states = 2, par = par)

# Create HMM object
hmm <- HMM$new(hid = hid, obs = obs)

## ----check-priors-------------------------------------------------------------
hmm$priors()
hmm$coeff_fe()

## ----set-priors1--------------------------------------------------------------
# Parameter of normal priors for observation parameters
prior_obs <- matrix(c(0, 5, 
                      0, 5,
                      log(2), 5,
                      log(2), 5), 
                    ncol = 2, byrow = TRUE)

## ----set-priors2--------------------------------------------------------------
# Parameter of normal priors for transition probabilities
prior_hid <- matrix(c(-2, 1,
                      -2, 1),
                    ncol = 2, byrow = TRUE)

## ----set-priors3--------------------------------------------------------------
# Update priors
hmm$set_priors(new_priors = list(coeff_fe_obs = prior_obs, 
                                 coeff_fe_hid = prior_hid))

hmm$priors()

## ----fit-hmm, results = 'hide'------------------------------------------------
hmm$fit_stan(chains = 1, iter = 2000)

## ----rstan-plots, fig.width = 6, fig.height = 4, out.width="80%", fig.align = "center", fig.show = "hold"----
rstan::traceplot(hmm$out_stan())
rstan::stan_dens(hmm$out_stan())

## ----iters-hist, fig.width = 8, fig.height = 4, out.width="100%", fig.align = "center"----
iters <- hmm$iters()
head(iters)

iters_df <- as.data.frame.table(iters)
ggplot(iters_df, aes(x = Freq)) + 
    geom_histogram(bins = 20, fill = "lightgrey", col = "white") + 
    facet_wrap("Var2", nrow = 2, scales = "free_x")

## ----plot-dist, fig.width = 6, fig.height = 3, out.width="80%", fig.align = "center"----
hmm$plot_dist("z")

