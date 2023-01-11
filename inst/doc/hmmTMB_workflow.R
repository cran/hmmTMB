## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, error = FALSE, warning = FALSE,
  comment = NA
)

## ----load-stuff---------------------------------------------------------------
# Load packages and color palette
library(ggplot2)
theme_set(theme_bw())
library(hmmTMB)
pal <- hmmTMB:::hmmTMB_cols

## ----head-data----------------------------------------------------------------
data(energy, package = "MSwM")
head(energy)

## ----add-date, fig.width = 6, fig.height = 3, out.width="60%", fig.align = "center"----
# Create a grid of days that excludes weekends
day1 <- as.POSIXct("2002/01/01", format = "%Y/%m/%d", tz = "UTC")
day2 <- as.POSIXct("2008/10/31", format = "%Y/%m/%d", tz = "UTC")
days_with_we <- seq(day1, day2, by = "1 day")
which_we <- which(format(days_with_we, '%u') %in% 6:7)
days <- days_with_we[-which_we]
energy$Day <- days

ggplot(energy, aes(Day, Price)) + geom_line()

## ----r6-1, eval = FALSE-------------------------------------------------------
#  object <- Class$new(...)

## ----r6-2, eval = FALSE-------------------------------------------------------
#  object$method(...)

## ----create-hid1--------------------------------------------------------------
hid1 <- MarkovChain$new(data = energy, n_states = 2)

## ----create-obs1--------------------------------------------------------------
# List observation distribution(s)
dists <- list(Price = "norm")
# List of initial parameter values
par0_2s <- list(Price = list(mean = c(3, 6), sd = c(1, 1)))
# Create observation model object
obs1 <- Observation$new(data = energy, n_states = 2, dists = dists, par = par0_2s)

## ----create-hmm1--------------------------------------------------------------
hmm1 <- HMM$new(obs = obs1, hid = hid1)

hmm1

## ----fit-hmm1-----------------------------------------------------------------
hmm1$fit(silent = TRUE)

hmm1

## ----viterbi-hmm1, fig.width = 5, fig.height = 3, out.width="49%", fig.align = "center", fig.show = "hold"----
data_plot <- energy

# Coloured by most likely state sequence
data_plot$state <- factor(paste0("State ", hmm1$viterbi()))
ggplot(data_plot, aes(Day, Price, col = state)) +
  geom_point() +
  scale_color_manual(values = pal, name = NULL)

# Coloured by state probability
data_plot$pr_s2 <- hmm1$state_probs()[,2]
ggplot(data_plot, aes(Day, Price, col = pr_s2)) +
  geom_point() +
  scico::scale_color_scico(palette = "berlin", 
                           name = expression("Pr("~S[t]~"= 2)"))

## ----plot-dist-hmm1, fig.width = 6, fig.height = 4, out.width="80%", fig.align = "center"----
hmm1$plot_dist("Price")

## ----plot-hmm1, fig.width = 6, fig.height = 4, out.width="80%", fig.align = "center"----
hmm1$plot("obspar", "EurDol", i = "Price.mean") + 
  geom_point(aes(x = EurDol, y = Price, fill = state, col = state), 
             data = data_plot, alpha = 0.3)

## ----pseudores-hmm1, fig.width = 5, fig.height = 4, out.width="49%", fig.align = "center", fig.show = "hold"----
# Get pseudo-residuals in a matrix (one row for each response variable)
pr <- hmm1$pseudores()

# Check normality; should be along 1:1 diagonal
qqnorm(pr["Price",])
abline(0, 1)

# Check independence; should decay to zero
acf(pr["Price",])

## ----check-hmm1, fig.width = 6, fig.height = 4, out.width="100%", fig.align = "center"----
# This function returns the statistics that we want to compare,
# in a named vector
gof <- function(data) {
  s <- c(quantile(data$Price, seq(0, 1, by = 0.25)),
         autocor = cor(data$Price[-1], data$Price[-nrow(data)]))
}

# Run posterior predictive checks
checks <- hmm1$check(check_fn = gof, silent = TRUE, nsims = 500)

# Plot histograms of simulated statistics
checks$plot

## ----hmm2, fig.width = 4, fig.height = 3, out.width="49%", fig.align = "center", fig.show = "hold"----
# Hidden state process
hid2 <- MarkovChain$new(data = energy, n_states = 3)

# Observation model
par0_3s <- list(Price = list(mean = c(2.5, 4, 6), sd = c(0.5, 0.5, 1)))
obs2 <- Observation$new(data = energy, n_states = 3, 
                        dists = dists, par = par0_3s)

# Create and fit HMM
hmm2 <- HMM$new(obs = obs2, hid = hid2)
hmm2$fit(silent = TRUE)

# Show parameters
hmm2

# Plot state-dependent distributions
hmm2$plot_dist("Price")

# Time series plot coloured by most likely state sequence
hmm2$plot_ts("Price")

# Plot prices against euro-dollar exchange rate, 
# with estimated state-specific means
data_plot$state <- factor(paste0("State ", hmm2$viterbi()))
hmm2$plot("obspar", "EurDol", i = "Price.mean") + 
  geom_point(aes(x = EurDol, y = Price, fill = state, col = state), 
             data = data_plot, alpha = 0.3)

## ----hmm3---------------------------------------------------------------------
# State process model
f <- ~ Oil
hid3 <- MarkovChain$new(data = energy, n_states = 2, formula = f)

# Observation model
obs3 <- Observation$new(data = energy, n_states = 2, 
                        dists = dists, par = par0_2s)

# Fit HMM
hmm3 <- HMM$new(obs = obs3, hid = hid3)
hmm3$fit(silent = TRUE)

## ----print-hmm3---------------------------------------------------------------
hmm3$coeff_fe()

## ----confint-hmm3-------------------------------------------------------------
round(hmm3$confint(level = 0.95)$coeff_fe$hid, 2)

## ----plot-hmm3, fig.width = 4, fig.height = 3, out.width="49%", fig.align = "center", fig.show = 'hold'----
# Plot transition probabilities as functions of oil price
hmm3$plot("tpm", var = "Oil")

# Plot stationary state probabilities as functions of oil price
hmm3$plot("delta", var = "Oil")

## ----hmm4---------------------------------------------------------------------
# State process
hid4 <- MarkovChain$new(data = energy, n_states = 2)

# Observation model
f <- list(Price = list(mean = ~ EurDol))
obs4 <- Observation$new(data = energy, n_states = 2, dists = dists, 
                        par = par0_2s, formulas = f)

# Fit HMM
hmm4 <- HMM$new(obs = obs4, hid = hid4)
hmm4$fit(silent = TRUE)

# Look at regression coefficients
hmm4$coeff_fe()

## ----plot-hmm4, fig.width = 6, fig.height = 4, out.width="80%", fig.align = "center"----
data_plot$state <- factor(paste0("State ", hmm4$viterbi()))
hmm4$plot("obspar", "EurDol", i = "Price.mean") + 
  geom_point(aes(x = EurDol, y = Price, fill = state, col = state), 
             data = data_plot, alpha = 0.3)

## ----hmm5, fig.width = 6, fig.height = 4, out.width="80%", fig.align = "center"----
hid5 <- MarkovChain$new(data = energy, n_states = 2)
f <- list(Price = list(mean = ~s(EurDol, k = 10, bs = "cs")))
obs5 <- Observation$new(data = energy, n_states = 2, dists = dists,
                        par = par0_2s, formulas = f)
hmm5 <- HMM$new(obs = obs5, hid = hid5)

hmm5$fit(silent = TRUE)

data_plot$state <- factor(paste0("State ", hmm5$viterbi()))
hmm5$plot("obspar", "EurDol", i = "Price.mean") +
  geom_point(aes(x = EurDol, y = Price, fill = state, col = state),
             data = data_plot, alpha = 0.3)

## ----plot-ts-hmm4-hmm5, fig.width = 5, fig.height = 4, out.width="49%", fig.align = "center", fig.show = "hold"----
hmm4$plot_ts("Price")
hmm5$plot_ts("Price")

## ----re-prep------------------------------------------------------------------
energy$Year <- factor(format(energy$Day, "%Y"))
head(energy$Year)

## ----re-fit-------------------------------------------------------------------
# Formula for RE of year on mean price
f <- list(Price = list(mean = ~ s(Year, bs = "re")))
# Create Observation object with RE formula
obs6 <- Observation$new(data = energy, n_states = 2,
                        dists = dists, par = par0_2s, 
                        formula = f)

# Create hidden state process
hid6 <- MarkovChain$new(data = energy, n_states = 2)
# Create and fit HMM
hmm6 <- HMM$new(obs = obs6, hid = hid6)
hmm6$fit(silent = TRUE)

## ----re-par-------------------------------------------------------------------
# Fixed effect coefficients
round(obs6$coeff_fe(), 2)

# Std dev of random effects
round(obs6$sd_re(), 2)

## ----re-par2------------------------------------------------------------------
# Predicted random intercepts
round(obs6$coeff_re(), 2)

## ----re-plot, fig.width = 6, fig.height = 4, out.width="80%", fig.align = "center"----
hmm6$plot("obspar", "Year", i = "Price.mean")

## ----pred-hmm3----------------------------------------------------------------
# Covariate data for predictions
newdata <- data.frame(Oil = c(20, 80))

# Predict transition probabilities
hmm3$predict(what = "tpm", newdata = newdata, n_post = 1e3)

# Predict stationary state probabilities
hmm3$predict(what = "delta", newdata = newdata, n_post = 1e3)

## ----sim-hmm1, fig.width = 6, fig.height = 4, out.width="80%", fig.align = "center"----
# Simulate a time series of same length as data
sim_data <- hmm3$simulate(n = nrow(energy), data = energy, silent = TRUE)
head(sim_data)

# Plot simulated prices
ggplot(sim_data, aes(Day, Price)) + geom_line()

