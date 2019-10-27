library(tidyverse)

## ## Practice Exercise #1

## 1. Replicate what we did above, adding two more (continuous) predictors, and using a data frame.  That is:
##     a. Create variables for all of the parameters needed to generate data from the linear model, including a slope parameter for each of three predictors, as well as the number of observations.  Choose different parameter values than those given above.

n_obs <- 100
beta_0 <- 1
beta_1 <- 2
beta_2 <- 4
beta_3 <- .5
sigma <- 1.5

##     b. Generate the predictor data.  For now, generate (theoretically) uncorrelated predictors by generating their samples separately (and independently), and putting them in a data frame.

df <- data.frame(x_1 <- as.numeric(scale(rnorm(n_obs))),
                 x_2 <- as.numeric(scale(rnorm(n_obs))),
                 x_3 <- as.numeric(scale(rnorm(n_obs))))

##     c. Generate the response data in the same data frame, using the linear model equation.

df$y <- beta_0 + (beta_1 * df$x_1) + (beta_2 * df$x_2) + (beta_3 * df$x_3) + rnorm(n_obs, 0, sigma)

##     d. Fit a model using `lm()`, and pull out the coefficients from the fitted model, to compare with your "ground truth" values.

fit <- lm(y ~ x_1 + x_2 + x_3, data = df)
summary(fit)
fit$coef
sd(fit$resid)

## 2. Convert your code from #1 into a function, so that you can pass the parameter values as arguments and get back the estimated parameter values in a list or data frame with appropriate names.

sim_multiple_lm <- function(n_obs, beta_0, beta_1, beta_2, beta_3, sigma) {
    # generate predictors
    df <- data.frame(x_1 <- as.numeric(scale(rnorm(n_obs))),
                     x_2 <- as.numeric(scale(rnorm(n_obs))),
                     x_3 <- as.numeric(scale(rnorm(n_obs))))

    # generate response
    df$y <- beta_0 + (beta_1 * df$x_1) + (beta_2 * df$x_2) + (beta_3 * df$x_3) + rnorm(n_obs, 0, sigma)

    # fit model
    fit <- lm(y ~ x_1 + x_2 + x_3, data = df)

    # format output
    output <- data.frame(beta_0, beta_0_hat = NA,
                         beta_1, beta_1_hat = NA,
                         beta_2, beta_2_hat = NA,
                         beta_3, beta_3_hat = NA,
                         sigma, sigma_hat = NA)
    output[, c("beta_0", "beta_1", "beta_2", "beta_3")] <- c(beta_0, beta_1, beta_2, beta_3)
    output[, c("beta_0_hat", "beta_1_hat", "beta_2_hat", "beta_3_hat")] <- fit$coef
    output$sigma_hat <- sd(fit$resid)
    output
}

## 3. Use your new function to explore a few different sets of parameter values.  Play around with scenarios where you might not expect the model to get the parameters exactly right, like low $N$ or high error variance.

sim_multiple_lm(n_obs = 100,
                beta_0 = 0,
                beta_1 = 2,
                beta_2 = 4,
                beta_3 = .2,
                sigma  = 1)
sim_multiple_lm(100, 0, 2, 4, .2, 5)
sim_multiple_lm(1000, 80, 1, .1, .01, 1)
sim_multiple_lm(10, 0, 2, 4, .2, 1)
sim_multiple_lm(10, 0, 2, 4, .2, 10)

########################################################################
########################################################################
## ## Practice Exercise #2

sim_simple_lm <- function(n_obs, beta_0, beta_1, sigma) {
  x_1 <- as.numeric(scale(rnorm(n_obs)))
  errors <- rnorm(n_obs, 0, sigma)
  y <- beta_0 + beta_1 * x_1 + errors
  fit <- lm(y ~ x_1)
  output <- list(n_obs = n_obs,
                 beta_0 = beta_0,
                 beta_0_hat = fit$coef["(Intercept)"],
                 beta_1 = beta_1,
                 beta_1_hat = fit$coef["x_1"],
                 sigma = sigma,
                 sigma_hat = sd(fit$resid))
  return(output)
}

n_sims <- 1e4 # scientific notatation, for legibility
sim_results <- data.frame(sim = 1:n_sims,
                          beta_0_hat = NA,
                          beta_1_hat = NA,
                          sigma_hat = NA)
for(this_sim in 1:n_sims) {
  if(this_sim %% 1000 == 0) { cat("starting simulation", this_sim, "\n") }
  this_fit <- sim_simple_lm(100, 1, 2, 10)
  sim_results[this_sim, 2:4] <- c(this_fit$beta_0_hat,
                                  this_fit$beta_1_hat,
                                  this_fit$sigma_hat)
}
summary(sim_results[, 2:4])

## 1. Practice your data wrangling by plotting histograms of the parameter values in `sim_results`.  That is:
##     a. Reshape the data so that all of the parameter values are in a single column, with a new column to distinguish the different parameters (`beta_0_hat`, etc.).

sim_results_long <- sim_results %>%
    pivot_longer(cols = 2:4,
                 names_to = "parameter",
                 values_to = "value")
head(sim_results_long)

##     b. Use `ggplot` to plot histograms of the parameters, using `facet_wrap(scales = "free")` to separate the parameters into different sub-plots.

ggplot(sim_results_long, aes(value)) + geom_histogram() +
    facet_wrap(~ parameter, scales = "free")

##     c. Play around with `binwidth` values.

ggplot(sim_results_long, aes(value)) + geom_histogram(binwidth = 1) +
    facet_wrap(~ parameter, scales = "free")

ggplot(sim_results_long, aes(value)) + geom_histogram(binwidth = .1) +
    facet_wrap(~ parameter, scales = "free")

ggplot(sim_results_long, aes(value)) + geom_histogram(binwidth = .01) +
    facet_wrap(~ parameter, scales = "free")

##     d. Play around with colors and/or themes.

ggplot(sim_results_long, aes(value)) +
    geom_histogram(aes(fill = parameter), binwidth = .1) +
    facet_wrap(~ parameter, scales = "free") +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal() +
    theme(legend.position = "none")

## 2. Take the loop code chunk above and convert the whole thing to a function, also including your plotting code.  The function should:
##     - take the model parameters plus the number of simulations as arguments
##     - should print out the summary of the simulated parameters as a side effect
##     - should generate your plot from #1 as a side effect
##     - should return the full data frame of `sim_results` as the return value

manysims_simple_lm <- function(n_sims, n_obs, beta_0, beta_1, sigma) {

    sim_results <- data.frame(sim = 1:n_sims,
                              beta_0_hat = NA,
                              beta_1_hat = NA,
                              sigma_hat = NA)
    for(this_sim in 1:n_sims) {
        if(this_sim %% 1000 == 0) { cat("starting simulation", this_sim, "\n") }
        this_fit <- sim_simple_lm(n_obs, beta_0, beta_1, sigma)
        sim_results[this_sim, 2:4] <- c(this_fit$beta_0_hat,
                                        this_fit$beta_1_hat,
                                        this_fit$sigma_hat)
    }
    cat("\nSummary of simulated parameter estimates:\n\n")
    print(summary(sim_results[, 2:4]))
    sim_results_long <- sim_results %>%
        pivot_longer(cols = 2:4,
                     names_to = "parameter",
                     values_to = "value")
    print(ggplot(sim_results_long, aes(value)) +
          geom_histogram(aes(fill = parameter), binwidth = .1) +
          facet_wrap(~ parameter, scales = "free") +
          scale_fill_brewer(palette = "Set1") +
          theme_minimal() +
          theme(legend.position = "none"))

    return(sim_results)
}

## 3. Use your spiffy new function from #2 and play around with some different parameter values (and number of observations), and look at the impact on the distributions of parameter values. For example, look at how the shape of $\sigma$ distributions change with the number of observations.

simresults_1 <- manysims_simple_lm(10000, 100, 5, 2, 3)
windows() # change this to quartz() if you're on Mac, x11() if you're on Linux
simresults_2 <- manysims_simple_lm(10000, 10, 5, 2, 3)
windows() # change this to quartz() if you're on Mac, x11() if you're on Linux
simresults_3 <- manysims_simple_lm(10000, 5, 5, 2, 3)

################################################################################
################################################################################
## ## Practice Exercise #3

## 1. Using the `sleep` data, create a new variable corresponding to a centered version of the `group` variable, calling it `group.c`.  Hint: how can you transform the `group` variable so that its mean value is 0?

mysleep <- sleep
mysleep$group.c <- as.numeric(mysleep$group) - 1.5
xtabs(~ group + group.c, mysleep)

## 2. Fit a model with the new `group.c` predictor as the only predictor of `extra`.  How is it different and similar to the original model with `group` or `group.num`?

summary(lm(extra ~ group, mysleep))
summary(lm(extra ~ group.c, mysleep))

## 3. What does the new intercept correspond to?  Calculate that value with another method to demonstrate this interpretation of the coefficient.

mysleep %>% summarize(grand.mean = mean(extra))

#############################################################################
#############################################################################
## ## Practice Exercise #4

## 1. Create a local copy of the `iris` data.

myiris <- iris

## 2. Fit a model predicting `Sepal.Width` using the `Species` factor.

fit1 <- lm(Sepal.Width ~ Species, data = myiris)

## 3. Create dummy variables to re-create the implicit coding from the model in #2, and re-fit the model using those new variables instead of `Species`.  Confirm that the coefficient estimates are identical to those in #2.

myiris <- myiris %>% mutate(versicolor.var = ifelse(Species %in% "versicolor", 1, 0),
                            virginica.var = ifelse(Species %in% "virginica", 1, 0))

fit2 <- lm(Sepal.Width ~ versicolor.var + virginica.var, data = myiris)

summary(fit1)
summary(fit2)

## 4. Use `mutate()` and `relevel()` to create another version of the `Species` factor, where the value of "versicolor" is the reference level.

myiris <- myiris %>% mutate(Species2 = relevel(Species, "versicolor"))

## 5. Re-fit the model from #2 using the new factor from #4.  Explain the differences between coefficients.

fit3 <- lm(Sepal.Width ~ Species2, data = myiris)
summary(fit3)

## The intercept of 2.77 is the mean for "versicolor", and is the same as the original
## intercept of 3.43 plus the original coef for "versicolor" of -0.66.  The new "setosa"
## coefficient is just the sign-flipped version of the original "versicolor" coefficient
## because they represent the same difference, just in the opposite direction.  The new
## "virginica" coefficient is the difference between the old "versicolor" and "virginica"
## coefficients.

## 6. Adding only a single new dummy variable, re-fit the same model as #5, using dummy-coded variables instead.  Confirm that the coefficient estimates are identical.

myiris <- myiris %>% mutate(setosa.var = ifelse(Species %in% "setosa", 1, 0))

fit4 <- lm(Sepal.Width ~ setosa.var + virginica.var, data = myiris)
summary(fit4)


#######################################################################################
#######################################################################################
## ## Practice Exercise #5

## 1. Let's simulate some data similar in structure to the `iris` data, focusing on the `Sepal.Width` model from earlier.  Make another fresh local copy of the data.

myiris <- iris

## 2. Fit model with `Species` predicting `Sepal.Width`, and take note of the model parameters, out to two or three decimal places.

fit5 <- lm(Sepal.Width ~ Species, data = myiris)
summary(fit5)

n_obs <- 150
beta_setosa_int <- 3.43
beta_versicolor <- -0.66
beta_virginica <- -0.45
sigma_iris <- 0.34

## 3. Create a data frame consisting of a `plantID` column that goes from "plant1" to "plant150", and two columns that represent `versicolor` and `virginica` dummy-coded factors.  Try two different methods.  With both methods, use a function like `xtabs()` or `group_by()`/`summarize()` 

##     a. use `rep()` to generate vectors of 0s and 1s

sim_iris1 <- data.frame(plantID = paste0("plant", 1:150),
                        versicolor = rep(c(0, 1, 0), each = 50),
                        virginica = rep(c(0, 0, 1), each = 50))
xtabs(~ versicolor + virginica, sim_iris1)
head(sim_iris1)

##     b. use `contr.treatment()` and take advantage of "recycling"

sim_iris2 <- data.frame(plantID = paste0("plant", 1:150),
                        versicolor = NA,
                        virginica = NA)
sim_iris2[, 2:3] <- as.data.frame(contr.treatment(3))
xtabs(~ versicolor + virginica, sim_iris2)
head(sim_iris2)

## 4. Adapt one of your previous functions to generate data from linear model parameters (specifically, a version with two slope parameters). Using the parameters from the model you fit in #2, generate data.

simdata_2pred_lm <- function(n_obs, beta_0, beta_1, beta_2, sigma, pred_names, response_name) {
    df <- data.frame(x_1 <- rep(c(0, 1, 0), each = n_obs/3),
                     x_2 <- rep(c(0, 0, 1), each = n_obs/3))
    errors <- rnorm(n_obs, 0, sigma)
    df$y <- beta_0 + beta_1 * df$x_1 + beta_2 * df$x_2 + errors
    colnames(df) <- c(pred_names, response_name)
    return(df)
}

replicated_iris <- simdata_2pred_lm(150, beta_setosa_int, beta_versicolor, beta_virginica, sigma_iris,
                                    c("versicolor", "virginica"), "Sepal.Width")

replicated_iris <- replicated_iris %>% mutate(plantID = paste0("simplant", 1:150))

## 5. Fit a model to this simulated data and compare to the original fitted model.

fit_simiris <- lm(Sepal.Width ~ versicolor + virginica, replicated_iris)
summary(fit_simiris)
summary(lm(Sepal.Width ~ Species, myiris))

## 6. Compare plots of the simulated and original data (using boxplots).  Hint: try to put both into a single data frame, and try to plot the corresponding boxes next to each other

real_iris <- myiris %>% mutate(plantID = paste0("plant", 1:150),
                               versicolor = ifelse(Species %in% "versicolor", 1, 0),
                               virginica = ifelse(Species %in% "virginica", 1, 0)) %>%
    select(plantID, Sepal.Width, versicolor, virginica)
head(real_iris)
head(replicated_iris)

realsim_iris <- bind_rows(real = real_iris, replicated = replicated_iris, .id = "data_set")
head(realsim_iris)
realsim_iris <- realsim_iris %>% mutate(Species = ifelse(versicolor, "versicolor",
                                                         ifelse(virginica, "virginica", "setosa")))

ggplot(realsim_iris, aes(Species, Sepal.Width)) + geom_boxplot(aes(fill = data_set)) +
    scale_fill_manual(values = c("steelblue", "steelblue1")) +
    theme_minimal()




