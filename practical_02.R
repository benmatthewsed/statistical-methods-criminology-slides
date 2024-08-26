library(tidyverse)
library(broom)

# see pages 58-63 at https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/09/simd-2020-technical-notes/documents/simd-2020-technical-notes/simd-2020-technical-notes/govscot%3Adocument/SIMD%2B2020%2Btechnical%2Bnotes.pdf

# because counts smaller than three were suppressed due to disclosure control, i have simulated crime counts
# for datazones with fewer than three crimes, allocating them 0, 1 or 2 crimes with equal probability

# i also created a simulated violent crime variable by multiplying the total crime count for the
# datazone by 0.24 (as non-sexual crimes of violence make up 24% of recorded crime
# see # from https://www.gov.scot/publications/recorded-crime-scotland-2023-24/pages/4/)

# these variables are just intended to illustrate the methods discussed in this course - they won't tell us
# anything informative about crime in Scotland (unless by accident)

# all other data comes from the Scottish Index of Multiple Deprivation ranks and indicators


# read in the data --------------------------------------------------------

simd <- readRDS(url("https://github.com/benmatthewsed/statistical-methods-criminology-slides/raw/master/resources/simd_crime_sim.rds"))


# descriptives ------------------------------------------------------------

# our key outcome is crime_integer_sim

simd |> 
  ggplot(aes(x = crime_integer_sim)) +
  geom_histogram(binwidth = 1)

# very skewed!
# the average crime count is just over 22

# but there is a lot of variability around this (sd of 32), and no datazones with less than zero crimes
# ... which makes sense!

simd |> 
  summarise(mean_crime = mean(crime_integer_sim),
            sd_crime = sd(crime_integer_sim),
            min_crime = min(crime_integer_sim),
            max_crime = max(crime_integer_sim))

# lets do the same for violent crime

simd |> 
  summarise(mean_vio = mean(vio_integer_sim),
            sd_vio = sd(vio_integer_sim),
            min_vio = min(vio_integer_sim),
            max_vio = max(vio_integer_sim))

# we will use overall SIMD (simd2020v2_rank) as our main independent variable
# this ranks all 6976 datazones from most (1) to least (6976) deprived
# so it is uniformly distributed with a mean halfway from 1 to 6976

simd |> 
  summarise(mean_simd = mean(simd2020_rank),
            sd_simd = sd(simd2020_rank),
            min_simd = min(simd2020_rank),
            max_simd = max(simd2020_rank))





# regression modelling ----------------------------------------------------

# we will compare linear and count models for overall crime and violence

# linear model

lm(crime_integer_sim ~ 1, data = simd) |> 
  broom::tidy()

# the intercept is the same as the mean crime count we calculated manually - that's good!

glm(crime_integer_sim ~ 1, 
    family = "poisson",
    data = simd) |> 
  broom::tidy()

# the estimate is 3.10. we need to exp() this to get the estimate on the outcome scale
# this is because the poisson model uses the log link function to link the coefficients
# to the observed data

glm(crime_integer_sim ~ 1, 
    family = "poisson",
    data = simd) |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate))

# exp_est is 22.3 - same as that we calculated already

# now let's try a quasipoisson

glm(crime_integer_sim ~ 1, 
    family = "quasipoisson",
    data = simd) |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate))

# the results are the same! but note the std.error column - this is now
# much larger (0.0176) than in the previous model (0.00254)
# this reflects the quasi poisson correcting for over-dispersion


# crime and SIMD ----------------------------------------------------------

# let's look at the relationship between crime and SIMD

mod_crime_simd_lm <- lm(crime_integer_sim ~ simd2020_rank, data = simd)

mod_crime_simd_lm |> 
  broom::tidy()

# SIMD is negative and statistically significant - with less deprivation (i.e. as SIMD increases)
# we would expect less crime

mod_crime_simd_p <- glm(crime_integer_sim ~ simd2020_rank, family = "poisson", data = simd)

mod_crime_simd_p |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate))

# the poisson model tells us the same, although the exp_est is rounded to 1.00
# if we calculate this directly we see it is 0.9997

exp(-0.000271)

# for every increase in SIMD you have 99.97% as much crime, or if you like, a 
# 1 - exp(-0.000271) - 0.027% increase in crime


# we can visualize the results:


simd |> 
  mutate(lm_pred = predict(mod_crime_simd_lm), # calculate the predicted values from the lm model
         p_pred = predict(mod_crime_simd_p, type = "response")) |> # ... and from the poisson model
  ggplot(aes(x = simd2020_rank, y = crime_integer_sim)) + # plot the raw data
  geom_point(alpha = 0.1) + # set transparency
  geom_line(aes(y = lm_pred), colour = "red") +
  geom_line(aes(y = p_pred), colour = "blue")

# the lines are similar but diverge at the extremes
# this is because the lm implies linear change, but the poisson model implies multiplicative change



# modelling violence ------------------------------------------------------

mod_vio_simd_lm <- lm(vio_integer_sim ~ simd2020_rank, 
                      data = simd)

mod_vio_simd_p <- glm(vio_integer_sim ~ simd2020_rank, 
                      family = "poisson",
                      data = simd)

mod_vio_simd_lm |> 
  broom::tidy() |> 
  mutate(conf_low = estimate - 1.96 * std.error, # we can calculate confidence intevals for these coefficients
         conf_upp = estimate + 1.96 * std.error)

# as before, simd is negatively and statistically significantely related to our crime indicator

# let's see this in the poisson model

mod_vio_simd_p |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate),
         conf_low = estimate - 1.96 * std.error,
         conf_upp = estimate + 1.96 * std.error,
         exp_conf_low = exp(conf_low),
         exp_conf_upp = exp(conf_upp))


# we might want to look at a quasipoisson model in case the the standard errors in the poisson
# are too small

mod_vio_simd_qp <- glm(vio_integer_sim ~ simd2020_rank, 
                      family = "quasipoisson",
                      data = simd)

mod_vio_simd_qp |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate),
         conf_low = estimate - 1.96 * std.error,
         conf_upp = estimate + 1.96 * std.error,
         exp_conf_low = exp(conf_low),
         exp_conf_upp = exp(conf_upp))

# the standard error is much larger - this might indicate that we should be worried about over-dispersion


# let's visualize the results again

simd |> 
  mutate(lm_pred = predict(mod_vio_simd_lm), # calculate the predicted values from the lm model
         p_pred = predict(mod_vio_simd_p, type = "response"),
         qp_pred = predict(mod_vio_simd_qp, type = "response"),) |> # ... and from the poisson model
  ggplot(aes(x = simd2020_rank, y = vio_integer_sim)) + # plot the raw data
  geom_point(alpha = 0.1) + # set transparency
  geom_line(aes(y = lm_pred), colour = "red") +
  geom_line(aes(y = p_pred), colour = "blue") +
  geom_line(aes(y = qp_pred), colour = "pink") 


# if we look closely at the models' predictions in low-deprivation areas, we can see something funny with lm


predict(mod_vio_simd_lm,
        newdata = data.frame(simd2020_rank = 6976), # this just gives a prediction for the least deprived datazone
        se.fit = TRUE) |> 
  as.data.frame() |> 
  mutate(conf_low = fit - 1.96 * se.fit, # approximating the 95% confidence interval as 1.96 * the standard error
         conf_upp = fit + 1.96 * se.fit)

# the model thinks that there will be 0.07 violent crimes in the least deprived neighbourhood,
# with a lower confidence intervals of -0.27 crimes!

# if we look at the poisson model instead

predict(mod_vio_simd_p,
        newdata = data.frame(simd2020_rank = 6976), # this just gives a prediction for the least deprived datazone
        se.fit = TRUE) |> 
  as.data.frame() |> 
  mutate(conf_low = fit - 1.96 * se.fit,
         conf_upp = fit + 1.96 * se.fit,
         exp_est = exp(fit),
         exp_conf_low = exp(conf_low),
         exp_conf_upp = exp(conf_upp))

# the model predicts 1.4 crimes, with a lower confidence interval of 1.37 crimes

# we can do the same for the quasipoisson model too

predict(mod_vio_simd_qp,
        newdata = data.frame(simd2020_rank = 6976), # this just gives a prediction for the least deprived datazone
        se.fit = TRUE) |> 
  as.data.frame() |> 
  mutate(conf_low = fit - 1.96 * se.fit,
         conf_upp = fit + 1.96 * se.fit,
         exp_est = exp(fit),
         exp_conf_low = exp(conf_low),
         exp_conf_upp = exp(conf_upp))

# the main estimate is unchanged, but the larger standard error means our confidence intervals are wider - but still positive


# over to you -------------------------------------------------------------

# using either the models above, or by using the additional variables in the SIMD data to fit your own model,
# write a summary of what this analysis can and can't tell us about the relationship between deprivation
# and crime in Scotland

# what are the pros and cons of the different modelling approaches?
# what are the limitations of the data used?



