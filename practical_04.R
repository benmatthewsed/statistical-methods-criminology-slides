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


simd <- 
simd |> 
  mutate(city_la = if_else(str_detect(council_area, "City"),
                           1,
                           0))


qp_mod <- 
glm(crime_integer_sim ~ simd2020_rank * city_la,
    family = "quasipoisson",
    data = simd)


# see https://www.r-bloggers.com/2012/08/generate-quasi-poisson-distribution-random-variable/

rqpois <- function(n, mu, theta) {
  rnbinom(n = n, mu = mu, size = mu/(theta-1))
}

qp_summary <- 
qp_mod |> 
  summary()

qp_summary$dispersion

n_sims <- 10

draws <- 
MASS::mvrnorm(
  n = n_sims,
  mu = coef(qp_mod),
  Sigma = vcov(qp_mod)
)

pred_dat <- 
simd |> 
  distinct(council_area) |> 
  mutate(simd2020_rank = 6976 / 2)




predict(qp_mod,
        newdata = pred_dat)


data.frame(fitted = 
             predict(qp_mod,
                     newdata = pred_dat)) |> 
  mutate(pred_count = map_dbl(fitted, ~ rqpois(n = 1, mu = .x, theta = 31.14463)))


pred_mean_crime_draws <- function(fit, ...){
  
  fits <- 
  data.frame(fitted = 
               predict(fit,
        newdata = pred_dat))
  
  fits |> 
    mutate(pred_count = map_dbl(fitted, ~ rqpois(n = 1, mu = .x, theta = qp_summary$dispersion))) |> 
    pull(pred_count)
    
  
}


pred_mean_crime_draws(qp_mod)  
  
pred_mean_crime_resp <- function(fit, ...){
  
  predict(fit,
          newdata = pred_dat,
          type = "response"
  )
  
}

pred_30_or_more_crime_resp <- function(fit, ...){
  
  data.frame(
    fitted =  predict(fit,
          type = "response"
  )) |> 
    mutate(thirty_or_more = if_else(fitted >= 30, 1, 0)) |> 
    pull(thirty_or_more)
  
}


sims <- clarify::sim(fit = qp_mod,
                     coefs = coef(qp_mod),
                     vcov = vcov(qp_mod),
                     n = 10)

res <- clarify::sim_apply(sim = sims, FUN = pred_30_or_more_crime_resp)

plot(res)

summary(res, method = "quantile")

res |> 
  as.data.frame() |> 
  as_tibble() |> 
  t() |> 
  as_tibble() |> 
  summarise(mean_V1 = mean(V1),
            mean_V2 = mean(V2))


# probability of 5 or more given SIMD?

clarify::sim_setx(sims, x = list(simd2020_rank = seq(1:6976), city_la = c(1, 0)),
         verbose = FALSE) |> plot()

ames <- clarify::sim_ame(sims, var = "city_la", by = ~simd2020_rank,
                 contrast = "diff")


plot(ames)

ames2 <- clarify::sim_ame(sims, var = "simd2020_rank", by = ~city_la,
                         contrast = "diff")

plot(ames2)

predict(
  qp_mod,
  newdata = data.frame(council_area = c("Shetland Islands", "City of Edinburgh"),
                       simd2020_rank = rep(6976 / 2, 2))
)
