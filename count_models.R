library(readxl)
library(tidyverse)

url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-indicator-data/documents/simd_2020_indicators/simd_2020_indicators/govscot%3Adocument/SIMD%2B2020v2%2B-%2Bindicators.xlsx"

temp_dir <- tempdir()

download.file(url = url,
              destfile = file.path(temp_dir, "simd_indicators.xlsx"),
              mode = "wb")


url_ranks <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/documents/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/govscot%3Adocument/SIMD%2B2020v2%2B-%2Branks.xlsx"

download.file(url = url_ranks,
              destfile = file.path(temp_dir, "simd_ranks.xlsx"),
              mode = "wb")

simd <- 
read_excel(
  file.path(temp_dir, "simd_indicators.xlsx"),
  sheet = 3
) |> 
  mutate(crime_count = if_else(crime_count == "*",
                               NA,
                               as.double(crime_count)))


simd_ranks <- 
  read_excel(
    file.path(temp_dir, "simd_ranks.xlsx"),
    sheet = 2
  ) |> 
  select(Data_Zone, SIMD2020v2_Rank)

simd <- left_join(simd, simd_ranks)

simd |> 
  ggplot(aes(x = crime_count)) +
  geom_histogram(binwidth = 1)

simd <- 
simd |> 
  mutate(crime_integer = as.integer(crime_count))



simd |> 
  mutate(crime_capped = if_else(crime_count > 10,
                                10,
                                as.integer(crime_count))) |> 
  count(crime_capped)

# we are missing counts under 3!

# draw probabilities 0, 1, and 2

simd <- 
simd |> 
  mutate(crime_integer_sim = map_dbl(crime_integer, ~ if_else(
    is.na(.x),
    sample(c(0, 1, 2),
           size = 1,
           prob = c(0.3, 0.3, 0.3)),
    .x
  ))) |> 
  mutate(crime_capped_sim = if_else(crime_integer_sim > 10,
                                10,
                                as.integer(crime_integer_sim)))


# let's simulate!

model <- 
mgcv::gam(crime_integer ~ s(SIMD2020v2_Rank),
          family = mgcv::nb(),
          data = simd)

pred_df <- tibble(
  SIMD2020v2_Rank = seq(1:6976)
)



plot(model)

model_res <- broom::augment(model)

full_pred <- 
predict(model,
        newdata = pred_df) |> 
  as_tibble() |> 
  mutate(SIMD2020v2_Rank = seq(1:6976))


full_res <- left_join(full_pred, model_res)

full_res |> 
  filter(is.na(.fitted))

read_excel(
  file.path(temp_dir, "simd_indicators.xlsx"),
  sheet = 3
) |> 
  select(crime_rate) |> 
  mutate(crime_rate = if_else(crime_rate == "*",
                               NA,
                               as.double(crime_rate))) |> 
  filter(is.na(crime_rate))
  


# fit with lm, pois, quasipoisson and nb

lm(crime_integer_sim ~ SIMD2020v2_Rank, data = simd) |> 
  broom::tidy()

simd |> 
  ggplot(aes(x = SIMD2020v2_Rank, y = crime_integer_sim)) +
  geom_point() +
  geom_smooth(method = "lm")

glm(crime_integer_sim ~ SIMD2020v2_Rank, 
    family = "poisson",
    data = simd) |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate))


glm(crime_integer_sim ~ SIMD2020v2_Rank, 
    family = "quasipoisson",
    data = simd) |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate))

MASS::glm.nb(crime_integer_sim ~ SIMD2020v2_Rank,
    data = simd) |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate))

simd <- 
simd |> 
  mutate(simd_scale = scale(SIMD2020v2_Rank)[,1])

nb_mod <- 
MASS::glm.nb(crime_integer_sim ~ simd_scale,
               data = simd)


simd |> View()

nb_mod |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate))

glm(crime_integer_sim ~ simd_scale,
    family = "poisson",
             data = simd) |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate))

qp_mod <- glm(crime_integer_sim ~ simd_scale,
    family = "quasipoisson",
    data = simd)

qp_mod |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate))

lm_mod <- 
lm(crime_integer_sim ~ simd_scale,
    data = simd)

simd |> 
  arrange(desc(simd_scale)) |> 
  select(simd_scale)

predict(qp_mod,
        newdata = tibble(simd_scale = 1.73),
        se.fit = TRUE)

p_mod <- glm(crime_integer_sim ~ simd_scale,
              family = "poisson",
              data = simd)

predict(p_mod,
        newdata = tibble(simd_scale = 1.73),
        se.fit = TRUE)


predict(nb_mod,
        newdata = tibble(simd_scale = 1.73),
        se.fit = TRUE)



# questions

# show how all models recover the mean of y?


# what are the SEs for the poisson and quasipoisson?
# what are the model fit statistics for poisson and quasipoisson?

AIC(p_mod)
AIC(qp_mod)
AIC(nb_mod)

summary(qp_mod)
summary(nb_mod)
summary(p_mod)


coef(qp_mod)

# what story is this model telling about deprivation and crime?

# exponentiate the coefficients?

p_mod |> 
  broom::tidy() |> 
  mutate(exp_est = exp(estimate))

# for a one-standard deviation increase in scaled SIMD (one standard deviation decline in SIMD)
# crime would be 57.9% of what it would otherwise be (or it would fall by 1 - 0.579  around 42%)

p_mod_la <- glm(crime_integer_sim ~ simd_scale + Council_area,
             family = "poisson",
             data = simd)

summary(p_mod_la)

qp_mod_la <- glm(crime_integer_sim ~ simd_scale + Council_area,
                family = "quasipoisson",
                data = simd)

summary(qp_mod_la)



# get them to play around and add variables in
# suggest some variables to add

# council area

# now add in some other variables and see what happens



qp_mod_la_int <- glm(crime_integer_sim ~ simd_scale * Council_area,
                 family = "quasipoisson",
                 data = simd)


summary(qp_mod_la_int)

nb_mod_la <- MASS::glm.nb(crime_integer_sim ~ simd_scale + Council_area,
                 data = simd)

nb_mod_la_int <- MASS::glm.nb(crime_integer_sim ~ simd_scale * Council_area,
                          data = simd)



summary(nb_mod_la_int)


broom::tidy(nb_mod_la) |> 
  mutate(exp_est = exp(estimate))

broom::tidy(qp_mod_la) |> 
  mutate(exp_est = exp(estimate))


nb_mod_la |> 
  predict(type = "response") |> 
  mean()

qp_mod_la |> 
  predict(type = "response") |> 
  bind_cols(simd) |> 
  group_by(Council_area) |> 
  summarise(mean_pred = mean(...1))

qp_mod_la |> 
predict(type = "response") |> 
  bind_cols(simd) |> 
  mutate(pred_prev = if_else(...1 == 0, 0, 1),
         prev = if_else(crime_integer_sim == 0, 0, 1)) |> 
  group_by(Council_area) |> 
  summarise(mean_pred = mean(...1),
            mean_pred_prev = mean(pred_prev),
            prev = mean(prev, na.rm = TRUE),
            count = mean(crime_integer_sim)) |> 
  print(n = 32)


nb_mod_la_int |> 
  predict() |> 
  bind_cols(simd) |> 
  mutate(
    mod_fit = map_dbl(...1, ~ rnbinom(n = 1, mu = exp(.x), size = 1.8555)),
    pred_prev = if_else(mod_fit == 0, 0, 1),
         prev = if_else(crime_integer_sim == 0, 0, 1)) |> 
  group_by(Council_area) |> 
  summarise(mean_pred = mean(mod_fit),
            mean_pred_prev = mean(pred_prev),
            prev = mean(prev, na.rm = TRUE),
            count = mean(crime_integer_sim)) |> 
  print(n = 32)

qp_mod_la_int |> 
  predict() |> 
  bind_cols(simd) |> 
  mutate(
    mod_fit = map_dbl(...1, ~ rqpois(n = 1, mu = exp(.x), theta = 31.14993)),
    pred_prev = if_else(mod_fit == 0, 0, 1),
    prev = if_else(crime_integer_sim == 0, 0, 1)) |> 
  group_by(Council_area) |> 
  summarise(mean_pred = mean(mod_fit),
            mean_pred_prev = mean(pred_prev),
            prev = mean(prev, na.rm = TRUE),
            count = mean(crime_integer_sim)) |> 
  print(n = 32)


nb_mod_la |> 
  predict() |> 
  bind_cols(simd) |> 
  mutate(
    mod_fit = map_dbl(...1, ~ rqpois(n = 1, mu = exp(.x), theta = 37.93042)),
    pred_prev = if_else(mod_fit == 0, 0, 1),
    prev = if_else(crime_integer_sim == 0, 0, 1)) |> 
  group_by(Council_area) |> 
  summarise(mean_pred = mean(mod_fit),
            mean_pred_prev = mean(pred_prev),
            prev = mean(prev, na.rm = TRUE),
            count = mean(crime_integer_sim)) |> 
  print(n = 32)


simd |> 
  mutate(prev = if_else(crime_integer_sim == 0, 0, 1)) |> 
  group_by(Council_area) |> 
  summarise(prev = mean(prev, na.rm = TRUE),
            count = mean(crime_integer_sim))


# from https://www.gov.scot/publications/recorded-crime-scotland-2023-24/pages/4/

simd <- 
simd |> 
  mutate(vio_count_sim = as.integer(crime_integer_sim * 0.24))


lm_mod_vio <- lm(vio_count_sim ~ simd_scale,
                     data = simd)

lm_mod_vio |> 
  predict(newdata = tibble(simd_scale = 1.76))


# so just do

# model overall crime
# lm, poisson, quasipoisson

# model violent crime
# lm, poisson, quasipoisson

# point is, that we want to see the negative predictions

# add some variables in

# add in council area

# look at coefficients

# point is - we want to see how inferences change


# advanced

# do negative binomial too?

zip_mod_la_int <- pscl::zeroinfl(formula = crime_integer_sim ~ simd_scale + Council_area |
                                  simd_scale * Council_area,
                     data = simd)

summary(zip_mod_la_int)

AIC(zip_mod_la_int)


simd |> 
  mutate(zero_p = predict(zip_mod_la_int, type = "zero"),
         count_mean = predict(zip_mod_la_int, type = "count"),
         mod_fit = map2_dbl(zero_p, count_mean, ~ ifelse(rbinom(1, size = 1, prob = .x) > 0, 0,
                                                         rpois(1, lambda = count_mean))),
    pred_prev = if_else(mod_fit == 0, 0, 1),
    prev = if_else(crime_integer_sim == 0, 0, 1)) |> 
  group_by(Council_area) |> 
  summarise(mean_pred = mean(mod_fit),
            mean_pred_prev = mean(pred_prev),
            prev = mean(prev, na.rm = TRUE),
            count = mean(crime_integer_sim),
            sum_pred = sum(mod_fit),
            sum_count = sum(crime_integer_sim)) |> 
  print(n = 32)


simd <- 
simd |> 
  janitor::clean_names() |> 
  select(data_zone, intermediate_zone, council_area, crime_integer_sim, vio_integer_sim = vio_count_sim, simd2020_rank = simd2020v2_rank, contains("rank"), everything())


saveRDS(simd,
        here::here("resources", "simd_crime_sim.rds"))


glm(
  crime_integer_sim ~ simd2020_rank + total_population, 
  offset = log(total_population),
  family = "poisson",
  data = simd |> filter(total_population > 0)
) |> summary()


glm(
  crime_integer_sim ~ simd2020_rank, 
  offset = log(total_population),
  family = "poisson",
  data = simd |> filter(total_population > 0)
) |> summary()
