library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(mgcv)
library(tidymv)

set.seed(10)
data <- gamSim(4, 400)
# model 1
## > Factor `by' variable example-------------
model <- gam(
  y ~
    fac +
    s(x2, by = fac),
  data = data
)

summary(model)
## extract predictions -----------------
model_p <- predict_gam(model)
model_p

## plot -----------------------------
model_p %>%
  ggplot(aes(x2, fit)) +
  geom_smooth_ci(fac)

# model with ti() ----------------
model_2 <- gam(
  y ~
    fac +
    s(x2, by=fac) +
    s(f1) +
    ti(x2, f1),
  data = data
)

summary(model_2)
## plot
model_2_pe <- predict_gam(model_2,exclude_terms = f1)

model_2_pe %>%
  ggplot(aes(x2, fit)) +
  geom_smooth_ci(fac)


## contour plot -----------------------
model_2_p <- predict_gam(model_2)
model_2_p
model_2_p %>%
  ggplot(aes(x2, f1, z = fit)) +
  geom_raster(aes(fill = fit)) +
  geom_contour(colour = "white") +
  scale_fill_continuous(name = "y") +
  theme_minimal() +
  theme(legend.position = "top")
