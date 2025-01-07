library(tidyverse)

# bats_ctd <-

bats_bottles <- read_csv("~/Downloads/Bermuda Atlantic Time-Series Study (BATS) Bottle/BATS_Bottle.csv")

# N:P ~ 16:1
redfield_lm <- lm(nitrate_nitrite ~ phosphate, bats_bottles)
redfield_optim <- optimize(
  function(intercept) {
    pred_nitrate_nitrite <- bats_bottles$phosphate * 16 + intercept
    sum((bats_bottles$nitrate_nitrite - pred_nitrate_nitrite)^2, na.rm = TRUE)
  },
  interval = c(-1, 2)
)
ggplot(bats_bottles, aes(phosphate, nitrate_nitrite)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = coef(redfield_lm)[1],
              slope = coef(redfield_lm)[2],
              color = "firebrick",
              linewidth = 1.5) +
  geom_abline(intercept = redfield_optim$minimum,
              slope = 16,
              color = "cornflowerblue",
              linewidth = 1.5,
              linetype = "dotted") +
  theme_bw()

# Vary by depth?
bats_bottles$depth_bin <- cut(bats_bottles$depth, breaks = c(0, 100, 500, Inf))
find_redfield <- function(group) {
  as.numeric(coef(lm(nitrate_nitrite ~ phosphate, group)))
}

ggplot(filter(bats_bottles, depth_bin == "(100,500]"),
       aes(phosphate, nitrate_nitrite)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ depth_bin) +
  theme_bw()

bats_bottles %>%
  filter(between(depth, 100, 500)) %>%
  pivot_longer(c(oxygen, CO2),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(time, value)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw()

read_csv("~/Downloads/3918_v8_bats_ctd.csv") %>% names()



