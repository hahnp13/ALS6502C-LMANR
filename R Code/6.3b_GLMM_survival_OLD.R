# install.packages(c("lubridate", "ggsurvfit", "gtsummary", "tidycmprsk"))
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(survival)
library(tidyverse)
library(emmeans)

# devtools::install_github("zabore/condsurv")
library(condsurv)

## example using lung cancer data
?lung

## build proportional hazard model - Surv is a "survival" object
cox1 <- coxph(Surv(time, status) ~ sex, data = lung)
cox1

summary(cox1)

## make kaplan meier plot
survmod <- survfit2(Surv(time, status) ~ sex, data = lung)

cox1plot <- ggsurvfit(survmod) +
  labs(x = "Days",
       y = "Overall survival probability")+ 
  add_confidence_interval() +
  scale_color_viridis(discrete = T, end=.8)+
  scale_fill_viridis(discrete = T, end=.8)
ggsave("cox1plot.tiff", cox1plot, width=6, height=4, units="in", dpi=600, compression = "lzw")

