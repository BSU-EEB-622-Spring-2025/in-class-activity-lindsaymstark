library(brms)
library(bayesplot)
library(ggplot2)
library(marginaleffects)
library(pROC)
library(MLmetrics)
library(modelr) 

skelton <- read.csv("frog_data_eeb/skelton.csv")
head(skelton)

lk70413 <- read.csv("frog_data_eeb/70413.csv")
head(lk70413)

skelton$cohort_id <- as.factor(skelton$cohort_id)
lk70413$cohort <- as.factor(lk70413$cohort)


ggplot(skelton) + 
  aes(x = Mean_temp, y = YOY_survival, color=cohort_id) + 
  geom_point() + # Color code observations by cohort
  theme_bw() +
  ylab("Year-over-Year Survival") +
  xlab("Mean Winter Temp (deg c)")

ggplot(lk70413) + 
  aes(x = Mean_temp, y = YOY_survival, color=cohort) + 
  geom_point() + # Color code observations by cohort
  theme_bw() +
  ylab("Year-over-Year Survival") +
  xlab("Mean Winter Temp (deg c)")

skelton_mod <- brm(YOY_survival ~ Mean_temp + 
                (1|cohort_id), ## this is our varying intercept 
              data=skelton,
              chains=4,
              iter=8000,
              control = list(adapt_delta = 0.99),
              family="zero_one_inflated_beta"(link=logit))
summary(skelton_mod)

lk70413_mod <- brm(YOY_survival ~ Mean_temp + 
                     (1|cohort), ## this is our varying intercept 
                   data=lk70413,
                   chains=4,
                   iter=8000,
                   control = list(adapt_delta = 0.99),
                   family="zero_one_inflated_beta"(link=logit))
summary(lk70413_mod)


posterior <- as.data.frame(skelton_mod)
head(posterior)
length(which(posterior$b_Mean_temp<0))/nrow(posterior)

posterior <- as.data.frame(lk70413_mod)
head(posterior)
length(which(posterior$b_Mean_temp>0))/nrow(posterior)


mcmc_plot(skelton_mod,pars=c("^b", "^sd")) + labs(title = "Lake 70550") + theme(plot.title = element_text(hjust = 0.5)) ## Just plots the "fixed" effects and the sd for the varying intercept (which begin with the strings "b_" or "sd")
mcmc_plot(lk70413_mod,pars=c("^b", "^sd")) + labs(title = "Lake 70413") + theme(plot.title = element_text(hjust = 0.5))

## The lines below will just show our varying intercepts (which all begin with the string "r_").
mcmc_plot(skelton_mod, variable = c("^r_")) + labs(title = "Lake 70550") + theme(plot.title = element_text(hjust = 0.5))

mcmc_plot(lk70413_mod, pars = c("^r_")) 

conditional_effects(skelton_mod) ## brms has a "marginal effects" plotting function built in, but the marginaleffects package works here still too!
plot_predictions(skelton_mod, condition = c("Mean_temp")) + theme_bw()

## The plots above show the mean prediction, across all bats, however we can also plot each "bat-level" prediction. Why or why would we NOT want to do this?
plot_predictions(skelton_mod, condition = c("Mean_temp", "cohort_id")) + theme_bw() + labs(title = "Lake 70550", x = "Mean Winter Temperature (deg C)", y = "Year-to-year Survival Proportion") +
  theme(plot.title = element_text(hjust = 0.5))

conditional_effects(lk70413_mod) ## brms has a "marginal effects" plotting function built in, but the marginaleffects package works here still too!
plot_predictions(lk70413_mod, condition = c("Mean_temp")) + theme_bw()

## The plots above show the mean prediction, across all bats, however we can also plot each "bat-level" prediction. Why or why would we NOT want to do this?
plot_predictions(lk70413_mod, condition = c("Mean_temp", "cohort")) + theme_bw()  + labs(title = "Lake 70413", x = "Mean Winter Temperature (deg C)", y = "Year-to-year Survival Proportion") +
  theme(plot.title = element_text(hjust = 0.5)) 


bayes_R2(skelton_mod)
performance::mae(skelton_mod)

bayes_R2(lk70413_mod)
performance::mae(lk70413_mod)


preds <- posterior_predict(skelton_mod, ndraws = 10 )


