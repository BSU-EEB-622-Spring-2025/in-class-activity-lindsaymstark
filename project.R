
library(brms)
library(ggplot2)
library(tibble)
library(marginaleffects)

recordings <- read.csv("recordings.csv")
sensorinfo <- read.csv("sensorinfo.csv")

recordings$watertemp <- as.numeric(recordings$watertemp)

# Merge the data frames using sensorid
merged_df <- merge(recordings, sensorinfo, by = "sensorid", all.x = TRUE)


noise_lengthmod <- brm(songlength~scale(boatnoise)+scale(boatactivity)+scale(watertemp)+(1|sensorid), data=merged_df, family="gamma")
summary(noise_lengthmod)
mcmc_plot(noise_lengthmod, type="areas") + theme_bw()
plot(noise_lengthmod)

noise_totmod <- brm(totsongs~scale(boatnoise)+scale(boatactivity)+scale(watertemp) + (1|sensorid), data=merged_df, family = "negbinomial")
summary(noise_totmod)
mcmc_plot(noise_totmod, type="areas") + theme_bw()
plot(noise_totmod)

activity_lengthmod <- brm(songlength~scale(boatactivity)+scale(boatnoise)+scale(watertemp)+scale(waterdepth)+scale(distshore) +(1|sensorid), data=merged_df,family="gamma")
summary(activity_lengthmod)
mcmc_plot(activity_lengthmod, type="areas") + theme_bw() 
plot(activity_lengthmod)

activity_totmod <- brm(totsongs~scale(boatactivity)+scale(boatnoise)+scale(watertemp)+scale(waterdepth)+scale(distshore) +(1|sensorid), data=merged_df, family = "negbinomial")
summary(activity_totmod)
mcmc_plot(activity_totmod, type="areas") + theme_bw()
plot(activity_totmod)

bayes_R2(noise_lengthmod)
bayes_R2(noise_totmod)
bayes_R2(activity_lengthmod)
bayes_R2(activity_totmod)

performance::mae(noise_lengthmod)
performance::mae(noise_totmod)
performance::mae(activity_lengthmod)
performance::mae(activity_totmod)

plot_predictions(noise_lengthmod, condition = c("boatnoise")) + theme_bw()
plot_predictions(noise_totmod, condition = c("boatnoise")) + theme_bw()
plot_predictions(activity_lengthmod, condition = c("boatactivity")) + theme_bw()
plot_predictions(activity_totmod, condition = c("boatactivity")) + theme_bw()

