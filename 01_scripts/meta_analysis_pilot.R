# meta_analysis_pilot.R
# run a Bayesian meta-analysis of AUC studies
# August 2024

# load libraries
library(dplyr)
library(nimble)
library(metamisc) # for EuroSCORE data
library(ggplot2)

### part 1: run the standard meta-analysis as a comparison ###
data(EuroSCORE)

# Meta-analysis of the c-statistic (random effects)
fit_discr <- valmeta(cstat = c.index, cstat.se = se.c.index, cstat.cilb = c.index.95CIl,
                     cstat.ciub = c.index.95CIu, cstat.cilv = 0.95, N = n, O = n.events,
                     slab = Study, data = EuroSCORE)

plot(fit_discr)

# check variance against sample size
plot(log2(EuroSCORE$n), fit_discr$data$theta.se)
median(EuroSCORE$n) # median study size


### part 1b: make the cumulative meta-analysis ###

# complete a meta-analysis for every kth study
for (study in 1:nrow(EuroSCORE)) {
  
  # Create dynamic variable names like fit1, fit2, fit3, etc.
  fit_name <- paste("fit", study, sep = "")
  
  fit <- valmeta(cstat = c.index, cstat.se = se.c.index, cstat.cilb = c.index.95CIl,
                 cstat.ciub = c.index.95CIu, cstat.cilv = 0.95, N = n, O = n.events,
                 slab = Study, data = EuroSCORE[1:study,])
  
  # Dynamically assign the 'fit' result to the new variable name
  assign(fit_name, fit)
}

# take the estaimte from each study
est <- c(fit1$est,fit2$est,fit3$est,fit4$est,fit5$est,fit6$est,fit7$est,fit8$est,fit9$est,fit10$est,
        fit11$est,fit12$est,fit13$est,fit14$est,fit15$est,fit16$est,fit17$est,fit18$est,fit19$est,fit20$est,
        fit21$est,fit22$est,fit23$est) %>% 
  as.data.frame() %>% 
  rename(., "est" = .)

# take the ci lower from each study
ci.lo <- c(fit1$ci.lb,fit2$ci.lb,fit3$ci.lb,fit4$ci.lb,fit5$ci.lb,fit6$ci.lb,fit7$ci.lb,fit8$ci.lb,fit9$ci.lb,fit10$ci.lb,
         fit11$ci.lb,fit12$ci.lb,fit13$ci.lb,fit14$ci.lb,fit15$ci.lb,fit16$ci.lb,fit17$ci.lb,fit18$ci.lb,fit19$ci.lb,fit20$ci.lb,
         fit21$ci.lb,fit22$ci.lb,fit23$ci.lb) %>% 
  as.data.frame()%>% 
  rename(., "ci.lo" = .)

# take the ci upper from each study
ci.up <- c(fit1$ci.ub,fit2$ci.ub,fit3$ci.ub,fit4$ci.ub,fit5$ci.ub,fit6$ci.ub,fit7$ci.ub,fit8$ci.ub,fit9$ci.ub,fit10$ci.ub,
         fit11$ci.ub,fit12$ci.ub,fit13$ci.ub,fit14$ci.ub,fit15$ci.ub,fit16$ci.ub,fit17$ci.ub,fit18$ci.ub,fit19$ci.ub,fit20$ci.ub,
         fit21$ci.ub,fit22$ci.ub,fit23$ci.ub) %>% 
  as.data.frame()%>% 
  rename(., "ci.up" = .)

pred_int_lb <- fit23$pi.lb %>% 
  as.data.frame()%>% 
  rename(., "pi.lp" = .)

pred_int_ub <- fit23$pi.ub%>% 
  as.data.frame()%>% 
  rename(., "pi.up" = .)

# use factors to plot separately
name <- factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), levels =c(23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1))

# combine to one df
plot <- cbind(est, ci.lo, ci.up, name, pred_int_lb, pred_int_ub)

# plot
plot %>%
  ggplot(aes(x = est, y = name)) +
  geom_point() +
  geom_errorbar(aes(xmin = ci.lo, xmax = ci.up)) +
  coord_cartesian(xlim = c(0.5, 1))













### part 2a: run in nimble for the discrimination ###
## code; copied from metamisc, bugsmodels.r
code_discr <- nimbleCode({
 ## Likelihood
 for (i in 1:N){ # loop through studies
  theta[i] ~ dnorm(alpha[i], wsprec[i])
  alpha[i] ~ dnorm(mu.tobs, bsprec) # study-specific estimate centred on overall mean
  wsprec[i] <- 1/(theta.var[i]) # study-specific precision using observed variance
 }
 # priors
 bsTau ~ T(dt(0, 1/(0.5^2), df = 3), 0, 10) # truncated, see paper for prior set-up
 mu.tobs ~ dnorm(0, 1/1000) # overall mean (logit)
 pred.tobs ~ dnorm(mu.tobs, bsprec) # prediction interval (logit)
 # back-transform to [0,1] and scalar:
 logit(mu.obs) <- mu.tobs
 logit(pred.obs) <- pred.tobs
 bsprec <- 1/(bsTau*bsTau) # transform SD to precision
})

## data
constants_discr <- list(N = nrow(EuroSCORE))

data_discr <- list(theta.var = fit_discr$data$theta.se^2, # use results from valmeta | this is the variance, it is calculated from different data when not directly available
             theta = fit_discr$data$theta) # this is the logit c-stat

## initial values
inits_discr <- list(bsTau = 1,
              mu.tobs = 0)

# parameters to store
parms_discr = c('mu.obs', 'pred.obs', 'bsTau')

# models
model_discr <- nimbleModel(code_discr, 
           data = data_discr, 
           inits = inits_discr, 
           constants = constants_discr)

# chain details
n.chains = 2
thin = 5
MCMC = 10000
seeds = c(1234,5678) # one per chain

# MCMC samples
mcmc_out_discr <- nimbleMCMC(model = model_discr,
                             inits = inits_discr,
                             monitors = parms_discr,
                             niter = MCMC*2*thin, # times 2 for burn-in 
                             thin = thin,
                             nchains = n.chains, 
                             nburnin = MCMC,
                             summary = TRUE, 
                             setSeed = seeds,
                             WAIC = FALSE)

# view the summary of all chains
mcmc_out_discr$summary$all.chains

# compare with valmeta result
fit_discr

# view the summary of a single chain
hist(mcmc_out_discr$samples$chain1[,1])
hist(mcmc_out_discr$samples$chain1[,2])

hist(mcmc_out_discr$samples$chain2[,1])
hist(mcmc_out_discr$samples$chain2[,2])





### part 2b: create a cumulative meta-analysis for the discrimination ###
# create a list of data lists in cumulative to make into multiple models
# create an empty list to store data
data_list <- list()

# create n data lists in cumulative to complete meta-analysis each time
for(i in 1:nrow(EuroSCORE)){
  
  a <- as.numeric(data_discr[[1]][1:i])
  b <- as.numeric(data_discr[[2]][1:i])
  name <- paste('item', i, sep='')
  tmp <- list(theta.var = a, theta = b)
  data_list[[name]] <- tmp
}

# create n models in cumulative to then be run through NimbleMCMC
# create an empty list to store data
cum_discr <- list()

# create n lists of models to complete meta-analysis each time
for(i in 1:length(data_list)){
  
  model_discr <- nimbleModel(code_discr, 
                             data = data_list[[i]], 
                             inits = inits_discr, 
                             constants = list(N = i))
  name <- paste('item', i, sep='')
  tmp <- list(model_1 = model_discr)
  cum_discr[[name]] <- tmp
}

# run the meta-analysis for each model in NimbleMCMC
# create an empty list to store outputs
cum_meta_discr <- list()

# run each cumulative model through MCMC
for(i in 1:length(cum_discr)){
  
  mcmc_out_discr <- nimbleMCMC(model = cum_discr[[i]][[1]],
                               inits = inits_discr,
                               monitors = parms_discr,
                               niter = MCMC*2*thin, # times 2 for burn-in 
                               thin = thin,
                               nchains = n.chains, 
                               nburnin = MCMC,
                               summary = TRUE, 
                               setSeed = seeds,
                               WAIC = FALSE)
  
  name <- paste('meta', i, sep='')
  tmp <- list(model = mcmc_out_discr)
  cum_meta_discr[[name]] <- tmp
}


# check that the meta-analysis was completed for each of the cumulative models
for (i in 1:length(cum_meta_discr)){
     print(cum_meta_discr[[i]]$model$summary$all.chains)
}

# take the output of all chains from the model
# create an empty list to store the outputs
cum_discr_plot <- list()

# take the outputs of each model
for (i in 1:length(cum_meta_discr)){
  
  mu <- cum_meta_discr[[i]]$model$summary$all.chains
  mu_df <- as.data.frame(mu)
  name <- paste('study', i, sep='')
  tmp <- list(summary = mu_df)
  cum_discr_plot[[name]] <- tmp
}


# from all the data take mu and confidence intervals to then be plotted
# create an empty list to store data
cum_discr_forest_plot <- data.frame()

# take mu and confidence intervals
for (i in 1:length(cum_discr_plot)){
  
  df <- as.data.frame(cum_discr_plot[[i]][[1]][2,1:5])
  cum_discr_forest_plot <- rbind(cum_discr_forest_plot, df)
}

# plot a forest plot for the discrimination
# for each of the cumulative models plot the mean and confidence interval
cum_discr_forest_plot %>% 
  mutate(name = 23:1) %>% 
  ggplot()+
  geom_errorbar(aes(x = Mean, y = name,
                    xmin = `95%CI_low`,
                    xmax = `95%CI_upp`))+
  geom_point(aes(x = Mean, y = name))+
  coord_cartesian(xlim = c(0.5, 1))+
  theme_classic()













### Part 3a: create the model for the calibration ###
# Meta-analysis of the O:E ratio (random effects)
fit_cal <- valmeta(measure = "OE",  N = n, O = n.events, E = e.events,
               slab = Study, data = EuroSCORE)

# check summary
fit_cal

# check plot
plot(fit_cal)

# check variance against sample size
plot(log2(EuroSCORE$n), fit_cal$data$theta.se)
median(EuroSCORE$n) # median study size

## code; copied from metamisc, bugsmodels.r
code_cal <- nimbleCode({
  for (i in 1:N){
    theta[i] ~ dnorm(alpha[i], wsprec[i])
    alpha[i] ~ dnorm(mu.tobs, bsprec)
    wsprec[i] <- 1/(theta.var[i])
  }
  # priors
  bsTau ~ T(dt(0, 1/(1.5^2), df = 3), 0, 10) # truncated, see paper for prior set-up
  mu.tobs ~ dnorm(0, 1/1000) # overall mean
  pred.tobs ~ dnorm(mu.tobs, bsprec) # prediction interval
  # back-transform from natutal log
  log(mu.oe) <- mu.tobs
  log(pred.oe) <- pred.tobs
  bsprec <- 1/(bsTau*bsTau) # transform SD to precision
})

## constants
constants_cal <- list(N = nrow(EuroSCORE))

# take the data from the valmeta fit
# it will calculate the O:E from other params when not directly available 
data_cal <- list(theta.var = fit_cal$data$theta.se^2, # use results from valmeta
             theta = fit_cal$data$theta)

## initial values
inits_cal <- list(bsTau = 1,
              mu.tobs = 0)

# parameters to store
parms_cal = c('mu.oe', 'pred.oe', 'bsTau')

# model
model_cal <- nimbleModel(code_cal, 
                        data = data_cal, 
                        inits = inits_cal, 
                        constants = constants_cal)

# chain details
n.chains = 2
thin = 5
MCMC = 10000
seeds = c(1234,5678) # one per chain

# MCMC samples
mcmc_out_cal <- nimbleMCMC(model = model_cal,
                       inits = inits_cal,
                       monitors = parms_cal,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seeds,
                       WAIC = FALSE)

# view the summary of all chains
mcmc_out_cal$summary

# compare with the valmeta result
fit_cal

# view the summary of a single chain
hist(mcmc_out_cal$samples$chain1[,1])
hist(mcmc_out_cal$samples$chain1[,2])






### part 3b: create a cumulative meta-analysis for the calibration ###
# create a list of data lists in cumulative to make into multiple models
# create an empty list to store data
data_list_cal <- list()

# create n data lists in cumulative to complete meta-analysis each time
for(i in 1:nrow(EuroSCORE)){
  
  a <- as.numeric(data_cal[[1]][1:i])
  b <- as.numeric(data_cal[[2]][1:i])
  name <- paste('item', i, sep='')
  tmp <- list(theta.var = a, theta = b)
  data_list_cal[[name]] <- tmp
}

# create n models in cumulative to then be run through NimbleMCMC
# create an empty list to store data
cum_cal <- list()

# create n lists of models to complete meta-analysis each time
for(i in 1:length(data_list_cal)){
  
  model_cal <- nimbleModel(code_cal, 
                             data = data_list_cal[[i]], 
                             inits = inits_cal, 
                             constants = list(N = i))
  name <- paste('item', i, sep='')
  tmp <- list(model_1 = model_cal)
  cum_cal[[name]] <- tmp
}


# run the meta-analysis for each model in NimbleMCMC
# create an empty list to store outputs
cum_meta_cal <- list()

# run each cumulative model through MCMC
for(i in 1:length(cum_cal)){
  
  mcmc_out_cal <- nimbleMCMC(model = cum_cal[[i]][[1]],
                               inits = inits_cal,
                               monitors = parms_cal,
                               niter = MCMC*2*thin, # times 2 for burn-in 
                               thin = thin,
                               nchains = n.chains, 
                               nburnin = MCMC,
                               summary = TRUE, 
                               setSeed = seeds,
                               WAIC = FALSE)
  
  name <- paste('meta', i, sep='')
  tmp <- list(model = mcmc_out_cal)
  cum_meta_cal[[name]] <- tmp
}


# check that the meta-analysis was completed for each of the cumulative models
for (i in 1:length(cum_meta_cal)){
  print(cum_meta_cal[[i]]$model$summary$all.chains)
}

# take the output of all chains from the model
# create an empty list to store the outputs
cum_cal_plot <- list()

# take the outputs of each model
for (i in 1:length(cum_meta_cal)){
  
  mu <- cum_meta_cal[[i]]$model$summary$all.chains
  mu_df <- as.data.frame(mu)
  name <- paste('study', i, sep='')
  tmp <- list(summary = mu_df)
  cum_cal_plot[[name]] <- tmp
}


# from all the data take mu and confidence intervals to then be plotted
# create an empty list to store data
cum_cal_forest_plot <- data.frame()

# take mu and confidence intervals
for (i in 1:length(cum_cal_plot)){
  
  df <- as.data.frame(cum_cal_plot[[i]][[1]][2,1:5])
  cum_cal_forest_plot <- rbind(cum_cal_forest_plot, df)
}

# plot a forest plot for the discrimination
# for each of the cumulative models plot the mean and confidence interval
cum_cal_forest_plot %>% 
  mutate(name = 23:1) %>% 
  ggplot()+
  geom_errorbar(aes(x = Mean, y = name,
                    xmin = `95%CI_low`,
                    xmax = `95%CI_upp`))+
  geom_point(aes(x = Mean, y = name))+
  coord_cartesian(xlim = c(0, 10))+
  theme_classic()



















### Part 4 create a funnel plot for the discrimination and calibration ###
# use the metamisc package function fat() to do so

# create the objects needed for the fat function
b_discr <- EuroSCORE$c.index
b_se_discr <- EuroSCORE$se.c.index
n_total_discr <- EuroSCORE$n
d_total_discr <- EuroSCORE$n.events

# complete the meta-regression using fat for the discrimination
funnel_discr <- EuroSCORE %>% fat(b = b_discr,
                                  b.se = b_se_discr,
                                  n.total = n_total_discr,
                                  d.total = n_total_discr,
                                  method = "E-FIV")

# plot the funnel plot for the discrimination
plot(funnel_discr)


# create the objects needed for the fat function
b_cal <- EuroSCORE$Po/EuroSCORE$Pe
b_se_cal <- EuroSCORE$SD.Pe/sqrt(EuroSCORE$n)
n_total_cal <- EuroSCORE$n
d_total_cal <- EuroSCORE$n.events

# complete the meta-regression using fat for the calibration
funnel_cal <- EuroSCORE %>% fat(b = b_cal,
                                b.se = b_se_cal,
                                n.total = n_total_cal,
                                d.total = d_total_cal,
                                method = "E-FIV")

# plot the funnel plot for the calibration
plot(funnel_cal)













