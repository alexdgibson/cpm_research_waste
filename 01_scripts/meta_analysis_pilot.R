# meta_analysis_pilot.R
# run a Bayesian meta-analysis of AUC studies
# August 2024

# load libraries
library(dplyr)
library(nimble)
library(metamisc) # for EuroSCORE data

### part 1: run the standard meta-analysis as a comparison ###
data(EuroSCORE)
# Meta-analysis of the c-statistic (random effects)
fit <- valmeta(cstat = c.index, cstat.se = se.c.index, cstat.cilb = c.index.95CIl,
        cstat.ciub = c.index.95CIu, cstat.cilv = 0.95, N = n, O = n.events,
        slab = Study, data = EuroSCORE)
plot(fit)
# check variance against sample size
plot(log2(EuroSCORE$n), fit$data$theta.se)
median(EuroSCORE$n) # median study size



### part 2: run in nimble ###
## code; copied from metamisc, bugsmodels.r
code <- nimbleCode({
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
constants <- list(N = nrow(EuroSCORE))
data <- list(theta.var = fit$data$theta.se^2, # use results from valmeta
       theta = fit$data$theta)

## initial values
inits <- list(bsTau = 1,
      mu.tobs = 0)

# parameters to store
parms = c('mu.obs', 'pred.obs', 'bsTau')

# models
model <- nimbleModel(code, 
           data = data, 
           inits = inits, 
           constants = constants)

# chain details
n.chains = 2
thin = 5
MCMC = 10000
seeds = c(1234,5678) # one per chain

# MCMC samples
mcmc_out <- nimbleMCMC(model = model,
            inits = inits,
            monitors = parms,
            niter = MCMC*2*thin, # times 2 for burn-in 
            thin = thin,
            nchains = n.chains, 
            nburnin = MCMC,
            summary = TRUE, 
            setSeed = seeds,
            WAIC = FALSE)
mcmc_out$summary$all.chains
hist(mcmc_out$samples$chain1[,1])
hist(mcmc_out$samples$chain1[,2])



### part 3: run the nimble meta-analysis on data from the streptokinsane study
# data is extracted from streptokinase_meta_analysis_data_wrangle.R file

# load in the data
strep_df <- read.csv(file = "02_data/formatted_meta_analysis_odds_ratio.csv")
