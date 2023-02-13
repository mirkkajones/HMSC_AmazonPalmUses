#################################################################################################################################################
# Codes associated with the analyses presented in the paper 
# "The legacy of human use in Amazonian palm communities along environmental and accessibility gradients". 
# Global Ecology and Biogeography. In press.
# Codes by Otso Ovaskainen, Mirkka Jones and Gabriela Zuquim
# The codes were modified from November 2020 Hmsc course scripts prepared by Otso Ovaskainen, Jari Oksanen and others.
# Current versions of these training materials are available at https://www.helsinki.fi/en/researchgroups/statistical-ecology/software/hmscCodes
# see readme file for details.
##################################################################################################################################################

library(Hmsc)

load(file = "unfitted_models.Rdata")

#### WARNING! Model fitting (next lines) would take many weeks to run on a normal computer (took 2 weeks with the settings below on a supercomputer).
#### For testing the codes, we hence recommend changing the values of nchains, samples and thin e.g. to 2, 50, 1, respectively

nChains = 4
samples = 250
thin = 1000
for(n in 1:1)
{
  m = models[[n]]
  m = sampleMcmc(m, samples = samples, thin=thin,
                 adaptNf=rep(ceiling(0.4*samples*thin),m$nr),
                 transient = ceiling(0.5*samples*thin),
                 nChains = nChains, nParallel = 1)
  models[[n]] = m
}

filename_out = paste("models_thin_", as.character(thin),
                     "_samples_", as.character(samples),
                     "_chains_",as.character(nChains),
                     ".Rdata",sep = "")

save(models, modelnames, file=filename_out)
