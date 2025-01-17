#################################################################################################################################################
# Codes associated with the analyses presented in the paper 
# "The legacy of human use in Amazonian palm communities along environmental and accessibility gradients". 
# Global Ecology and Biogeography. In press.
# Codes by Otso Ovaskainen, Mirkka Jones and Gabriela Zuquim
# The codes were modified from November 2020 Hmsc course scripts prepared by Otso Ovaskainen, Jari Oksanen and others.
# Current versions of these training materials are available at https://www.helsinki.fi/en/researchgroups/statistical-ecology/software/hmscCodes
# see readme file for details.
##################################################################################################################################################

load("models_thin_1000_samples_250_chains_4.Rdata")

library(Hmsc)

m = models[[1]]
postBeta = getPostEstimate(m, parName="Beta")
Betameans = t(postBeta$mean[-1,])
colnames(Betameans) = m$covNames[-1]
head(m$TrData)
Fig2_data = cbind(m$TrData$use_intensity, Betameans)
colnames(Fig2_data)[1] = "N human uses"
colnames(Fig2_data)[-1] = paste0("Beta_", colnames(Fig2_data)[-1])

write.csv2(Fig2_data, "Fig2_data_UseIntensity_Beta.csv")
