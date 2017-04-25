# META ANALYSES \ META REGRESSION
##
#
#
# random effect of case nested within study, used for all meta-analytic models
rand <- list(~ 1 | studyID/id)

# slope as an effect size
# slope is not an appropriate effect size because of the non-normally distributed residuals
global.slope.init <- rma.mv(yi = slope_b, V = Var_b, random = rand, data = dat, method = "ML")
plot(residuals.rma(global.slope.init))
hist(residuals.rma(global.slope.init))
# all other analyses use z as effect size


# Global/grand mean calculations ---------------------------------------
#
# HILLEBRAND'S DATASET
##
#
# Load and clean Hillebrand's data
#
Hillebrand <- gs_title("Hillebrand.csv") #load data from Hillebrand (2004)
Hil <- gs_read_csv(Hillebrand, ws = "Hillebrand.csv", col_names = TRUE)
Hil <- as.data.frame(Hil)
var.remove <- which(is.na(Hil$VarRz))
Hil <-Hil[-var.remove, ]
x.replace <- which(Hil == "x", arr.ind = TRUE)
Hil[x.replace] <- NA
casetab <- with(Hil, tapply(X1, paper, function(x) unique(x)))
caselab <- unlist(lapply(casetab, function(x) 1:length(x)))

# change character (categorical variates) to factor
for (i in 1:ncol(Hil)) {
  if (class(Hil[, i]) == "character") {
    Hil[, i] <- as.factor(Hil[, i])
  }
}
str(Hil)
Hil$id <- caselab
Hil$studyID <- Hil$paper

# global mean
# correlation coefficient - Fisher's z (rz)
global.H <- rma.mv(yi = Rz, V = VarRz, random = rand, data = Hil, method = "ML")
summary(global.H)


#
# OUR DATASET
##
#
# global mean
# correlation coefficient - Fisher's z (rz)
global <- rma.mv(yi = rz, V = VarRz, random = rand, data = dat, method = "ML")
summary(global)

#
# COMBINED DATASET
##
#
# Fisher's z global mean INCLUDING our cases and Hillebrand's cases
Hil.combine <- data.frame(id = Hil$id, studyID = Hil$studyID, rz = Hil$Rz, VarRz = Hil$VarRz)
Kin.combine <- data.frame(id = dat$id, studyID = dat$studyID, rz = dat$rz, VarRz = dat$VarRz)
combined <- rbind(Hil.combine, Kin.combine)
global.combined <- rma.mv(yi = rz, V = VarRz, random = rand, data = combined, method = "ML")
summary(global.combined)


#
# subset: ANIMALIA
##
# 
# subset of data with all plants to feed to rma
animalia <- which(dat$kingdom == "Animalia")
dat.ani <- dat[animalia, ]
# grand mean
#
ani <- rma.mv(yi = rz, V = VarRz, random = rand, data = dat.ani, method = "ML")
summary(ani)

#
# subset: PLANTAE
##
#
# subset of data with all plants to feed to rma
plantae <- which(dat$kingdom == "Plantae")
dat.pla <- dat[plantae, ]
# grand mean
#
pla <- rma.mv(yi = rz, V = VarRz, random = rand, data = dat.pla, method = "ML")
summary(pla)

#
# subset: TERRESTRIAL
##
#
# subset of data with only terrestrial realm to feed to rma
ter <- which(dat$realm == "terrestrial")
dat.ter <- dat[ter, ]
# grand mean
#
ter <- rma.mv(yi = rz, V = VarRz, random = rand, data = dat.ter, method = "ML")
summary(ter)

#
# subset: MARINE
##
#
# subset of data with only terrestrial realm to feed to rma
mar <- which(dat$realm == "marine")
dat.mar <- dat[mar, ]
# grand mean
#
mar <- rma.mv(yi = rz, V = VarRz, random = rand, data = dat.mar, method = "ML")
summary(mar)


# Variable diagnostics ---------------------------------------

# are the variables of interest correlated?
# pairs(~ longitude_midpoint + longitude_range + latitude_range + latitude_midpoint, data = dat)

# hetcor in the polycor allows for the calculation of r between numeric, polyserial correlations between numeric and ordinal, polychoric correlations between ordinal
# correlations <- hetcor(data = dat, std.err = FALSE)
# correlations
# Hil.corr <- hetcor(data = Hil[, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19)], std.err = FALSE)
# Hil.corr

# # take a closer look at correlated variables of interest
# # scale and diversity
# table(dat$scale, dat$diversity)
# chisq.test(x = dat$scale, y = dat$diversity)
# table(Hil$scale, Hil$diversity)
# chisq.test(x = Hil$scale, y = Hil$diversity)
# # taxonomic resolution and measure of richness
# table(dat$taxonomic_resolution, dat$measure_of_richness)
# chisq.test(x = dat$taxonomic_resolution, y = dat$measure_of_richness)
# # measure of richness and diversity
# table(dat$diversity, dat$measure_of_richness)
# chisq.test(x = dat$diversity, y = dat$measure_of_richness)
# table(Hil$diversity, Hil$measured)
# chisq.test(x = Hil$diversity, y = Hil$measured)
# # realm and habitat
# table(dat$realm, dat$habitat)
# chisq.test(x = dat$realm, y = dat$habitat)
# table(Hil$realm, Hil$habitat)
# chisq.test(x = Hil$realm, y = Hil$habitat)
# # organism group and dispersal type
# table(Hil$organism_group, Hil$dispersal_type)
# chisq.test(Hil$organism_group, Hil$dispersal_type)




# Moderator analysis \ metaregression with single covariates ---------------------------------------
#
# generate using rma (random effects model with single moderator and no intercept)

# OUR DATA
##
#
# specific figures with "multiple" or "both" removed where necessary (multiple often contains too much variation)
subset.hemi <- dat[which(dat$hemisphere != "both"), ]
subset.orgrp <- dat[which(dat$organism_group != "multiple"), ]
subset.therm <- dat[which(dat$thermoregulation != "multiple"), ]
subset.hab <- dat[which(dat$habitat != "Multiple"), ]
king <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + kingdom, random = rand, data = dat, method = "ML")
orggrp <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + factor(organism_group), random = rand, data = subset.orgrp, method = "ML")
therm <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + factor(thermoregulation), random = rand, data = subset.therm, method = "ML")
realm <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + realm, random = rand, data = dat, method = "ML")
hab <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + factor(subset.hab$habitat), random = rand, data = subset.hab, method = "ML")
troph <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + trophic_position, random = rand, data = dat, method = "ML")
hemi <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + factor(hemisphere), random = rand, data = subset.hemi, method = "ML")
div <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + diversity, random = rand, data = dat, method = "ML")
meas <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + measure_of_richness, random = rand, data = dat, method = "ML")
latmid <- rma.mv(yi = rz, V = VarRz, mods = ~ latitude_midpoint, random = rand, data = dat, method = "ML")
latrange <- rma.mv(yi = rz, V = VarRz, mods = ~ latitude_range, random = rand, data = dat, method = "ML")
longmid <- rma.mv(yi = rz, V = VarRz, mods = ~ longitude_midpoint, random = rand, data = dat, method = "ML")
longmid2 <- rma.mv(yi = rz, V = VarRz, mods = ~ longitude_midpoint + I(longitude_midpoint^2), random = rand, data = dat, method = "ML")
longmidcirc <- rma.mv(yi = rz, V = VarRz, mods = ~ sin((longitude_midpoint + 180) * (pi / 180)) + cos((longitude_midpoint + 180) * (pi / 180)), random = rand, data = dat, method = "ML")
# split longitude into evenly spaced bins
lab.init <- seq(-180, 180, 60)
lab.long <- c()
for (i in 1:(length(lab.init) - 1)) {
  lab.long[i] <- paste(lab.init[i], lab.init[i + 1], sep = ",")
}
dat$region <- as.factor(cut(dat$longitude_midpoint, lab.init, labels = FALSE))
longreg <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + region, random = rand, data = dat, method = "ML")
# terrestrial habitats only
dat.ter$region.ter <- as.factor(cut(dat.ter$longitude_midpoint, lab.init, labels = FALSE))
terrestrial <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + region.ter, random = rand, data = dat.ter, method = "ML")
# marine habitats only
dat.mar$region.mar <- as.factor(cut(dat.mar$longitude_midpoint, lab.init, labels = FALSE))
marine <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + region.mar, random = rand, data = dat.mar, method = "ML")
dat.marter <- rbind.fill(dat.ter, dat.mar)
marter <- rma.mv(yi = rz, V = VarRz, mods = ~ -1 + factor(realm), random = rand, data = dat.marter, method = "ML")

# including intercept to get an accurate Q-test for heterogeneity
# rma.mv(yi = rz, V = VarRz, mods = ~ kingdom, random = rand, data = dat, method = "ML")
# rma.mv(yi = rz, V = VarRz, mods = ~ factor(organism_group), random = rand, data = subset.orgrp, method = "ML")
# rma.mv(yi = rz, V = VarRz, mods = ~ factor(thermoregulation), random = rand, data = subset.therm, method = "ML")
# rma.mv(yi = rz, V = VarRz, mods = ~ realm, random = rand, data = dat, method = "ML")
# rma.mv(yi = rz, V = VarRz, mods = ~ factor(subset.hab$habitat), random = rand, data = subset.hab, method = "ML")
# rma.mv(yi = rz, V = VarRz, mods = ~ trophic_position, random = rand, data = dat, method = "ML")
# rma.mv(yi = rz, V = VarRz, mods = ~ factor(hemisphere), random = rand, data = subset.hemi, method = "ML")
# rma.mv(yi = rz, V = VarRz, mods = ~ diversity, random = rand, data = dat, method = "ML")
# rma.mv(yi = rz, V = VarRz, mods = ~ measure_of_richness, random = rand, data = dat, method = "ML")
# rma.mv(yi = rz, V = VarRz, mods = ~ region, random = rand, data = dat, method = "ML")
# rma.mv(yi = rz, V = VarRz, mods = ~ factor(realm), random = rand, data = dat.marter, method = "ML")



# HILLEBRAND'S DATA
##
#
H.orggrp <- rma.mv(yi = Rz, V = VarRz, mods = ~ -1 + organism_group, random = rand, data = Hil, method = "ML")
H.therm <- rma.mv(yi = Rz, V = VarRz, mods = ~ -1 + thermoregulation, random = rand, data = Hil, method = "ML")
H.realm <- rma.mv(yi = Rz, V = VarRz, mods = ~ -1 + realm, random = rand, data = Hil, method = "ML")
H.hab <- rma.mv(yi = Rz, V = VarRz, mods = ~ -1 + habitat, random = rand, data = Hil, method = "ML")
H.hemi <- rma.mv(yi = Rz, V = VarRz, mods = ~ -1 + hemisphere, random = rand, data = Hil, method = "ML")
H.scale <- rma.mv(yi = Rz, V = VarRz, mods = ~ -1 + scale, random = rand, data = Hil, method = "ML")
H.div <- rma.mv(yi = Rz, V = VarRz, mods = ~ -1 + diversity, random = rand, data = Hil, method = "ML")
H.troph <- rma.mv(yi = Rz, V = VarRz, mods = ~ -1 + trophic_position, random = rand, data = Hil, method = "ML")
H.long <- rma.mv(yi = Rz, V = VarRz, mods = ~ -1 + longitude, random = rand, data = Hil, method = "ML")
H.rich <- rma.mv(yi = Rz, V = VarRz, mods = ~ Log_Sglob, random = rand, data = Hil, method = "ML")
H.body <- rma.mv(yi = Rz, V = VarRz, mods = ~ log_bodyweight, random = rand, data = Hil, method = "ML")
H.grain <- rma.mv(yi = Rz, V = VarRz, mods = ~ grain, random = rand, data = Hil, method = "ML")
H.latrange <- rma.mv(yi = Rz, V = VarRz, mods = ~ range, random = rand, data = Hil, method = "ML")

# including intercept to get an accurate Q-test for heterogeneity
# rma.mv(yi = Rz, V = VarRz, mods = ~ organism_group, random = rand, data = Hil, method = "ML")
# rma.mv(yi = Rz, V = VarRz, mods = ~ thermoregulation, random = rand, data = Hil, method = "ML")
# rma.mv(yi = Rz, V = VarRz, mods = ~ realm, random = rand, data = Hil, method = "ML")
# rma.mv(yi = Rz, V = VarRz, mods = ~ habitat, random = rand, data = Hil, method = "ML")
# rma.mv(yi = Rz, V = VarRz, mods = ~ hemisphere, random = rand, data = Hil, method = "ML")
# rma.mv(yi = Rz, V = VarRz, mods = ~ scale, random = rand, data = Hil, method = "ML")
# rma.mv(yi = Rz, V = VarRz, mods = ~ diversity, random = rand, data = Hil, method = "ML")
# rma.mv(yi = Rz, V = VarRz, mods = ~ trophic_position, random = rand, data = Hil, method = "ML")
# rma.mv(yi = Rz, V = VarRz, mods = ~ longitude, random = rand, data = Hil, method = "ML")



# Multiple mixed effects meta regression and model selection ---------------------------------------
#
# glmulti scans through candidate models to calculate AICc for each model (find combination of variates that best explains the data)
# fit function included so this is a meta regression
# NOTE - takes several hours to run

# OUR DATA
##
#
# determine the number of reasonable candidate models for glmulti to run through using method "d"
candidateset <- glmulti(rz ~ kingdom + realm + longitude_range + longitude_midpoint + I(longitude_midpoint^2) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat, level = 1, method = "d", fitfunction = rma.glmulti, crit = "aicc")
# candidateset <- glmulti(rz ~ kingdom + realm + longitude_range + sin((longitude_midpoint + 180) * (pi/180)) + cos((longitude_midpoint + 180) * (pi/180)) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat, level = 1, method = "d", fitfunction = rma.glmulti, crit = "aicc")
# run glmulti with meta regression function
models <- glmulti(rz ~ kingdom + realm + longitude_range + longitude_midpoint + I(longitude_midpoint^2) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = candidateset)
# models <- glmulti(rz ~ kingdom + realm + longitude_range + sin((longitude_midpoint + 180) * (pi/180)) + cos((longitude_midpoint + 180) * (pi/180)) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = candidateset)
print(models)
# AICc and weights of models within 5 units of the best model
rank.models <- weightable(models)
rank.models <- rank.models[rank.models$aicc <= min(rank.models$aicc) + 2, ]
rank.models


# model averaging
#
# use model weights to make inferences about variables based on all candidate models
# relative importance of all predictors - sum of weights for all models containing that variable
# greater than 0.8 = important variable

# view estimates, variance, importance, and significance of variables using AICc and weight of all candidate models
weights <- round(coef.glmulti(models, select = nrow(rank.models), icmethod = "Burnham"), 7)
weights <- weights[, c(1, 4, 5)]
weights

# conditional R2 GLMM describes proportion of variance in the data explained by both the fixed and random factors in the mixed model (Nakagawa and Schielzeth 2013)
r2c <- c()
for (i in 1:nrow(rank.models)) {
  fixrand <- (models@objects[[i]]$tau2 + sum(models@objects[[i]]$sigma2))  # tau (between groups variance) is from fixed effects and sigma2 is from each random effect
  wi <- weights.rma.mv(models@objects[[i]])  # inverse of the sampling variance
  s2 <- ((models@objects[[i]]$k - 1) * sum(wi, na.rm = TRUE)) / (sum(wi, na.rm = TRUE)^2  - sum(wi^2, na.rm = TRUE))
  r2c[i] <- fixrand / (fixrand + s2)
}
r2c
mean(r2c) # summarize R2 for all equivalent models
sqrt(var(r2c) / length(r2c))

# save output to file
models.output <- summary(models)
models.output.coef <- round(coef(models), 4)
models.output.weights <- weights
write.csv(models.output, file = "models.csv")
write.csv(models.output.coef, file = "models_coef.csv")
write.csv(models.output.weights, file = "models_weights.csv")


# Meta regression: Terrestrial

# model selection
ter.candidateset <- glmulti(rz ~ kingdom + factor(trophic_position) + factor(habitat) + longitude_range + longitude_midpoint + I(longitude_midpoint^2) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.ter, level = 1, method = "d", fitfunction = rma.glmulti)
ter.models <- glmulti(rz ~ kingdom + factor(trophic_position) + factor(habitat) + longitude_range + longitude_midpoint + I(longitude_midpoint^2) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.ter, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = ter.candidateset)
# ter.candidateset <- glmulti(rz ~ kingdom + factor(trophic_position) + factor(habitat) + longitude_range + sin((longitude_midpoint + 180) * (pi/180)) + cos((longitude_midpoint + 180) * (pi/180)) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.ter, level = 1, method = "d", fitfunction = rma.glmulti)
# ter.models <- glmulti(rz ~ kingdom + factor(trophic_position) + factor(habitat) + longitude_range + sin((longitude_midpoint + 180) * (pi/180)) + cos((longitude_midpoint + 180) * (pi/180)) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.ter, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = ter.candidateset)
print(ter.models)
ter.rank <- weightable(ter.models)
ter.rank <- ter.rank[ter.rank$aicc <= min(ter.rank$aicc) + 2, ]
ter.rank

# model averaging
terweights <- round(coef.glmulti(ter.models, select = nrow(ter.rank), icmethod = "Burnham"), 7)
terweights <- terweights[, c(1, 4, 5)]
terweights

# conditional R2 GLMM
ter.r2c <- c()
for (i in 1:nrow(ter.rank)) {
  fixrand <- (ter.models@objects[[i]]$tau2 + sum(ter.models@objects[[i]]$sigma2))
  wi <- weights.rma.mv(ter.models@objects[[i]])
  s2 <- ((ter.models@objects[[i]]$k - 1) * sum(wi, na.rm = TRUE)) / (sum(wi, na.rm = TRUE)^2  - sum(wi^2, na.rm = TRUE))
  ter.r2c[i] <- fixrand / (fixrand + s2)
}
ter.r2c
mean(ter.r2c)
sqrt(var(ter.r2c) / length(ter.r2c))
# save output
ter.output <- summary(ter.models)
ter.output.coef <- round(coef(ter.models), 4)
ter.output.weights <- terweights
write.csv(ter.output, file = "ter_models.csv")
write.csv(ter.output.coef, file = "ter_coef.csv")
write.csv(ter.output.weights, file = "ter_weights.csv")


# Meta regression: Marine

# model selection
mar.candidateset <- glmulti(rz ~ factor(kingdom) + factor(trophic_position) + factor(habitat) + longitude_range + longitude_midpoint + I(longitude_midpoint^2) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.mar, level = 1, method = "d", fitfunction = rma.glmulti)
mar.models <- glmulti(rz ~ factor(kingdom) + factor(trophic_position) + factor(habitat) + longitude_range + longitude_midpoint + I(longitude_midpoint^2) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.mar, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = mar.candidateset)
# mar.candidateset <- glmulti(rz ~ factor(kingdom) + factor(trophic_position) + factor(habitat) + longitude_range + sin((longitude_midpoint + 180) * (pi/180)) + cos((longitude_midpoint + 180) * (pi/180)) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.mar, level = 1, method = "d", fitfunction = rma.glmulti)
# mar.models <- glmulti(rz ~ factor(kingdom) + factor(trophic_position) + factor(habitat) + longitude_range + sin((longitude_midpoint + 180) * (pi/180)) + cos((longitude_midpoint + 180) * (pi/180)) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.mar, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = mar.candidateset)
print(mar.models)
mar.rank <- weightable(mar.models)
mar.rank <- mar.rank[mar.rank$aicc <= min(mar.rank$aicc) + 2, ]
mar.rank

# model averaging
marweights <- round(coef.glmulti(mar.models, select = nrow(mar.rank), icmethod = "Burnham"), 7)
marweights <- marweights[, c(1, 4, 5)]
marweights

# conditional R2 GLMM
mar.r2c <- c()
for (i in 1:nrow(mar.rank)) {
  fixrand <- (mar.models@objects[[i]]$tau2 + sum(mar.models@objects[[i]]$sigma2))
  wi <- weights.rma.mv(mar.models@objects[[i]])
  s2 <- ((mar.models@objects[[i]]$k - 1) * sum(wi, na.rm = TRUE)) / (sum(wi, na.rm = TRUE)^2  - sum(wi^2, na.rm = TRUE))
  mar.r2c[i] <- fixrand / (fixrand + s2)
}
mar.r2c
mean(mar.r2c)
sqrt(var(mar.r2c) / length(mar.r2c))
# save output
mar.output <- summary(mar.models)
mar.output.coef <- round(coef(mar.models), 4)
mar.output.weights <- marweights
write.csv(mar.output, file = "mar_models.csv")
write.csv(mar.output.coef, file = "mar_coef.csv")
write.csv(mar.output.weights, file = "mar_weights.csv")


# Meta regression: Animalia

# specify moderators for mixed effects model based on biology
ani.mod <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + organism_group + thermoregulation + dispersal_type + hemisphere + trophic_position + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity + measure_of_richness,  random = rand, data = dat, subset = animalia)

# model selection
ani.candidateset <- glmulti(rz ~ factor(organism_group) + realm + thermoregulation + dispersal_type + trophic_position + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.ani, level = 1, method = "d", fitfunction = rma.glmulti)
ani.models <- glmulti(rz ~ factor(organism_group) + realm + thermoregulation + dispersal_type + trophic_position + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.ani, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = ani.candidateset)
# ani.candidateset <- glmulti(rz ~ factor(organism_group) + realm + thermoregulation + dispersal_type + trophic_position + longitude_range + sin((longitude_midpoint + 180) * (pi/180)) + cos((longitude_midpoint + 180) * (pi/180)) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.ani, level = 1, method = "d", fitfunction = rma.glmulti)
# ani.models <- glmulti(rz ~ factor(organism_group) + realm + thermoregulation + dispersal_type + trophic_position + longitude_range + sin((longitude_midpoint + 180) * (pi/180)) + cos((longitude_midpoint + 180) * (pi/180)) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.ani, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = ani.candidateset)
print(ani.models)
ani.rank <- weightable(ani.models)
ani.rank <- ani.rank[ani.rank$aicc <= min(ani.rank$aicc) + 2, ]
ani.rank

# model averaging
aniweights <- round(coef.glmulti(ani.models, select = nrow(ani.rank), icmethod = "Burnham"), 7)
aniweights <- aniweights[, c(1, 4, 5)]
aniweights

 # conditional R2 GLMM
ani.r2c <- c()
for (i in 1:nrow(ani.rank)) {
  fixrand <- (ani.models@objects[[i]]$tau2 + sum(ani.models@objects[[i]]$sigma2))
  wi <- weights.rma.mv(ani.models@objects[[i]])
  s2 <- ((ani.models@objects[[i]]$k - 1) * sum(wi)) / (sum(wi)^2  - sum(wi^2))
  ani.r2c[i] <- fixrand / (fixrand + s2)
}
ani.r2c
mean(ani.r2c)
sqrt(var(ani.r2c) / length(ani.r2c))
# save output
ani.output <- summary(ani.models)
ani.output.coef <- round(coef(ani.models), 4)
ani.output.weights <- aniweights
write.csv(ani.output, file = "ani_models.csv")
write.csv(ani.output.coef, file = "ani_coef.csv")
write.csv(ani.output.weights, file = "ani_weights.csv")

# Meta regression: Plantae
# specify moderators for mixed effects model based on biology
pla.mod <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + organism_group + hemisphere + habitat + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity + measure_of_richness,  random = rand, data = dat, subset = plantae)

# model selection
pla.candidateset <- glmulti(rz ~ factor(organism_group) + factor(habitat) + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.pla, level = 1, method = "d", fitfunction = rma.glmulti, crit = "aicc")
pla.models <- glmulti(rz ~ factor(organism_group) + factor(habitat) + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.pla, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = pla.candidateset)
# pla.candidateset <- glmulti(rz ~ factor(organism_group) + factor(habitat) + longitude_range + sin((longitude_midpoint + 180) * (pi/180)) + cos((longitude_midpoint + 180) * (pi/180)) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.pla, level = 1, method = "d", fitfunction = rma.glmulti, crit = "aicc")
# pla.models <- glmulti(rz ~ factor(organism_group) + factor(habitat) + longitude_range + sin((longitude_midpoint + 180) * (pi/180)) + cos((longitude_midpoint + 180) * (pi/180)) + latitude_range + latitude_midpoint + diversity + measure_of_richness, V = "VarRz", random = "rand", data = dat.pla, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = pla.candidateset)
print(pla.models)
pla.rank <- weightable(pla.models)
pla.rank <- pla.rank[pla.rank$aicc <= min(pla.rank$aicc) + 2, ]
pla.rank

# model averaging
plaweights <- round(coef.glmulti(pla.models, select = nrow(pla.rank), icmethod = "Burnham"), 7)
plaweights <- plaweights[, c(1, 4, 5)]
plaweights

# conditional R2 GLMM
pla.r2c <- c()
for (i in 1:nrow(pla.rank)) {
  fixrand <- (pla.models@objects[[i]]$tau2 + sum(pla.models@objects[[i]]$sigma2))
  wi <- weights.rma.mv(pla.models@objects[[i]])
  s2 <- ((pla.models@objects[[i]]$k - 1) * sum(wi)) / (sum(wi)^2  - sum(wi^2))
  pla.r2c[i] <- fixrand / (fixrand + s2)
}
pla.r2c
mean(pla.r2c)
sqrt(var(pla.r2c) / length(pla.r2c))
# save output
pla.output <- summary(pla.models)
pla.output.coef <- round(coef(pla.models), 4)
pla.output.weights <- plaweights
write.csv(pla.output, file = "pla_models.csv")
write.csv(pla.output.coef, file = "pla_coef.csv")
write.csv(pla.output.weights, file = "pla_weights.csv")


# HILLEBRAND'S DATA
##
#
# multiple mixed effects meta regression and model selection
# determine the number of reasonable candidate models for glmulti to run through using method "d"
candidateset <- glmulti(Rz ~ organism_group + trophic_position + log_bodyweight + Log_Sglob + realm + longitude + hemisphere + measured + grain + range, V = "VarRz", random = "rand", data = Hil, level = 1, method = "d", fitfunction = rma.glmulti, crit = "aicc")
# run glmulti with meta regression function
H <- glmulti(Rz ~ organism_group + trophic_position + log_bodyweight + Log_Sglob + realm + longitude + hemisphere + measured + grain + range, V = "VarRz", random = "rand", data = Hil, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize = candidateset)
print(H)
# AICc and weights of models within 5 units of the best model
rank.H <- weightable(H)
rank.H <- rank.H[rank.H$aicc <= min(rank.H$aicc) + 2, ]
rank.H


# model averaging
#
# use model weights to make inferences about variables based on all candidate models
# relative importance of all predictors - sum of weights for all models containing that variable
# greater than 0.8 = important variable

# view estimates, variance, importance, and significance of variables using AICc and weight of all candidate models
H.weights <- round(coef.glmulti(H, select = nrow(rank.H), icmethod = "Burnham"), 7)
H.weights

# conditional R2 GLMM describes proportion of variance in the data explained by both the fixed and random factors in the mixed model (Nakagawa and Schielzeth 2013)
r2c <- c()
for (i in 1:nrow(rank.H)) {
  fixrand <- (H@objects[[i]]$tau2 + sum(H@objects[[i]]$sigma2))  # tau (between groups variance) is from fixed effects and sigma2 is from each random effect
  wi <- weights.rma.mv(H@objects[[i]])  # inverse of the sampling variance
  s2 <- ((H@objects[[i]]$k - 1) * sum(wi, na.rm = TRUE)) / (sum(wi, na.rm = TRUE)^2  - sum(wi^2, na.rm = TRUE))
  r2c[i] <- fixrand / (fixrand + s2)
}
r2c
mean(r2c) # summarize R2 for all equivalent models
sqrt(var(r2c) / length(r2c))

# save output to file
H.output <- summary(H)
H.output.coef <- round(coef(H), 4)
H.output.weights <- H.weights
write.csv(H.output, file = "H_models.csv")
write.csv(H.output.coef, file = "H_coef.csv")
write.csv(H.output.weights, file = "H_weights.csv")





