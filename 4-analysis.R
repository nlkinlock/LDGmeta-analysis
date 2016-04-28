# META ANALYSES \ META REGRESSION
##
#


# Meta analysis ---------------------------------------

# global analysis (random effects)
#
# COMPLETE DATASET
##
#

# slope is not an appropriate effect size because of the non-normally distributed residuals
global.slope.init <- rma(yi = slope_b, vi = Var_b, data = dat)
plot(residuals.rma(global.slope.init))
hist(residuals.rma(global.slope.init))

# correlation coefficient - Fisher's z (rz)
global.init <- rma(yi = rz, vi = VarRz, data = dat)
summary(global.init)

global.inf <- influence(global.init)
plot(global.inf)
outliers <- which(global.inf$is.infl == TRUE)
dat[outliers, ]
subset <- dat[-outliers, ]

global <- rma(yi = rz, vi = VarRz, data = subset)
summary(global)

#
# ANIMALIA
##
#
# subset of data with all plants to feed to rma
animalia <- which(dat$kingdom == "Animalia")
dat.ani <- dat[animalia, ]
# grand mean
#
ani <- rma(yi = rz, vi = VarRz, data = dat.ani)
summary(ani)

#
# PLANTAE
##
#
# subset of data with all plants to feed to rma
plantae <- which(dat$kingdom == "Plantae")
dat.pla <- dat[plantae, ]
# grand mean
#
pla <- rma(yi = rz, vi = VarRz, data = dat.pla)
summary(pla)


# Variable diagnostics ---------------------------------------

# are the variables of interest correlated?
# pairs(~ longitude_midpoint + longitude_range + latitude_range + latitude_midpoint, data = dat)

# hetcor in the polycor allows for the calculation of r between numeric, polyserial correlations between numeric and ordinal, polychoric correlations between ordinal
# correlations <- hetcor(data = dat, std.err = FALSE)
# correlations

# take a closer look at correlated variables of interest
# scale and diversity
# table(dat$scale, dat$diversity)
# chisq.test(x = dat$scale, y = dat$diversity)
# taxonomic resolution and measure of richness
# table(dat$habitat, dat$realm)
# chisq.test(x = dat$taxonomic_resolution, y = dat$measure_of_richness)
# measure of richness and diversity
# table(dat$diversity, dat$measure_of_richness)
# chisq.test(x = dat$diversity, y = dat$measure_of_richness)
# realm and habitat
# table(dat$realm, dat$habitat)
# chisq.test(x = dat$realm, y = dat$habitat)


# Meta regression: all kingdoms ---------------------------------------

# metaregression
#
# specify covariates for random effects model based on biology (all relevant covariates)
# with outliers
# list of random effects - test with and without each and compare AICc
rand <- list(~ 1 | id)
rand2 <- list(~ 1 | id, ~ 1 | studyID)
rand3 <- list(~ 1 | id, ~ 1 | studyID, ~ 1 | measure_of_richness)
rand4 <- list(~ 1 | id, ~ 1 | measure_of_richness)
rand5 <- list(~ id | studyID)

int.rand2 <- rma.mv(yi = rz, V = VarRz, random = rand2, data = dat)
full.model <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + organism_group + thermoregulation + dispersal_type + trophic_position + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + hemisphere + diversity, data = dat, random = rand)
mods.a <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, data = dat, random = rand)
mods.a2 <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, random = rand2, data = dat)
mods.a3 <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, random = rand3, data = dat)
mods.a4 <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, random = rand4, data = dat)
paste("full model: ", round(aicc(full.model), digits = 2), "red model rand: ", round(aicc(mods.a), digits = 2), "red model rand2: ", round(aicc(mods.a2), digits = 2), "red model rand3: ", round(aicc(mods.a3), digits = 2), "red model rand4: ", round(aicc(mods.a4), digits = 2))
# rand2 gives lowest aicc
# coefficients for the continuous variables do not differ between intercept/no intercept
summary(full.model)
summary(mods.a)
summary(mods.a2)
summary(mods.a3)
summary(mods.a4)

# without outliers
mods.out <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm  + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, random = rand, data = subset)
mods.out2 <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, random = rand2, data = subset)
mods.out3 <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, random = rand3, data = subset)
mods.out4 <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, random = rand4, data = subset)
summary(mods.out)
summary(mods.out2)
summary(mods.out3)
summary(mods.out4)
paste("model rand w/o outliers: ", round(aicc(mods.out), digits = 2), "model rand2 w/o outliers: ", round(aicc(mods.out2), digits = 2), "model rand3 w/o outliers: ", round(aicc(mods.out3), digits = 2), "model rand4 w/o outliers: ", round(aicc(mods.out4), digits = 2))

# model selection
#
# glmulti scans through candidate models to calculate AICc for each model (find combination of variates that best explains the data)
# fit function included so this is a meta regression
# NOTE - takes several hours to run
# determine the number of reasonable candidate models for glmulti to run through using method "d"
candidateset <- glmulti(rz ~ kingdom + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, V = "VarRz", random = "rand2", data = dat, level = 1, method = "d", fitfunction = rma.glmulti, crit = "aicc")
# run glmulti with meta regression function
models <- glmulti(rz ~ kingdom + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, V = "VarRz", random = "rand2", data = dat, level = 1, method = "g", fitfunction = rma.glmulti, crit = "aicc", confsetsize = candidateset)
print(models)

# AICc and weights of models within 5 units of the best model
rank.models <- weightable(models)
rank.models <- rank.models[rank.models$aicc <= min(rank.models$aicc) + 2, ]
rank.models

# look at summary of the best model with no intercept (categorical variable estimates conditional on all other important covariates being held at mean)
summary(models@objects[[1]])


# model averaging
#
# use model weights to make inferences about variables based on all candidate models
# relative importance of all predictors - sum of weights for all models containing that variable
# greater than 0.8 = important variable

# view estimates, variance, importance, and significance of variables using AICc and weight of all candidate models
weights <- round(coef.glmulti(models, select = nrow(rank.models), icmethod = "Burnham"), 5)
weights <- weights[, c(1, 4, 5)]
weights
all.weights <- weights[c(1, 3, 7, 8, 9, 10, 11, 13), 2]
names(all.weights) <- NULL
vars <- c("Kingdom", "Longitude range", "Latitude range", "Diversity", "Longitude midpoint", "Realm", "Latitude midpoint")
all.weights <- data.frame(Covariates = vars, Importance = all.weights)
all.weights

# conditional R2 GLMM describes proportion of variance in the data explained by both the fixed and random factors in the mixed model (Nakagawa and Schielzeth 2013)
r2c <- c()
for (i in 1:nrow(rank.models)) {
  fixrand <- (models@objects[[i]]$tau2 + sum(models@objects[[i]]$sigma2))  # tau (between groups variance) is from fixed effects and sigma2 is from each random effect
  wi <- weights.rma.mv(models@objects[[i]])  # inverse of the sampling variance
  s2 <- ((models@objects[[i]]$k - 1) * sum(wi)) / (sum(wi)^2  - sum(wi^2))  # typical within study variance of the effect size from Higgins and Thompson (2002)
  r2c[i] <- fixrand / (fixrand + s2)
}
r2c
mean(r2c) # summarize R2 for all equivalent models
sqrt(var(r2c) / length(r2c))

# save output to file
models.output <- capture.output(summary(models))
models.output.coef <- capture.output(modweights)
cat("Multiple meta-regression of rz", models.output, file = "models.txt", sep = "\n", append = TRUE)
cat("Multiple meta-regression coefficients of rz", models.output.coef, file = "models_coef.txt", sep = "\n", append = TRUE)


# Meta regression: Animalia ---------------------------------------

# specify moderators for mixed effects model based on biology
ani.mod <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + organism_group + thermoregulation + dispersal_type + trophic_position + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity,  random = rand2, data = dat, subset = animalia)
ani.mod

# model selection
ani.candidateset <- glmulti(rz ~ organism_group + thermoregulation + dispersal_type + trophic_position + habitat + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, V = "VarRz", random = "rand2", data = dat.ani, level = 1, method = "d", fitfunction = rma.glmulti)
ani.models <- glmulti(rz ~ organism_group + thermoregulation + dispersal_type + trophic_position + habitat + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, V = "VarRz", random = "rand2", data = dat.ani, level = 1, method = "g", fitfunction = rma.glmulti, crit = "aicc", confsetsize = ani.candidateset)
print(ani.models)
ani.rank <- weightable(ani.models)
ani.rank <- ani.rank[ani.rank$aicc <= min(ani.rank$aicc) + 2, ]
ani.rank
summary(ani.models@objects[[1]])

# model averaging
aniweights <- round(coef.glmulti(ani.models, select = nrow(ani.rank), icmethod = "Burnham"), 5)
aniweights <- aniweights[, c(1, 4, 5)]
aniweights
ani.weights <- aniweights[c(1, 2, 3, 5, 10, 12, 20, 22, 23), 2]
names(ani.weights) <- NULL
ani.vars <- c("Thermoregulation", "Longitude range", "Dispersal type", "Organism group", "Longitude midpoint", "Habitat", "Hemisphere", "Latitude range", "Latitude midpoint")
ani.weights <- data.frame(Covariates = ani.vars, Importance = ani.weights)
ani.weights

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
ani.output <- capture.output(summary(ani.models))
ani.output.coef <- capture.output(round(coef(ani.models), 4))
cat("Multiple meta-regression of rz (Animalia)", ani.output, file = "ani_rz.txt", sep = "n", append = TRUE)
cat("Multiple meta-regression coefficients of rz (Animalia)", ani.output.coef, file = "ani_rz_coef.txt", sep = "n", append = TRUE)


# Meta regression: Plantae ---------------------------------------

# specify moderators for mixed effects model based on biology
pla.mod <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + organism_group + habitat + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity,  random = rand2, data = dat, subset = plantae)
summary(pla.mod)

# model selection
pla.candidateset <- glmulti(rz ~ organism_group + habitat + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, V = "VarRz", random = "rand2", data = dat.pla, level = 1, method = "d", fitfunction = rma.glmulti, crit = "aicc")
pla.models <- glmulti(rz ~ organism_group + habitat + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, V = "VarRz", random = "rand2", data = dat.pla, level = 1, method = "g", fitfunction = rma.glmulti, crit = "aicc", confsetsize = pla.candidateset)
print(pla.models)
pla.rank <- weightable(pla.models)
pla.rank <- pla.rank[pla.rank$aicc <= min(pla.rank$aicc) + 2, ]
pla.rank
summary(pla.models@objects[[1]])

# model averaging
plaweights <- round(coef.glmulti(pla.models, select = nrow(pla.rank), icmethod = "Burnham"), 5)
plaweights <- plaweights[, c(1, 4, 5)]
plaweights
pla.weights <- plaweights[c(1, 2, 3, 4, 6, 10), 2]
names(pla.weights) <- NULL
pla.vars <- c("Diversity", "Latitude range", "Longitude range", "Longitude midpoint", "Habitat", "Latitude midpoint")
pla.weights <- data.frame(Covariates = pla.vars, Importance = pla.weights)
pla.weights

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
pla.output <- capture.output(summary(pla.models))
pla.output.coef <- capture.output(round(coef(pla.models), 4))
cat("Multiple meta-regression of rz (Plantae)", pla.output, file = "pla_rz.txt", sep = "n", append = TRUE)
cat("Multiple meta-regression coefficients of rz (Plantae)", pla.output.coef, file = "pla_rz_coef.txt", sep = "n", append = TRUE)







