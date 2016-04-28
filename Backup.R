library(devtools)
library(googlesheets)
library(metafor)
library(ggplot2)
library(glmulti)
library(polycor)

# Loading data and clean up data frame ---------------------------------------

gs_ls() # load data sheet directly from Google Spreadsheet
LDG_data <- gs_title("Coding Datasheet")  # load correct sheet within spreadsheet
dat <- gs_read(LDG_data, ws = "IncludedStudies")
dat <- as.data.frame(dat)  #coerce to data frame (ggplot only works with df)
str(dat)  # check type of data (numerical, integer, character) for each variable

# remove comment columns, extent, and grain
dat <- dat[, -c(1, 4, 7, 10, 16, 18, 21, 22, 25, 26, 27, 28, 41, 42, 43)]

# assign unique id for each study and case
dat$id <- factor(paste(sprintf(fmt = "%04d", dat$studyID), sprintf(fmt = "%02d", dat$caseID), sep = "_"))

# look for duplicates in the unique id
dat[which(duplicated(dat$id)), ]

# if n = 3, VarRz = Inf
# Set VarRz to 1 (looks like the asymptote)
a <- which(dat$number.of.points == 3)
dat[a, "VarRz"] <- 1

# measure of richness is either S or an index
# not a verified change (don't change in table yet)
b <- which(dat$measure.of.richness != "S" & dat$measure.of.richness != "index")
dat[b, "measure.of.richness"] <- "index"


# change character columns to factor
# makes it possible to find correlations among all variables
dat$year <- as.factor(dat$year)
dat$kingdom <- as.factor(dat$kingdom)
dat$organism.group <- as.factor(dat$organism.group)
dat$thermoregulation <- as.factor(dat$thermoregulation)
dat$dispersal.type <- as.factor(dat$dispersal.type)
dat$trophic.position <- as.factor(dat$trophic.position)
dat$realm <- as.factor(dat$realm)
dat$habitat <- as.factor(dat$habitat)
dat$hemisphere <- as.factor(dat$hemisphere)
dat$measure.of.richness <- as.factor(dat$measure.of.richness)
dat$scale <- as.factor(dat$scale)
dat$diversity <- as.factor(dat$diversity)
dat$taxonomic.resolution <- as.factor(dat$taxonomic.resolution)
str(dat)



# Visualize variables with descriptive stats ---------------------------------------

# plot histograms of all continuous/integer variables
for(i in 1:ncol(dat)) {
  if(class(dat[, i]) == "numeric" | class(dat[, i]) == "integer"){
  p <- ggplot(data = dat) + geom_histogram(aes(x = dat[, i])) + xlab(names(dat)[i]) + ggtitle(paste(names(dat)[i])) + theme_bw()
  print(p)
    }
}  

# plot bar charts of all non-numerical variables
for(i in 1:ncol(dat)){
  if(class(dat[, i]) == "factor") {
    p <- ggplot(data = dat) + geom_bar(aes(x = dat[, i])) + xlab(names(dat)[i]) + ggtitle(paste(names(dat)[i])) + theme_bw()
    print(p)
  }
}  

# visualize lat and long midpoints on world map
# size of point = sample size
world <- map_data("world")
worldmap <- ggplot(world, aes(x = long, y = lat)) +
  geom_path(aes(group = group)) + geom_point(data = dat, aes(x = longitude.midpoint, y = latitude.midpoint, size = number.of.points), color="darkslategray") +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) + theme_bw()
worldmap


# Meta analysis ---------------------------------------

# global analysis (random effects)
#
# slope (b)
global.slope <- rma(yi = dat$slope.b, sei = dat$SE..b., slab = dat$id)
global.slope
# correlation coefficient - Fisher's z (rz)
global.rz <- rma(yi = dat$rz, vi = dat$VarRz, slab = dat$id)
global.rz


# functions for using meta-regression within glmulti by W. Veichtbauer
#
# function to incorporate meta regression into glmulti
rma.glmulti <- function(formula, data, ...) {
  rma(as.formula(paste(deparse(formula))), vi, data = data, method = "ML", ...)
}

# get glmulti to handle rma.uni objects (metafor)
setOldClass("rma.uni")
setMethod('getfit', 'rma.uni', function(object, ...) {
  if (object$knha == FALSE) {
    cbind(estimate = coef(object), se = sqrt(diag(vcov(object))), df = 100000)
  } else {
    cbind(estimate = coef(object), se = sqrt(diag(vcov(object))), df = object$k - object$p)
  }
})


# Meta regression ---------------------------------------

# diagnostics of variables
#
# sum of NAs in each column (i.e. how complete is the column?)
colSums(is.na(dat[]))

# right now, most of our continuous variables have too many missing values to use
# once we have more continuous variables I will have to look at scatter plots of each variate plotted agaist effect size (both slope and rz) to check for nonlinearity, unequal variance, and outliers
# also will need to look at a scatter plot and correlation matrix of all variates to look for collinearity
# and finally check variance inflation factor of multiple meta regression to detect collinearity

# are the variables of interest correlated?
pairs(~ longitude.midpoint + longitude.range + latitude.range + latitude.midpoint, data = dat)

# hetcor in the polycor allows for the calculation of r between numeric, polyserial correlations between numeric and ordinal, polychoric correlations between ordinal
dat.subset <- dat[, -c(1, 2, 3, 4, 13, 14, 17, 18, 19, 25)]  #13 and 14 longitude 17 and 18 latitude 19 taxonomic res 25 intercept
correlations <- hetcor(data = dat.subset, std.err = FALSE)
correlations

# take a closer look at correlated variables of interest
# scale and diversity
table(dat$scale, dat$diversity)
chisq.test(x = dat$scale, y = dat$diversity)
# taxonomic resolution and trophic position
table(dat$taxonomic.resolution, dat$trophic.position)
chisq.test(x = dat$taxonomic.resolution, y = dat$trophic.position)

# metaregression: slope
#
# model below with all variables currently will not work because of missing data
# mods.slope <- glmulti(slope.b ~ kingdom + organism.group + thermoregulation +  dispersal.type + trophic.position + realm + habitat + hemisphere + longitude.range + longitude.midpoint + scale + diversity + latitude.range + latitude.midpoint + total.taxonomic.richness + taxonomic.resolution + measure.of.richness, vi = dat$SE..b., data = dat, level = 1, fitfunction = rma.glmulti, crit = "aicc")

# new column with variance of slope
Var.b <- (dat[, "SE..b."] * sqrt(dat[, "number.of.points"]))^2
dat <- data.frame(dat, Var.b)

# determine the number of reasonable candidate models for glmulti to run through using method "d"
mods.slope.d <- glmulti(slope.b ~ kingdom + thermoregulation +  dispersal.type + trophic.position + realm + hemisphere + scale + diversity + latitude.range + taxonomic.resolution + measure.of.richness, data = dat, vi = dat$Var.b, level = 1, method = "d", fitfunction = rma.glmulti, crit = "aicc")

# glmulti scans through candidate models to calculate AICc for each model (find combination of variates that best explains the data)
# fit function included so this is a meta regression
# mods.slope <- glmulti(slope.b ~ kingdom + thermoregulation +  dispersal.type + trophic.position + realm + hemisphere + scale + diversity + latitude.range + taxonomic.resolution + measure.of.richness, data = dat, vi = dat$Var.b, level = 1, method = "g", fitfunction = rma.glmulti, crit = "aicc", confsetsize = 2048)
print(mods.slope)

# look at the AICc and weights of the models with an AICc within 5 units of the best model
# Akaike weight is the probability that the model is the best model, i.e. contains the most information
rank.slope <- weightable(mods.slope)
rank.slope <- rank.slope[rank.slope$aicc <= min(rank.slope$aicc) + 5, ]
rank.slope

# look at summary of the best model
summary(mods.slope@objects[[1]])


# multimodel inference: slope
#
# use model weights to make inferences about variables based on all candidate models
# relative importance of all predictors - sum of weights for all models containing that variable
# greater than 0.8 = important variable
plot(mods.slope, type = "s")

# view estimates, variance, importance, and significance of variables using AICc and weight of all candidate models
round(coef(mods.slope), 4)


# save output to file
output <- capture.output(summary(mods.slope))
output2 <- capture.output(round(coef(mods.slope), 4))
cat("Multiple meta-regression of slope", output, file = "slope.txt", sep = "n", append = TRUE)
cat("Multiple meta-regression of slope", output2, file = "slope2.txt", sep = "n", append = TRUE)



# metaregression: rz
#
# determine the number of reasonable candidate models for glmulti to run through using method "d"
mods.rz.d <- glmulti(rz ~ kingdom + thermoregulation +  dispersal.type + trophic.position + realm + hemisphere + scale + diversity + latitude.range + taxonomic.resolution + measure.of.richness, vi = dat$VarRz, data = dat, level = 1, method = "d", fitfunction = rma.glmulti.2, crit = "aicc")

# run glmulti with meta regression function
# mods.rz <- glmulti(rz ~ kingdom + thermoregulation +  dispersal.type + trophic.position + realm + hemisphere + scale + diversity + latitude.range + taxonomic.resolution + measure.of.richness, vi = dat$VarRz, data = dat, level = 1, method = "g", fitfunction = rma.glmulti.2, crit = "aicc", confsetsize = 2048)
print(mods.rz)

# AICc and weights of models within 5 units of the best model
rank.rz <- weightable(mods.rz)
rank.rz <- rank.rz[rank.rz$aicc <= min(rank.rz$aicc) + 5, ]
rank.rz

# look at summary of the best model
summary(mods.rz@objects[[1]])


# multimodel inference: rz
#
# use model weights to make inferences about variables based on all candidate models
# relative importance of all predictors - sum of weights for all models containing that variable
# greater than 0.8 = important variable
plot(mods.rz, type = "s")

# view estimates, variance, importance, and significance of variables using AICc and weight of all candidate models
round(coef(mods.rz), 4)


# save output to file
output.rz <- capture.output(summary(mods.rz))
output2.rz <- capture.output(round(coef(mods.rz), 4))
cat("Multiple meta-regression of rz", output.rz, file = "rz.txt", sep = "n", append = TRUE)
cat("Multiple meta-regression of rz", output2.rz, file = "rz2.txt", sep = "n", append = TRUE)



