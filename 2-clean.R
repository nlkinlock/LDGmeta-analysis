# CLEAN
##
#

# Adding/removing columns ---------------------------------------

# remove comment columns, extent, and grain
dat <- dat.init[, -c(1, 7, 10, 16, 18, 21, 22, 25, 26, 27, 28, 41, 42)]
# sum of NAs in each column (i.e. how complete is the column?)
colSums(is.na(dat[]))

# assign unique id for each study and case
dat$id <- factor(paste(sprintf(fmt = "%04d", dat$studyID), sprintf(fmt = "%02d", dat$caseID), sep = "_"))

# new column with the variance of slope
Var_b <- (dat[, "SE_b"] * sqrt(dat[, "number_of_points"]))^2
dat <- data.frame(dat, Var_b)

# Correct rz, code from Chloe Ryu ---------------------------------------
#
dat$r<-NA
dat$corr_rz<-NA

for (i in 1:nrow(dat)){
  # calculate r from r squared
  # if slope b < 0, r is negative
  ifelse(dat$slope_b[i] < 0, dat$r[i] <- -sqrt(dat$r_sq[i]), dat$r[i] <- sqrt(dat$r_sq[i]))
  # calculate rz
  dat$corr_rz[i] <- 0.5 * log((1 + dat$r[i])/(1 - dat$r[i])) #fisher z transformation from r
}

# if slope_b and r is negative, corr_rz should also be negative. Check.
# the product of r and corr_rz should always be positive
check <- c()
for (i in 1:nrow(dat)){
  check[i] <- dat$r[i] * dat$corr_rz[i]
}
unique(check > 0) # there should only be TRUE

# if correct, replace the rz column in the original dataset with corr.rz 
# remove rz
dat$rz <- dat$corr_rz
dat$corr_rz <- NULL
dat$r_sq <- NULL
#
#

# Combining and removing categories ---------------------------------------
#
# Calculate VarRz for missing studies
indices <- is.na(dat$VarRz)
dat$VarRz[indices] <- 1/(dat$number_of_points[indices] - 3)

# if n = 3, VarRz = Inf
# Remove these values
var.3 <- which(dat$number_of_points == 3)
dat <- dat[-var.3, ]

# remove studies with no rz (i.e. missing number of points, etc.)
rz.check <- which(is.na(dat$rz))
dat <- dat[-rz.check, ]

# measure of richness should either be S (raw richness) or an index
index <- which(dat$measure_of_richness != "S" & dat$measure_of_richness != "index")
dat[index, "measure_of_richness"] <- "index"

# habitat category very unequal, need to group some together. Herb/shrub/sav and wetlands each only have two studies.
herb <- which(dat$habitat == "Herb/shrub/sav")
dat[herb, "habitat"] <- "Terrestrial general"  # herbaceous habitat, shrubland, savannah can broadly be considered terrestrial

wetlands <- which(dat$habitat == "Wetlands")
dat[wetlands, "habitat"] <- "Freshwater"  # wetlands are only freshwater (estuaries grouped separately)

# "ranges" was a category Hillebrand (2004) used to split measures of gamma diversity. In recent papers, this measure seems to be less popular. As it stands, this will not work well in regression.
# studies that used ranges were aiming to calculate gamma diversity (overlap of ranges in a block)
ranges <- which(dat$diversity == "ranges")
dat[ranges, "diversity"] <- "gamma"

# remove studies with multiple kingdoms
multikingdom <- which(dat$kingdom == "multiple")
dat <- dat[-multikingdom, ]

# remove studies with too few species (less than 10)
species <- which(dat$measure_of_richness != "index" & dat$total_taxonomic_richness < 10)
dat <- dat[-species, ]

# looking at the distribution among resolutions, species greatly outnumbers all others.
# remove studies where the resolution is higher than species
higher.res <- which(dat$taxonomic_resolution != "species" & !is.na(dat$taxonomic_resolution))
dat <- dat[-higher.res, ]

# remove beta diversity
# there are too many possible ways to calculate beta diversity and too many disparate indices
m <- which(dat$diversity == "beta")
dat <- dat[-m, ]
dat$diversity <- factor(dat$diversity)


# change character columns to factor
# makes it possible to find correlations among all variables
dat$studyID <- as.factor(dat$studyID)
dat$year <- as.factor(dat$year)
dat$kingdom <- as.factor(dat$kingdom)
dat$organism_group <- factor(dat$organism_group, levels = c("other invertebrates", "herpetofauna", "arthropods", "bacteria", "birds", "fishes", "fungi", "herbaceous plants", "mammals", "multiple", "other plants", "protozoa", "woody plants" ))
dat$thermoregulation <- as.factor(dat$thermoregulation)
dat$dispersal_type <- as.factor(dat$dispersal_type)
dat$trophic_position <- factor(dat$trophic_position, levels = c("multiple", "autotrophs", "carnivores", "detritivores", "herbivores", "omnivores", "parasites", "suspension"))
dat$realm <- factor(dat$realm, levels = c("terrestrial", "marine", "freshwater"))
dat$habitat <- factor(dat$habitat, levels = c("Coastal/estuary", "Coral reefs", "Open ocean", "Benthic", "Freshwater", "Forest", "Terrestrial general", "Terrestrial other", "Multiple"))
dat$hemisphere <- factor(dat$hemisphere, levels = c("N", "S", "both"))
dat$measure_of_richness <- factor(dat$measure_of_richness, levels = c("S", "index"))
dat$taxonomic_resolution <- as.factor(dat$taxonomic_resolution)
dat$scale <- as.factor(dat$scale)
dat$diversity <- as.factor(dat$diversity)
str(dat)

# save cleaned data frame
# write.csv(dat, file = "cleaned_data_complete.csv")
# write.csv(dat, file = "full_data_complete.csv")



