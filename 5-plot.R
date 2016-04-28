# DESCRIPTIVE STATS AND FIGURES
##
#


# Load and clean Hillebrand's data to be used in figures ---------------------------------------
#
Hillebrand <- gs_title("Hillebrand.csv") #load data from Hillebrand (2004)
Hil <- gs_read_csv(Hillebrand, ws = "Hillebrand.csv", col_names = TRUE)
Hil <- as.data.frame(Hil)

# change character (categorical variates) to factor
for (i in 1:ncol(Hil)) {
  if (class(Hil[, i]) == "character") {
    Hil[, i] <- as.factor(Hil[, i])
  }
}
str(Hil)

# moderator analysis of Hillebrand's covariates to compare with ours
H.orggrp <- rma(yi = Rz, vi = VarRz, mods = ~ -1 + organism_group, data = Hil)
H.therm <- rma(yi = Rz, vi = VarRz, mods = ~ -1 + thermoregulation, data = Hil)
H.realm <- rma(yi = Rz, vi = VarRz, mods = ~ -1 + realm, data = Hil)
H.hab <- rma(yi = Rz, vi = VarRz, mods = ~ -1 + habitat, data = Hil)
H.hemi <- rma(yi = Rz, vi = VarRz, mods = ~ -1 + hemisphere, data = Hil)
H.scale <- rma(yi = Rz, vi = VarRz, mods = ~ -1 + scale, data = Hil)
H.div <- rma(yi = Rz, vi = VarRz, mods = ~ -1 + diversity, data = Hil)
H.troph <- rma(yi = Rz, vi = VarRz, mods = ~ -1 + trophic_position, data = Hil)

H.table <- data.frame(variate = c(rep(x = "organism_group", length(H.orggrp$b)), rep(x = "thermoregulation", length(H.therm$b)), rep(x = "realm", length(H.realm$b)), rep(x = "habitat", length(H.hab$b)), rep(x = "hemisphere", length(H.hemi$b)), rep(x = "scale", length(H.scale$b)), rep(x = "diversity", length(H.div$b)), rep(x = "trophic_position", length(H.troph$b))),
                      rz = c(H.orggrp$b, H.therm$b, H.realm$b, H.hab$b, H.hemi$b, H.scale$b, H.div$b, H.troph$b), 
                      CI.LB = c(H.orggrp$ci.lb, H.therm$ci.lb, H.realm$ci.lb, H.hab$ci.lb, H.hemi$ci.lb, H.scale$ci.lb, H.div$ci.lb, H.troph$ci.lb), 
                      CI.UB = c(H.orggrp$ci.ub, H.therm$ci.ub, H.realm$ci.ub, H.hab$ci.ub, H.hemi$ci.ub, H.scale$ci.ub, H.div$ci.ub, H.troph$ci.ub),
                      lab = c(levels(Hil$organism_group), levels(Hil$therm), levels(Hil$realm), levels(Hil$hab), levels(Hil$hemi), levels(Hil$scale), levels(Hil$div), levels(Hil$troph)),
                      N = c(summary(Hil$organism_group[!is.na(subset.orgrp$organism_group)]), summary(Hil$therm[!is.na(Hil$therm)]), 
                            summary(Hil$realm[!is.na(Hil$realm)]), summary(Hil$hab[!is.na(Hil$hab)]), summary(Hil$hemi[!is.na(Hil$hemi)]), summary(Hil$scale[!is.na(Hil$scale)]), 
                            summary(Hil$diversity[!is.na(Hil$diversity)]), summary(Hil$trophic_position[!is.na(Hil$trophic_position)])))

# Visualize covariates with descriptive stats ---------------------------------------
#
# plot histograms of all continuous/integer variables
for(i in 2:ncol(dat)) {
  if(class(dat[, i]) == "numeric" | class(dat[, i]) == "integer"){
    p <- ggplot(data = dat) + geom_histogram(aes(x = dat[, i])) + labs(title = names(dat)[i], x = names(dat)[i]) + theme_bw()
    print(p)
    ggsave(plot = p, file = paste("num_plot", i, ".pdf", sep=""))
    }
}

# plot bar charts of all non-numerical variables
for(i in 1:ncol(dat)) {
  if(class(dat[, i]) == "factor") {
    q <- ggplot(data = dat) + geom_bar(aes(x = dat[, i])) + labs(title = names(dat)[i], x = names(dat)[i]) + theme_bw()
    print(q)
    ggsave(plot = q, file = paste("cat_plot", i, ".pdf", sep=""))
  }
}  

# the residuals on slope are highly non-normal and slope cannot be used as an effect size
# however, descriptive stats of slope are useful
slope <- ggplot(data = dat, aes(x = slope_b)) + geom_density(fill = "gray60", color = "gray30") + theme_classic() + xlab("Slope") + ylab("Density")
print(slope)
ggsave(plot = slope, file = paste("slope_dens.pdf"))

s <- which(dat$SE_b > 100 | is.na(dat$slope_b))
subset.slope <- dat[-s, ]
slope_latmid <- ggplot(data = subset.slope, aes(x = latitude_midpoint, y = slope_b))  + geom_point() + geom_errorbar(aes(ymax = slope_b + SE_b, ymin = slope_b - SE_b, width = 0.1)) + 
  scale_x_continuous(breaks = seq(-90, 90, 20), limits = c(-90, 90)) + ylab("Slope") + xlab("Latitude midpoint") + theme_classic()
print(slope_latmid)
ggsave(plot = slope_latmid, file = paste("slope_latmid.pdf"))

slope_latrange <- ggplot(data = subset.slope, aes(x = latitude_range, y = slope_b))  + geom_point() + geom_errorbar(aes(ymax = slope_b + SE_b, ymin = slope_b - SE_b, width = 0.1)) + 
  scale_x_continuous(breaks = seq(0, 180, 20), limits = c(0, 180)) + ylab("Slope") + xlab("Latitude range") + theme_classic()
print(slope_latrange)
ggsave(plot = slope_latrange, file = paste("slope_latrange.pdf"))

slope_longmid <- ggplot(data = subset.slope, aes(x = longitude_midpoint, y = slope_b))  + geom_point() + geom_errorbar(aes(ymax = slope_b + SE_b, ymin = slope_b - SE_b, width = 0.1)) + 
  scale_x_continuous(breaks = seq(-180, 180, 20), limits = c(-180, 180)) + ylab("Slope") + xlab("Longitude midpoint") + theme_classic()
print(slope_longmid)
ggsave(plot = slope_longmid, file = paste("slope_longmid.pdf"))

slope.org <- ggplot(data = subset.slope[!is.na(subset.slope$organism_group), ], aes(x = organism_group, y = slope_b)) + geom_boxplot() + scale_x_discrete(name="") + 
  ylab("Slope") + theme_classic() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
print(slope.org)
ggsave(plot = slope.org, file = paste("slope_org.pdf"))

slope.org.lim <- ggplot(data = subset.slope[!is.na(subset.slope$organism_group), ], aes(x = organism_group, y = slope_b)) + geom_boxplot() + coord_cartesian(ylim = c(-20, 10)) + 
  scale_x_discrete(name="") + ylab("Slope") + theme_classic() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
print(slope.org.lim)
ggsave(plot = slope.org.lim, file = paste("slope_org_lim.pdf"))


# Moderator analysis \ linear metaregression with single covariates ---------------------------------------
#
# plots of mean effect size (slope and rz) grouped by factor with CIs
# generate using rma (random effects model with single moderator and no intercept)

# specific figures with "multiple" or "both" removed where necessary (multiple often contains too much variation)
subset.hemi <- dat[which(dat$hemisphere != "both"), ]
subset.orgrp <- dat[which(dat$organism_group != "multiple"), ]
subset.therm <- dat[which(dat$thermoregulation != "multiple"), ]
subset.hab <- dat[which(dat$habitat != "Multiple"), ]

king <- rma(yi = rz, vi = VarRz, mods = ~ - 1 + kingdom, data = dat)
orggrp <- rma(yi = rz, vi = VarRz, mods = ~ -1 + factor(organism_group), data = subset.orgrp)
therm <- rma(yi = rz, vi = VarRz, mods = ~ -1 + factor(thermoregulation), data = subset.therm)
realm <- rma(yi = rz, vi = VarRz, mods = ~ -1 + realm, data = dat)
hab <- rma(yi = rz, vi = VarRz, mods = ~ -1 + factor(subset.hab$habitat), data = subset.hab)
troph <- rma(yi = rz, vi = VarRz, mods = ~ -1 + trophic_position, data = dat)
hemi <- rma(yi = rz, vi = VarRz, mods = ~ -1 + factor(hemisphere), data = subset.hemi)
div <- rma(yi = rz, vi = VarRz, mods = ~ -1 + diversity, data = dat)
meas <- rma(yi = rz, vi = VarRz, mods = ~ -1 + measure_of_richness, data = dat)
latmid <- rma(yi = rz, vi = VarRz, mods = ~ latitude_midpoint, data = dat)
latrange <- rma(yi = rz, vi = VarRz, mods = ~ latitude_range, data = dat)
longmid <- rma(yi = rz, vi = VarRz, mods = ~ longitude_midpoint, data = dat)

# create a categorization of longitude, split by E and W hemisphere
longcat <- ifelse(dat$longitude_midpoint < 0, "W", "E")
dat$longcat <- longcat
longEW <- rma(yi = rz, vi = VarRz, mods = ~ - 1 + longcat, data = dat)
# new categorization of longitude, split by 3 major regions (Americas, Europe/Africa, and Asia)
dat$region <- NA
americas <- which(dat$longitude_midpoint >= -180 & dat$longitude_midpoint < -20)
eurafr <- which(dat$longitude_midpoint >= -20 & dat$longitude_midpoint < 60)
asia <- which(dat$longitude_midpoint >= 60 & dat$longitude_midpoint < 180)
dat[americas, "region" ] <- "Americas"
dat[eurafr, "region" ] <- "EurAfr"
dat[asia, "region" ] <- "Asia"
longreg <- rma(yi = rz, vi = VarRz, mods = ~ - 1 + region, data = dat)

# split longitude once more, keeps N and S hemisphere separate
dat$subregion <- NA
noam <- which(dat$longitude_midpoint >= -180 & dat$longitude_midpoint < -20 & dat$latitude_midpoint > 0)
soam <- which(dat$longitude_midpoint >= -180 & dat$longitude_midpoint < -20 & dat$latitude_midpoint < 0)
eur <- which(dat$longitude_midpoint >= -20 & dat$longitude_midpoint < 60 & dat$latitude_midpoint > 0)
afr <- which(dat$longitude_midpoint >= -20 & dat$longitude_midpoint < 60 & dat$latitude_midpoint < 0)
noasia <- which(dat$longitude_midpoint >= 60 & dat$longitude_midpoint < 180 & dat$latitude_midpoint > 0)
soasia <- which(dat$longitude_midpoint >= 60 & dat$longitude_midpoint < 180 & dat$latitude_midpoint < 0)
dat[noam, "subregion" ] <- "NA"
dat[soam, "subregion" ] <- "SA"
dat[eur, "subregion" ] <- "Eur"
dat[afr, "subregion" ] <- "Afr"
dat[noasia, "subregion" ] <- "NAsia"
dat[soasia, "subregion" ] <- "SAsia"
longsubreg <- rma(yi = rz, vi = VarRz, mods = ~ - 1 + subregion, data = dat)

# table of all single factor regressions used for plots
tab <- data.frame(variate = c(rep(x = "kingdom", length(king$b)), rep(x = "organism_group", length(orggrp$b)), rep(x = "thermoregulation", length(therm$b)), rep(x = "realm", length(realm$b)), 
                              rep(x = "habitat", length(hab$b)), rep(x = "trophic_position", length(troph$b)), rep(x = "hemisphere", length(hemi$b)), rep(x = "diversity", length(div$b)), rep(x = "measure_of_richness", length(meas$b)), 
                              rep(x = "longitude_midpoint", length(longmid$b)), rep(x = "longEW", length(longEW$b)), rep(x = "longreg", length(longreg$b)), rep(x = "longsubreg", length(longsubreg$b)), 
                              rep(x = "latitude_midpoint", length(latmid$b)), rep(x = "latitude_range", length(latrange$b))),
                  rz = c(king$b, orggrp$b, therm$b, realm$b, hab$b, troph$b, hemi$b, div$b, meas$b, longmid$b, longEW$b, longreg$b, longsubreg$b, latmid$b, latrange$b), 
                  CI.LB = c(king$ci.lb, orggrp$ci.lb, therm$ci.lb, realm$ci.lb, hab$ci.lb, troph$ci.lb, hemi$ci.lb, div$ci.lb, meas$ci.lb, longmid$ci.lb, longEW$ci.lb, longreg$ci.lb, longsubreg$ci.lb, latmid$ci.lb, latrange$ci.lb), 
                  CI.UB = c(king$ci.ub, orggrp$ci.ub, therm$ci.ub, realm$ci.ub, hab$ci.ub, troph$ci.ub, hemi$ci.ub, div$ci.ub, meas$ci.ub, longmid$ci.ub, longEW$ci.ub, longreg$ci.ub, longsubreg$ci.ub, latmid$ci.ub, latrange$ci.ub),
                  lab = c(levels(dat$kingdom), levels(factor(subset.orgrp$organism_group)), levels(factor(subset.therm$thermoregulation)), levels(dat$realm), levels(factor(subset.hab$habitat)), levels(dat$trophic_position), 
                          levels(factor(subset.hemi$hemisphere)), levels(dat$div), levels(dat$measure_of_richness), "longitude_midpoint_int", "longitude_midpoint_slope", levels(factor(dat$longcat)), levels(factor(dat$region)), 
                          levels(factor(dat$subregion)), "latitude_midpoint_int", "latitude_midpoint_slope", "latitude_range_int", "latitude_range_slope"),
                  N = c(summary(dat$kingdom[!is.na(dat$kingdom)]), summary(factor(subset.orgrp$organism_group[!is.na(subset.orgrp$organism_group)])), summary(factor(subset.therm$thermoregulation[!is.na(subset.therm$thermoregulation)])), 
                        summary(dat$realm[!is.na(dat$realm)]), summary(factor(subset.hab$habitat[!is.na(subset.hab$habitat)])), summary(dat$trophic_position[!is.na(dat$trophic_position)]), summary(factor(subset.hemi$hemisphere[!is.na(subset.hemi$hemisphere)])), 
                        summary(dat$diversity[!is.na(dat$diversity)]), summary(dat$measure_of_richness[!is.na(dat$measure_of_richness)]), rep(sum(!is.na(dat$longitude_midpoint)), 2), summary(factor(dat$longcat[!is.na(dat$longcat)])), 
                        summary(factor(dat$region[!is.na(dat$region)])), summary(factor(dat$subregion[!is.na(dat$subregion)])), rep(sum(!is.na(dat$latitude_midpoint)), 2), rep(sum(!is.na(dat$latitude_range)), 2)))

# cleaning necessary for figure generation
# for organism group
org.tab <- subset(tab, variate == "kingdom" | variate == "organism_group")
org.tab$lab <- as.character(org.tab$lab)
org.tab$lab[1] <- "all animals"
org.tab$lab[4] <- "all plants"
org.tab <- org.tab[-2, ]  # remove bacteria
org.tab <- org.tab[-2, ]  # remove fungi
org.tab <- org.tab[-3, ]  # remove protists
org.tab <- org.tab[, -1]
org.tab
org.levels <- org.tab[c(6, 13, 10, 14, 12, 2, 9, 3, 5, 8, 4, 7, 11, 1), 4]
org.tab$lab <- factor(org.tab$lab, levels = org.levels)
org.tab
# for trophic level
troph.subset <- tab[which(tab$variate == "trophic_position"), ]
troph.subset$lab <- as.character(troph.subset$lab)
troph.levels <- troph.subset[order(troph.subset$rz), 5]
# create data frame with habitat values that match Hillebrand and our data
hab.subset <- tab[which(tab$variate == "habitat" | tab$variate == "realm"), ]
SB <- hab.subset[which(hab.subset$lab != "Coral reefs" & hab.subset$lab != "Terrestrial other" & hab.subset$lab != "Terrestrial general" & hab.subset$lab != "Freshwater"), ]
hab.subset$lab <- as.character(hab.subset$lab)
hab.subset$lab[2] <- "All marine"
hab.subset$lab[1] <- "All terrestrial"
hab.subset$lab[3] <- "Freshwater"
hab.subset <- hab.subset[-3, ]
H <- H.table[which(H.table$variate == "realm" | H.table$lab == "coastal"), ]
H$lab <- as.character(H$lab)
SB$lab <- as.character(SB$lab)
H$lab[1] <- "Freshwater (all)"
H$lab[2] <- "Marine (all)"
H$lab[3] <- "Terrestrial (all)"
H$lab[4] <- "Coastal/estuary"
H <- H[, -6]
H <- H[, -1]
SB$lab[2] <- "Marine (all)"
SB$lab[1] <- "Terrestrial (all)"
SB$lab[3] <- "Freshwater (all)"
SB <- SB[, -6]
SB <- SB[, -1]
# add in values from the published version of Hillebrand extracted using ImageJ on Figure 5
H <- rbind(H, c(-1.476, -1.698, -1.254, "Open ocean"), c(-1.254, -1.577, -0.931, "Benthic"), c(-0.579, -1.083, -0.075, "Forest"))
H <- H[c(5, 6, 4, 2, 1, 7, 3), ]
SB <- SB[c(5, 6, 4, 2, 3, 7, 1), ]
hab.tab <- data.frame(H, SB)
hab.tab$rz <- as.numeric(hab.tab$rz)
hab.tab$CI.LB <- as.numeric(hab.tab$CI.LB)
hab.tab$CI.UB <- as.numeric(hab.tab$CI.UB)
hab.tab

# create plots with mean effect size (rz) grouped by moderator variates
div.plot <- ggplot(data = subset(tab, variate == "diversity"), aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + 
  geom_text(aes(y = -1.0, label = paste("n = ", N, sep = "")), size = 3) + scale_y_continuous(breaks = seq(-1.0, 0.2, 0.2)) + scale_x_discrete(name="") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.0, 0.2)) + theme_classic()
print(div.plot)
ggsave(plot = div.plot, file = paste("mean_ci_rz_div.pdf"))

hemi.plot <- ggplot(data = subset(tab, variate == "hemisphere"), aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + 
  geom_text(aes(y = -1.0, label = paste("n = ", N, sep = "")), size = 3) + scale_y_continuous(breaks = seq(-1.0, 0.2, 0.2)) + scale_x_discrete(name="") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.0, 0.2)) + theme_classic()
print(hemi.plot)
ggsave(plot = hemi.plot, file = paste("mean_ci_rz_hemi.pdf"))

longEW.plot <- ggplot(data = subset(tab, variate == "longEW"), aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + 
  geom_text(aes(y = -1.0, label = paste("n = ", N, sep = "")), size = 3) + scale_y_continuous(breaks = seq(-1.0, 0.2, 0.2)) + scale_x_discrete(name="") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.0, 0.2)) + theme_classic()
print(longEW.plot)
ggsave(plot = longEW.plot, file = paste("mean_ci_rz_longEW.pdf"))

longreg.plot <- ggplot(data = subset(tab, variate == "longreg"), aes(x = factor(lab, levels = c("Americas", "EurAfr", "Asia")), y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + 
  geom_text(aes(y = -1.0, label = paste("n = ", N, sep = "")), size = 3) + scale_y_continuous(breaks = seq(-1.0, 0.2, 0.2)) + scale_x_discrete(name="") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.0, 0.2)) + theme_classic()
print(longreg.plot)
ggsave(plot = longreg.plot, file = paste("mean_ci_rz_longreg.pdf"))

longsubreg.plot <- ggplot(data = subset(tab, variate == "longsubreg"), aes(x = factor(lab, levels = c("NA", "SA", "Eur", "Afr", "NAsia", "SAsia")), y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + 
  geom_text(aes(y = -1.0, label = paste("n = ", N, sep = "")), size = 3) + scale_y_continuous(breaks = seq(-1.0, 0.2, 0.2)) + scale_x_discrete(name="") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.0, 0.2)) + theme_classic()
print(longsubreg.plot)
ggsave(plot = longsubreg.plot, file = paste("mean_ci_rz_longsubreg.pdf"))

meas.plot <- ggplot(data = subset(tab, variate == "measure_of_richness"), aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + 
  geom_text(aes(y = -1.0, label = paste("n = ", N, sep = "")), size = 3)  + scale_y_continuous(breaks = seq(-1.0, 0.2, 0.2)) + scale_x_discrete(name="") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.0, 0.2)) + theme_classic()
print(meas.plot)
ggsave(plot = meas.plot, file = paste("mean_ci_rz_meas.pdf"))

therm.plot <- ggplot(data = subset(tab, variate == "thermoregulation"), aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + 
  geom_text(aes(y = -1.0, label = paste("n = ", N, sep = "")), size = 3)  + scale_y_continuous(breaks = seq(-1.0, 0.2, 0.2)) + scale_x_discrete(name="") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.0, 0.2)) + theme_classic()
print(therm.plot)
ggsave(plot = therm.plot, file = paste("mean_ci_rz_therm.pdf"))

troph.plot <- ggplot(data = subset(tab, variate == "trophic_position"), aes(x = factor(lab, levels = troph.levels), y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + 
  geom_text(aes(y = -2.0, label = paste("n = ", N, sep = "")), size = 3)  + scale_x_discrete(name="") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-2.0, 0.5)) + 
  theme_classic() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
print(troph.plot)
ggsave(plot = troph.plot, file = paste("mean_ci_rz_troph.pdf"))

# Contains kingdom
org.plot <- ggplot(data = org.tab, aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + geom_text(aes(y = -2.0, label = paste("n = ", N, sep = "")), size = 2) + 
  scale_x_discrete(name="") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-2.0, 0.5)) + theme_classic() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
print(org.plot)
ggsave(plot = org.plot, file = paste("mean_ci_rz_org.pdf"))

# Contains realm
hab.plot <- ggplot(data = hab.subset, aes(x = factor(lab, levels = c("Open ocean", "Benthic", "Coral reefs", "Coastal/estuary", "All marine", "Freshwater", "Forest", "Terrestrial general", "Terrestrial other", "All terrestrial")), y = rz)) + 
  geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + geom_text(aes(y = -2.0, label = paste("n = ", N, sep = "")), size = 3)  + scale_x_discrete(name="") + ylab("Effect size (z)") + 
  coord_cartesian(ylim = c(-2.0, 0.5)) + theme_classic() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
print(hab.plot)
ggsave(plot = hab.plot, file = paste("mean_ci_rz_hab.pdf"))

# Contains realm
habcomp.plot <- ggplot(data = hab.tab, aes(x = rz, y = rz.1)) + geom_point() + geom_errorbar(aes(ymax = CI.UB.1, ymin = CI.LB.1)) + geom_errorbarh(aes(xmax = CI.UB, xmin = CI.LB), color = "gray40") +
  geom_text(aes(label = lab), hjust = -0.4, vjust = 0.15, size = 3) + geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "indianred4") + xlab("Effect size (z) from Hillebrand (2004)") + 
  ylab("Effect size (z) from current study") + coord_cartesian(ylim = c(-1.8, 0.2), xlim = c(-1.8, 0.2)) + theme_classic()
print(habcomp.plot)
ggsave(plot = habcomp.plot, file = paste("mean_ci_rz_habcomp.pdf"))

longmid <- rma(yi = rz, vi = VarRz, mods = ~ longitude_midpoint, data = dat)
tab.longmid <- print.list.rma(predict.rma(longmid))
tab.longmid <- data.frame(tab.longmid, longitude = dat$longitude_midpoint[which(!is.na(dat$longitude_midpoint))])
longmid.plot <- ggplot(tab.longmid, aes(x = longitude, y = pred)) + geom_line() + geom_line(aes(x = longitude, y = ci.lb), linetype = "dashed") + 
  geom_line(aes(x = longitude, y = ci.ub), linetype = "dashed") + geom_rug(sides = "b") + ylab("Effect size (z)") + xlab("Longitude midpoint") + 
  scale_x_continuous(breaks = seq(-180, 180, 40), limits = c(-180, 180)) + ylim(c(-1.6, 0.1)) + theme_classic()
print(longmid.plot)
ggsave(plot = longmid.plot, file = paste("mean_ci_rz_longmid.pdf"))

latmid <- rma(yi = rz, vi = VarRz, mods = ~ latitude_midpoint, data = dat)
tab.latmid <- print.list.rma(predict.rma(latmid))
tab.latmid <- data.frame(tab.latmid, latitude = dat$latitude_midpoint[which(!is.na(dat$latitude_midpoint))])
latmid.plot <- ggplot(tab.latmid, aes(x = latitude, y = pred)) + geom_line() + geom_line(aes(x = latitude, y = ci.lb), linetype = "dashed") + 
  geom_line(aes(x = latitude, y = ci.ub), linetype = "dashed") + geom_rug(sides = "b") + ylab("Effect size (z)") + xlab("Latitude midpoint") + 
  scale_x_continuous(breaks = seq(-90, 90, 20), limits = c(-90, 90)) + ylim(c(-1.6, 0.1)) + theme_classic()
print(latmid.plot)
ggsave(plot = latmid.plot, file = paste("mean_ci_rz_latmid.pdf"))

latrange <- rma(yi = rz, vi = VarRz, mods = ~ latitude_range, data = dat)
tab.latrange <- print.list.rma(predict.rma(latrange))
tab.latrange <- data.frame(tab.latrange, latitude = dat$latitude_range[which(!is.na(dat$latitude_range))])
latrange.plot <- ggplot(tab.latrange, aes(x = latitude, y = pred)) + geom_line() + geom_line(aes(x = latitude, y = ci.lb), linetype = "dashed") + 
  geom_line(aes(x = latitude, y = ci.ub), linetype = "dashed") + geom_rug(sides = "b") + ylab("Effect size (z)") + xlab("Latitude range") + 
  scale_x_continuous(breaks = seq(0, 180, 20), limits = c(0, 180)) + ylim(c(-1.6, 0.1)) + theme_classic()
print(latrange.plot)
ggsave(plot = latrange.plot, file = paste("mean_ci_rz_latrange.pdf"))

# model averaging figures (Importance of covariates)
imp <- ggplot(all.weights, aes(factor(Covariates, levels = vars), Importance)) + 
  geom_bar(stat = "identity") + labs(x = "", y = "Importance") + coord_flip() + theme_classic()
print(imp)
ggsave(plot = imp, file = paste("imp.pdf"))

ani.imp <- ggplot(ani.weights, aes(factor(Covariates, levels = ani.vars), Importance)) + 
  geom_bar(stat = "identity") + labs(x = "", y = "Importance") + coord_flip() + theme_classic()
print(ani.imp)
ggsave(plot = ani.imp, file = paste("ani_imp.pdf"))

pla.imp <- ggplot(pla.weights, aes(factor(Covariates, levels = pla.vars), Importance)) + 
  geom_bar(stat = "identity") + labs(x = "", y = "Importance") + coord_flip() + theme_classic()
print(pla.imp)
ggsave(plot = pla.imp, file = paste("pla_imp.pdf"))

# violin plots of the distribution of habitats in all latitude and longitude midpoints
hab.lat <- ggplot(dat[which(dat$habitat != "Multiple"), ], aes(x = factor(habitat, levels = c("Open ocean", "Benthic", "Coral reefs", "Coastal/estuary", "Freshwater", "Forest", "Terrestrial general", "Terrestrial other")), y = latitude_midpoint)) + 
  geom_violin(fill = "gray60", color = "gray30") + scale_x_discrete(name="") + ylab("Latitude midpoint") + theme_classic() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
print(hab.lat)
ggsave(plot = hab.lat, file = paste("hab_lat.pdf"))

hab.long <- ggplot(dat[which(dat$habitat != "Multiple"), ], aes(x = factor(habitat, levels = c("Open ocean", "Benthic", "Coral reefs", "Coastal/estuary", "Freshwater", "Forest", "Terrestrial general", "Terrestrial other")), y = longitude_midpoint)) + 
  geom_violin(fill = "gray60", color = "gray30") + scale_x_discrete(name="") + ylab("Longitude midpoint") + theme_classic() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
print(hab.long)
ggsave(plot = hab.long, file = paste("hab_long.pdf"))



