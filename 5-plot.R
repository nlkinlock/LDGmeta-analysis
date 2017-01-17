# DESCRIPTIVE STATS AND FIGURES
##
#

# Visualize covariates with descriptive stats ---------------------------------------
#
# plot histograms of all continuous/integer variables
for(i in 2:ncol(dat)) {
  if(class(dat[, i]) == "numeric" | class(dat[, i]) == "integer"){
    p <- ggplot(data = dat) + geom_histogram(aes(x = dat[, i])) + labs(title = names(dat)[i], x = names(dat)[i]) + theme_bw()
    print(p)
    ggsave(plot = p, file = paste("num_plot", i, ".jpg", sep=""))
    }
}

# plot bar charts of all non-numerical variables
for(i in 1:ncol(dat)) {
  if(class(dat[, i]) == "factor") {
    q <- ggplot(data = dat) + geom_bar(aes(x = dat[, i])) + labs(title = names(dat)[i], x = names(dat)[i]) + theme_bw()
    print(q)
    ggsave(plot = q, file = paste("cat_plot", i, ".jpg", sep=""))
  }
}  

# the residuals on slope are highly non-normal and slope cannot be used as an effect size
# however, descriptive stats of slope are useful
# distribution
subset.slope <- subset(dat, select = c(slope_b, SE_b, latitude_midpoint, longitude_midpoint, organism_group))
subset.slope$logslope <- log(subset.slope$slope_b)
subset.slope$logse <- log(subset.slope$SE_b)
subset.slope <- subset.slope[-which(is.na(subset.slope$SE_b) | subset.slope$SE_b > 100),]
subset.slope$distance <- abs(subset.slope$slope_b)
subset.slope$overlap <- ifelse((subset.slope$slope_b + subset.slope$SE_b) * (subset.slope$slope_b - subset.slope$SE_b) > 0, 1, 0)
subset.slope$pos <- ifelse(subset.slope$overlap & subset.slope$slope_b > 0, 1, 0)
subset.slope$neg <- ifelse(subset.slope$overlap > 0 & subset.slope$slope_b < 0, 2, 0)
subset.slope$sign <- as.factor(subset.slope$pos + subset.slope$neg)
for (i in 1:nrow(subset.slope)) {
  if (subset.slope$sign[i] == 0) {
  subset.slope$distancese[i] <- ifelse(subset.slope$slope_b[i] > 0, abs(subset.slope$slope_b[i] - subset.slope$SE_b[i]), abs(subset.slope$slope_b[i] + subset.slope$SE_b[i]))
} else {
  subset.slope$distancese[i] <- ifelse(subset.slope$slope_b[i] > 0, -abs(subset.slope$slope_b[i] - subset.slope$SE_b[i]), -abs(subset.slope$slope_b[i] + subset.slope$SE_b[i]))
}
}
slope.se <- arrange(subset.slope, -distancese)
slope.se$index <- c(1:nrow(slope.se))
slope_forest <- ggplot(data = slope.se, aes(x = slope_b, y = index))  + geom_point(aes(colour = sign), size = 0.1) + geom_errorbarh(aes(xmax = slope_b + SE_b, xmin = slope_b - SE_b, width = 0, colour = sign), size = 0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + scale_colour_manual(values = c("gray45", "indianred3", "dodgerblue3"), guide = FALSE) + 
  ylab("") + xlab("Slope") + coord_cartesian(ylim = c(0, 400)) + theme_classic() + theme(text = element_text(size = 10))
print(slope_forest)

SE_rz <- sqrt(dat[, "VarRz"]) / sqrt(dat[, "number_of_points"])
subset.rz <- data.frame(rz = dat$rz, se = SE_rz)
subset.rz$distance <- abs(subset.rz$rz)
subset.rz$overlap <- ifelse((subset.rz$rz + subset.rz$se) * (subset.rz$rz - subset.rz$se) > 0, 1, 0)
subset.rz$pos <- ifelse(subset.rz$overlap & subset.rz$rz > 0, 1, 0)
subset.rz$neg <- ifelse(subset.rz$overlap > 0 & subset.rz$rz < 0, 2, 0)
subset.rz$sign <- as.factor(subset.rz$pos + subset.rz$neg)
for (i in 1:nrow(subset.rz)) {
  if (subset.rz$sign[i] == 0) {
    subset.rz$distancerz[i] <- ifelse(subset.rz$rz[i] > 0, abs(subset.rz$rz[i] - subset.rz$se[i]), abs(subset.rz$rz[i] + subset.rz$se[i]))
  } else {
    subset.rz$distancerz[i] <- ifelse(subset.rz$rz[i] > 0, -abs(subset.rz$rz[i] - subset.rz$se[i]), -abs(subset.rz$rz[i] + subset.rz$se[i]))
  }
}
rz.se <- arrange(subset.rz, -distancerz)
rz.se$index <- c(1:nrow(rz.se))
rz_forest <- ggplot(data = rz.se, aes(x = rz, y = index))  + geom_point(aes(colour = sign), size = 0.1) + geom_errorbarh(aes(xmax = rz + SE_rz, xmin = rz - SE_rz, width = 0, colour = sign), size = 0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + scale_colour_manual(values = c("gray45", "indianred3", "dodgerblue3"), guide = FALSE) + 
  ylab("Case") + xlab("z") + coord_cartesian(ylim = c(0, 400)) + theme_classic() + theme(text = element_text(size = 10), axis.title.x = element_text(face = "italic"))
print(rz_forest)

pdf(file = "fig3.pdf", height = 3.5, width = 6)
multiplot(rz_forest, slope_forest, cols = 2)
dev.off()

# latitude midpoint with SEs
slope_latmid <- ggplot(data = subset.slope, aes(x = latitude_midpoint, y = logslope))  + geom_point() + geom_errorbar(aes(ymax = logslope + logse, ymin = logslope - logse, width = 0.1)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + annotate("text", x = -90, y = 5, label = "a", fontface = "bold") + 
  scale_x_continuous(breaks = seq(-90, 90, 20), limits = c(-90, 90)) + ylab("log Slope") + xlab("Latitude midpoint") + theme_classic() + theme(text = element_text(size = 10)) 
print(slope_latmid)
ggsave(plot = slope_latmid, file = paste("slope_latmid.tiff"))
# longitude midpoint with SEs
slope_longmid <- ggplot(data = subset.slope, aes(x = longitude_midpoint, y = logslope))  + geom_point() + geom_errorbar(aes(ymax = logslope + logse, ymin = logslope - logse, width = 0.1)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + annotate("text", x = -180, y = 5, label = "b", fontface = "bold") + 
  scale_x_continuous(breaks = seq(-180, 180, 40), limits = c(-180, 180)) + ylab("log Slope") + xlab("Longitude midpoint") + theme_classic() + theme(text = element_text(size = 10))
print(slope_longmid)
ggsave(plot = slope_longmid, file = paste("slope_longmid.tiff"))

pdf(file = "figED2.pdf")
multiplot(slope_latmid, slope_longmid, cols = 1)
dev.off()

# slopes grouped by organism, boxplot
slope.org <- ggplot(data = subset.slope[!is.na(subset.slope$organism_group), ], aes(x = organism_group, y = logslope)) + geom_boxplot() + scale_x_discrete(name="") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + ylab("log Slope") + theme_classic() + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 60, hjust = 1)) 
print(slope.org)
ggsave(plot = slope.org, file = paste("slope_org.tiff"))


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

# splitting up longitude
# split longitude into evenly spaced bins
lab.init <- seq(-180, 180, 60)
lab.long <- c()
for (i in 1:(length(lab.init) - 1)) {
lab.long[i] <- paste(lab.init[i], lab.init[i + 1], sep = ",")
}
dat$region <- as.factor(cut(dat$longitude_midpoint, lab.init, labels = FALSE))
longreg <- rma(yi = rz, vi = VarRz, mods = ~ - 1 + region, data = dat)
# terrestrial habitats only
dat.ter$region.ter <- as.factor(cut(dat.ter$longitude_midpoint, lab.init, labels = FALSE))
terrestrial <- rma(yi = rz, vi = VarRz, mods = ~ - 1 + region.ter, data = dat.ter)
# marine habitats only
dat.mar$region.mar <- as.factor(cut(dat.mar$longitude_midpoint, lab.init, labels = FALSE))
marine <- rma(yi = rz, vi = VarRz, mods = ~ - 1 + region.mar, data = dat.mar)
dat.marter <- rbind.fill(dat.ter, dat.mar)

marter <- rma(yi = rz, vi = VarRz, mods = ~ -1 + factor(realm), data = dat.marter)
marter$ci.lb

# table of all single factor regressions used for plots
tab <- data.frame(variate = c(rep(x = "kingdom", length(king$b)), rep(x = "organism_group", length(orggrp$b)), rep(x = "thermoregulation", length(therm$b)), rep(x = "realm", length(realm$b)), 
                              rep(x = "habitat", length(hab$b)), rep(x = "trophic_position", length(troph$b)), rep(x = "hemisphere", length(hemi$b)), rep(x = "diversity", length(div$b)), rep(x = "measure_of_richness", length(meas$b)), 
                              rep(x = "longitude_midpoint", length(longmid$b)), rep(x = "longreg", length(longreg$b)), rep(x = "terrestrial", length(terrestrial$b)), 
                              rep(x = "marine", length(marine$b)), rep(x = "latitude_midpoint", length(latmid$b)), rep(x = "latitude_range", length(latrange$b))),
                  rz = c(king$b, orggrp$b, therm$b, realm$b, hab$b, troph$b, hemi$b, div$b, meas$b, longmid$b, longreg$b, terrestrial$b, marine$b, latmid$b, latrange$b), 
                  CI.LB = c(king$ci.lb, orggrp$ci.lb, therm$ci.lb, realm$ci.lb, hab$ci.lb, troph$ci.lb, hemi$ci.lb, div$ci.lb, meas$ci.lb, longmid$ci.lb, longreg$ci.lb, terrestrial$ci.lb, marine$ci.lb, latmid$ci.lb, latrange$ci.lb), 
                  CI.UB = c(king$ci.ub, orggrp$ci.ub, therm$ci.ub, realm$ci.ub, hab$ci.ub, troph$ci.ub, hemi$ci.ub, div$ci.ub, meas$ci.ub, longmid$ci.ub, longreg$ci.ub, terrestrial$ci.ub, marine$ci.ub, latmid$ci.ub, latrange$ci.ub),
                  lab = c(levels(dat$kingdom), levels(factor(subset.orgrp$organism_group)), levels(factor(subset.therm$thermoregulation)), levels(dat$realm), levels(factor(subset.hab$habitat)), levels(dat$trophic_position), 
                          levels(factor(subset.hemi$hemisphere)), levels(dat$div), levels(dat$measure_of_richness), "longitude_midpoint_int", "longitude_midpoint_slope", lab.long, lab.long, lab.long, "latitude_midpoint_int", 
                          "latitude_midpoint_slope", "latitude_range_int", "latitude_range_slope"),
                  N = c(summary(dat$kingdom[!is.na(dat$kingdom)]), summary(factor(subset.orgrp$organism_group[!is.na(subset.orgrp$organism_group)])), summary(factor(subset.therm$thermoregulation[!is.na(subset.therm$thermoregulation)])), 
                        summary(dat$realm[!is.na(dat$realm)]), summary(factor(subset.hab$habitat[!is.na(subset.hab$habitat)])), summary(dat$trophic_position[!is.na(dat$trophic_position)]), summary(factor(subset.hemi$hemisphere[!is.na(subset.hemi$hemisphere)])), 
                        summary(dat$diversity[!is.na(dat$diversity)]), summary(dat$measure_of_richness[!is.na(dat$measure_of_richness)]), rep(sum(!is.na(dat$longitude_midpoint)), 2), summary(factor(dat$region[!is.na(dat$region)])), 
                        summary(factor(dat.ter$region.ter[!is.na(dat.ter$region.ter)])), summary(factor(dat.mar$region.mar[!is.na(dat.mar$region.mar)])), rep(sum(!is.na(dat$latitude_midpoint)), 2), rep(sum(!is.na(dat$latitude_range)), 2)))

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


# create plots with mean effect size (rz) grouped by moderator variates
div.plot <- ggplot(data = subset(tab, variate == "diversity"), aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) +
  annotate("text", x = 0.5, y = 0.1, label = "a", fontface = "bold") + geom_text(aes(y = -1.0, label = paste("n = ", N, sep = "")), size = 3) + scale_y_continuous(breaks = seq(-1.0, 0.1, 0.2)) + scale_x_discrete(name = "") + 
  ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.0, 0.1)) + theme_classic() + theme(text = element_text(size = 10))
print(div.plot)
ggsave(plot = div.plot, file = paste("mean_ci_rz_div.tiff"))

meas.plot <- ggplot(data = subset(tab, variate == "measure_of_richness"), aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + 
  annotate("text", x = 0.5, y = 0.1, label = "b", fontface = "bold") + geom_text(aes(y = -1.0, label = paste("n = ", N, sep = "")), size = 3)  + scale_y_continuous(breaks = seq(-1.0, 0.1, 0.2)) + scale_x_discrete(name = "") + ylab("Effect size (z)") + 
  coord_cartesian(ylim = c(-1.0, 0.1)) + theme_classic() + theme(text = element_text(size = 10))
print(meas.plot)
ggsave(plot = meas.plot, file = paste("mean_ci_rz_meas.tiff"))

therm.plot <- ggplot(data = subset(tab, variate == "thermoregulation"), aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + 
  annotate("text", x = 0.5, y = 0.1, label = "c", fontface = "bold") + geom_text(aes(y = -1.0, label = paste("n = ", N, sep = "")), size = 3)  + scale_y_continuous(breaks = seq(-1.0, 0.1, 0.2)) + scale_x_discrete(name = "") + ylab("Effect size (z)") + 
  coord_cartesian(ylim = c(-1.0, 0.1)) + theme_classic() + theme(text = element_text(size = 10))
print(therm.plot)
ggsave(plot = therm.plot, file = paste("mean_ci_rz_therm.tiff"))

pdf(file = "figED3.pdf", width = 3.5, height = 6)
multiplot(div.plot, meas.plot, therm.plot, cols = 1)
dev.off()

# Contains realm
hab.plot <- ggplot(data = hab.subset, aes(x = factor(lab, levels = c("Open ocean", "Benthic", "Coral reefs", "Coastal/estuary", "All marine", "Freshwater", "Forest", "Terrestrial general", "Terrestrial other", "All terrestrial")), y = rz)) + 
  geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + geom_text(aes(y = -1.65, label = paste("n = ", N, sep = "")), size = 3) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + 
  annotate("text", x = 0.7, y = 0.5, label = "a", fontface = "bold") + scale_x_discrete(name = "", labels = c("open ocean", "benthic", "coral reef", "coast/est", "all mar", "freshw", "forest", "terr gen", "terr other", "all terr")) + ylab("Effect size (z)") + 
  coord_cartesian(ylim = c(-1.65, 0.5)) + theme_classic() + theme(text = element_text(size = 10)) 
print(hab.plot)
ggsave(plot = hab.plot, file = paste("mean_ci_rz_hab.tiff"))

# Contains kingdom
org.plot <- ggplot(data = org.tab, aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + 
  geom_text(aes(y = -1.65, label = paste("n = ", N, sep = "")), size = 2) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + 
  annotate("text", x = 0.7, y = 0.5, label = "b", fontface = "bold") + scale_x_discrete(name = "", labels = c("bact", "proto", "herb", "wood", "other pl", "all pl", "fungi", "other inv", "arthro", "fish", "herp", "bird", "mammal", "all ani")) + 
  ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.65, 0.5)) + theme_classic() + theme(text = element_text(size = 10)) 
print(org.plot)
ggsave(plot = org.plot, file = paste("mean_ci_rz_org.tiff"))

troph.plot <- ggplot(data = subset(tab, variate == "trophic_position"), aes(x = factor(lab, levels = troph.levels), y = rz))  + 
  geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + 
  annotate("text", x = 0.7, y = 0.5, label = "c", fontface = "bold") + geom_text(aes(y = -1.65, label = paste("n = ", N, sep = "")), size = 3)  + 
  scale_x_discrete(name = "") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.65, 0.5)) + theme_classic() + theme(text = element_text(size = 10))
print(troph.plot)
ggsave(plot = troph.plot, file = paste("mean_ci_rz_troph.tiff"))


# Contains realm
habcomp.plot <- ggplot(data = hab.tab, aes(x = rz, y = rz.1)) + geom_point() + geom_errorbar(aes(ymax = CI.UB.1, ymin = CI.LB.1)) + geom_errorbarh(aes(xmax = CI.UB, xmin = CI.LB)) +
  geom_text(aes(label = lab), hjust = -0.4, vjust = 0.15, size = 3) + geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "indianred3") + xlab("Effect size (z) from Hillebrand (2004)") + 
  ylab("Effect size (z) from current study") + coord_cartesian(ylim = c(-1.8, 0.3), xlim = c(-1.8, 0.3)) + theme_classic() + theme(text = element_text(size = 10))
print(habcomp.plot)
ggsave(plot = habcomp.plot, file = paste("mean_ci_rz_habcomp.tiff"))

hemi.plot <- ggplot(data = subset(tab, variate == "hemisphere"), aes(x = lab, y = rz))  + geom_point() + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + 
  annotate("text", x = 0.5, y = 0.1, label = "d", fontface = "bold") + geom_text(aes(y = -1.1, label = paste("n = ", N, sep = "")), size = 3) + scale_y_continuous(breaks = seq(-1.0, 0, 0.2)) + 
  scale_x_discrete(name = "Hemisphere") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.1, 0.1)) + theme_classic() + theme(text = element_text(size = 10))
print(hemi.plot)
ggsave(plot = hemi.plot, file = paste("mean_ci_rz_hemi.tiff"))

longreg.plot <- ggplot(data = subset(tab, variate == "longreg"), aes(x = factor(lab, levels = lab.long), y = rz))  + geom_point() + 
  geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) +
  annotate("text", x = 1, y = 0.1, label = "b", fontface = "bold") + geom_text(aes(y = -1.1, label = paste("n = ", N, sep = "")), size = 3) + scale_y_continuous(breaks = seq(-1.0, 0.2, 0.2)) + 
  scale_x_discrete(name = "Longitude midpoint") + ylab("Effect size (z)") + coord_cartesian(ylim = c(-1.1, 0.1)) + theme_classic() + theme(text = element_text(size = 10))
print(longreg.plot)
ggsave(plot = longreg.plot, file = paste("mean_ci_rz_longreg.tiff"))

mar.ter.plot <- ggplot(data = tab, aes(x = factor(lab, levels = lab.long), y = rz, color = variate))  + geom_point(data = subset(tab, variate == "terrestrial")) + geom_errorbar(data = subset(tab, variate == "terrestrial"), aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + 
  geom_point(data = subset(tab, variate == "marine")) + geom_errorbar(data = subset(tab, variate == "marine"), aes(ymax = CI.UB, ymin = CI.LB, width = 0.1)) + scale_y_continuous(breaks = seq(-1.6, 0.2, 0.2)) + scale_x_discrete(name = "Longitude") + 
  ylab("Effect size (z)") + scale_color_manual("Realm", values = c("terrestrial" = "indianred3", "marine" = "dodgerblue3")) + coord_cartesian(ylim = c(-1.7, 0.2)) + theme_classic() + theme(text = element_text(size = 10))
print(mar.ter.plot)
ggsave(plot = mar.ter.plot, file = paste("mean_ci_rz_marter.tiff"))

tab.longmid <- print.list.rma(predict.rma(longmid))
tab.longmid <- data.frame(tab.longmid, longitude = dat$longitude_midpoint[which(!is.na(dat$longitude_midpoint), arr.ind = TRUE)])
longmid.plot <- ggplot(tab.longmid, aes(x = longitude, y = pred)) + geom_line() + geom_line(aes(x = longitude, y = ci.lb), linetype = "dashed") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) +  geom_line(aes(x = longitude, y = ci.ub), linetype = "dashed") + 
  geom_rug(sides = "b") + ylab("Effect size (z)") + xlab("Longitude midpoint") + scale_x_continuous(breaks = seq(-180, 180, 40), limits = c(-180, 180)) + 
  ylim(c(-1.6, 0.1)) + theme_classic() + theme(text = element_text(size = 10))
print(longmid.plot)
ggsave(plot = longmid.plot, file = paste("mean_ci_rz_longmid.tiff"))

longmid2 <- rma(yi = rz, vi = VarRz, mods = ~ longitude_midpoint + I(longitude_midpoint^2), data = dat)
tab.longmid2 <- print.list.rma(predict.rma(longmid2))
tab.longmid2 <- data.frame(tab.longmid2, longitude = dat$longitude_midpoint[which(!is.na(dat$longitude_midpoint), arr.ind = TRUE)])
longmid.plot2 <- ggplot(tab.longmid2, aes(x = longitude, y = pred)) + geom_line() + geom_line(aes(x = longitude, y = ci.lb), linetype = "dashed") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) +  geom_line(aes(x = longitude, y = ci.ub), linetype = "dashed") + 
  geom_rug(sides = "b") + ylab("Effect size (z)") + xlab("Longitude midpoint") + annotate("text", x = -180, y = 0.1, label = "a", fontface = "bold") + 
  scale_x_continuous(breaks = seq(-180, 180, 40), limits = c(-180, 180)) + ylim(c(-1.6, 0.1)) + theme_classic() + theme(text = element_text(size = 10))
print(longmid.plot2)
ggsave(plot = longmid.plot2, file = paste("mean_ci_rz_longmid2.tiff"))

tab.latmid <- print.list.rma(predict.rma(latmid))
tab.latmid <- data.frame(tab.latmid, latitude = dat$latitude_midpoint[which(!is.na(dat$latitude_midpoint), arr.ind = TRUE)])
latmid.plot <- ggplot(tab.latmid, aes(x = latitude, y = pred)) + geom_line() + geom_line(aes(x = latitude, y = ci.lb), linetype = "dashed") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + annotate("text", x = -90, y = 0.1, label = "c", fontface = "bold") + 
  geom_line(aes(x = latitude, y = ci.ub), linetype = "dashed") + geom_rug(sides = "b") + ylab("Effect size (z)") + xlab("Latitude midpoint") + 
  scale_x_continuous(breaks = seq(-90, 90, 20), limits = c(-90, 90)) + ylim(c(-1.6, 0.1)) + theme_classic() + theme(text = element_text(size = 10))
print(latmid.plot)
ggsave(plot = latmid.plot, file = paste("mean_ci_rz_latmid.tiff"))


tab.latrange <- print.list.rma(predict.rma(latrange))
tab.latrange <- data.frame(tab.latrange, latitude = dat$latitude_range[which(!is.na(dat$latitude_range), arr.ind = TRUE)])
latrange.plot <- ggplot(tab.latrange, aes(x = latitude, y = pred)) + geom_line() + geom_line(aes(x = latitude, y = ci.lb), linetype = "dashed") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray65", size = 0.2) + geom_line(aes(x = latitude, y = ci.ub), linetype = "dashed") + 
  geom_rug(sides = "b") + ylab("Effect size (z)") + xlab("Latitude range") + scale_x_continuous(breaks = seq(0, 180, 20), limits = c(0, 180)) + 
  ylim(c(-1.6, 0.1)) + theme_classic() + theme(text = element_text(size = 10))
print(latrange.plot)
ggsave(plot = latrange.plot, file = paste("mean_ci_rz_latrange.tiff"))


pdf(file = "fig1.pdf")
multiplot(longmid.plot2, longreg.plot, latmid.plot, hemi.plot, layout = matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
dev.off()

pdf(file = "fig2.pdf")
multiplot(hab.plot, org.plot, troph.plot, cols = 1)
dev.off()




