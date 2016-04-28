#JUNK

# remove rows if effect sizes (slope or r) or sample sizes are missing
g <- which(is.na(dat$slope_b) | is.na(dat$rz) | is.na(dat$number_of_points))
dat <- dat[-g, ]

# new column with sd of slope
SD_b <- (dat[, "SE_b"] * sqrt(dat[, "number_of_points"]))
dat <- data.frame(dat, SD_b)

# remove missing values is measure of richness because "No NAs allowed in variables specified in the 'random' argument."
missing.meas <- which(is.na(dat$measure_of_richness))
dat <- dat[-missing.meas, ]

# studies in the southern hemisphere should have a negative lat midpoint
lat.check <- which(dat$hemisphere == "S" & dat$latitude_midpoint > 0)
lat.check

# unclear. Change to species and index.
c <- which(dat$taxonomic_resolution == "?")
dat[c, "taxonomic_resolution"] <- "species"
dat[c, "measure_of_richness"] <- "index"
# not a category
d <- which(dat$taxonomic_resolution == "Phylotype")
dat[d, "taxonomic_resolution"] <- "species"
dat[d, "measure_of_richness"] <- "index"


# error in latitude/longitude ranges, hope to fix soon
i <- which(abs(dat$longitude_range) > 360 | abs(dat$latitude_midpoint) > 90)
dat <- dat[-i, ]


#group herbaceous and shrubland/savannah
l <- which(dat$habitat == "Herbaceous" | dat$habitat == "Shrublands/savannah")
dat[l, "habitat"] <- "Herb/shrub/sav"

#group reptiles and amphibians
j <- which(dat$organism_group == "reptiles" | dat$organism_group == "amphibians")
dat[j, "organism_group"] <- "herpetofauna"


# assign id, look for duplicates, and view table completeness
#
# assign unique id for each study and case - won't work if study isn't numerical
# dat$id <- factor(paste(sprintf(fmt = "%04d", dat$studyID), sprintf(fmt = "%02d", dat$caseID), sep = "_"))

# look for duplicates in the unique id
# dat[which(duplicated(dat$id)), ]

# new column with r
# r <- sqrt(dat[, "r_sq"])
# dat <- data.frame(dat, r)

# if slope is negative, r and rz should be negative
# h <- which(dat$slope_b < 0)
# dat[h, "r"] <- dat[h, "r"] * -1
# y <- which(dat$slope_b < 0 & dat$rz > 0)
# dat[y, "rz"] <- dat[y, "rz"] * -1

# plots of mean effect size (slope and rz) grouped by factor with CIs
# generate using rma (random effects model with single moderator and no intercept)
for(i in 2:ncol(dat)){
  if(class(dat[, i]) == "factor") {
    model <- rma(yi = rz, vi = VarRz, mods = ~ -1 + dat[, i], data = dat)
    tab <- data.frame(rz = model$b, CI.LB = model$ci.lb, CI.UB = model$ci.ub)
    tab <- data.frame(tab, labels = levels(dat[, i]))
    fig <- ggplot(tab, aes(x = labels, y = rz)) + geom_point() + scale_x_discrete(name="") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic()
    print(fig)
    ggsave(plot = fig, file = paste("mean_ci_rz", i, ".pdf", sep=""))
  }
}

# plots of categorical variables, with bubble size proportional to the inverse of variance
# plot against both effect sizes (slope and rz)
for(i in 1:ncol(dat)){
  if(class(dat[, i]) == "factor") {
    s <- ggplot(dat, aes(x = dat[, i], y = rz, label = studyID)) + geom_point(aes(size = (1/VarRz)), alpha = 0.2) + labs(x = names(dat)[i], y = "rz") + scale_size(range = c(1, 15)) + theme_classic()
    print(s)
    ggsave(plot = s, file = paste("eff_plot_rz", i, ".pdf", sep=""))
  }
}

slope.org <- ggplot(data = dat, aes(x = organism_group, y = slope_b)) + ggtitle("Slope grouped by organism group") + geom_boxplot() + ylab("Slope") + xlab("") + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
slope.org.lim <- ggplot(data = dat, aes(x = organism_group, y = slope_b)) + ggtitle("Slope grouped by organism group") + geom_boxplot() +  coord_cartesian(ylim = c(-40, 40)) + ylab("Slope") + xlab("") + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
slope.king <- ggplot(data = dat, aes(x = kingdom, y = slope_b)) + ggtitle("Slope grouped by kingdom") + geom_boxplot(na.rm = TRUE) + ylab("Slope") + xlab("") + theme_classic()
slope.king.lim <- ggplot(data = dat, aes(x = kingdom, y = slope_b)) + ggtitle("Slope grouped by kingdom") + geom_boxplot(na.rm = TRUE) + coord_cartesian(ylim = c(-40, 40)) + ylab("Slope") + xlab("") + theme_classic()
print(slope.org)
print(slope.org.lim)
print(slope.king)
print(slope.king.lim)

dat$longmin <- dat$longitude_midpoint - (dat$longitude_range/2)
dat$latmin <- dat$latitude_midpoint - (dat$latitude_range/2)
dat$longmax <- dat$longitude_midpoint + (dat$longitude_range/2)
dat$latmax <- dat$latitude_midpoint + (dat$latitude_range/2)

worldmap <- ggplot(world, aes(x = long, y = lat)) +
  geom_path(aes(group = group)) + geom_point(data = dat, aes(x = longitude_midpoint, y = latitude_midpoint, size = (1/VarRz)), color="darkslategray") +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) + theme_classic()
worldmap
ggsave(plot = worldmap, file = "worldmapVarRz.pdf")

# visualize lat and long midpoints on world map
# size of point = sample size
world <- map_data("world")
worldmapRz <- ggplot(data = world, aes(x = long, y = lat)) +
  geom_path(aes(group = group)) + geom_segment(data = dat, aes(x = longmin, y = latitude_midpoint, xend = longmax, yend = latitude_midpoint, colour = rz, alpha = 0.8)) +
  geom_segment(data = dat, aes(x = longitude_midpoint, y = latmin, xend = longitude_midpoint, yend = latmax, colour = rz, alpha = 0.8)) +
  scale_size(guide = "none") + scale_alpha(guide = "none") + scale_colour_distiller(type = "div", palette = "RdBu", direction = -1) +
  scale_y_continuous(breaks = (-2:2) * 30, limits = c(-90, 90)) + ylab("Latitude") +
  scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)) + xlab("Longitude") + theme_classic()
worldmapRz
ggsave(plot = worldmapRz, file = "worldmapRz.pdf")

worldmapRz.pt <- ggplot(data = world, aes(x = long, y = lat)) +
  geom_path(aes(group = group)) + geom_point(data = dat, aes(x = longitude_midpoint, y = latitude_midpoint, colour = rz), size = 2) +
  scale_size(guide = "none") + scale_colour_distiller(type = "div", palette = "RdBu", direction = -1) +
  scale_y_continuous(breaks = (-2:2) * 30, limits = c(-90, 90)) + ylab("Latitude") +
  scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)) + xlab("Longitude") + theme_classic()
worldmapRz.pt
ggsave(plot = worldmapRz, file = "worldmapRz.pdf")

worldmapSlope <- ggplot(world, aes(x = long, y = lat)) +
  geom_path(aes(group = group)) + geom_segment(data = dat, aes(x = longmin, y = latitude_midpoint, xend = longmax, yend = latitude_midpoint, colour = slope_b, alpha = 0.8)) +
  geom_segment(data = dat, aes(x = longitude_midpoint, y = latmin, xend = longitude_midpoint, yend = latmax, colour = slope_b, alpha = 0.8)) +
  scale_size(guide = "none") + scale_alpha(guide = "none") + scale_colour_distiller(type = "div", palette = "RdBu", direction = -1) +
  scale_y_continuous(breaks = (-2:2) * 30, limits = c(-90, 90)) + ylab("Latitude") +
  scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)) + xlab("Longitude") + theme_classic()
worldmapSlope
ggsave(plot = worldmapSlope, file = "worldmapSlope.pdf")


H.table <- data.frame(H.table, Source = rep("Hillebrand", nrow(H.table)))
tab <- data.frame(tab, Source = rep("SB", nrow(tab)))
all.tab <- rbind(H.table, tab)

kingdom <- rma(yi = rz, vi = VarRz, mods = ~ - 1 + kingdom, data = dat)
tab1 <- data.frame(rz = kingdom$b, CI.LB = kingdom$ci.lb, CI.UB = kingdom$ci.ub)
tab1 <- data.frame(tab1, labels = levels(dat$kingdom))
fig1 <- ggplot(tab1, aes(x = labels, y = rz)) + ggtitle("Rz grouped by kingdom") + geom_point() + scale_x_discrete(name="") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic()
print(fig1)
ggsave(plot = fig1, file = paste("mean_ci_rz_kingdom.pdf"))

subset.orgrp <- dat[which(dat$organism_group != "multiple"), ]
orggrp <- rma(yi = rz, vi = VarRz, mods = ~ -1 + organism_group, data = subset.orgrp)
tab2 <- data.frame(rz = orggrp$b, CI.LB = orggrp$ci.lb, CI.UB = orggrp$ci.ub)
tab2 <- data.frame(tab2, labels = levels(factor(subset.orgrp$organism_group)))
fig2 <- ggplot(tab2, aes(x = labels, y = rz)) + geom_point() + ggtitle("Rz grouped by organism group (multiple removed)") + scale_x_discrete(name="") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(fig2)
ggsave(plot = fig2, file = paste("mean_ci_rz_orggrp.pdf"))

subset.therm <- dat[which(dat$thermoregulation != "multiple"), ]
thermoreg <- rma(yi = rz, vi = VarRz, mods = ~ -1 + thermoregulation, data = subset.therm)
tab3 <- data.frame(rz = thermoreg$b, CI.LB = thermoreg$ci.lb, CI.UB = thermoreg$ci.ub)
tab3 <- data.frame(tab3, labels = (levels(factor(subset.therm$thermoregulation))))
fig3 <- ggplot(tab3, aes(x = labels, y = rz)) + geom_point() + scale_x_discrete(name="") + ggtitle("Rz grouped by thermoregulation (multiple removed)") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic()
print(fig3)
ggsave(plot = fig3, file = paste("mean_ci_rz_therm.pdf"))

realm <- rma(yi = rz, vi = VarRz, mods = ~ -1 + realm, data = dat)
tab4 <- data.frame(rz = realm$b, CI.LB = realm$ci.lb, CI.UB = realm$ci.ub)
tab4 <- data.frame(tab4, labels = levels(dat$realm))
fig4 <- ggplot(tab4, aes(x = labels, y = rz)) + geom_point()+ ggtitle("Rz grouped by realm") + scale_x_discrete(name="") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic()
print(fig4)
ggsave(plot = fig4, file = paste("mean_ci_rz_realm.pdf"))

subset.hab <- dat[which(dat$habitat != "Multiple"), ]
hab <- rma(yi = rz, vi = VarRz, mods = ~ -1 + habitat, data = subset.hab)
tab5 <- data.frame(rz = hab$b, CI.LB = hab$ci.lb, CI.UB = hab$ci.ub)
tab5 <- data.frame(tab5, labels = levels(factor(subset.hab$habitat, levels = c("Coastal/estuary", "Coral reefs", "Open ocean", "Benthic", "Freshwater", "Forest", "Terrestrial general", "Terrestrial other"))))
fig5 <- ggplot(tab5, aes(x = labels, y = rz)) + geom_point() + ggtitle("Rz grouped by habitat (multiple removed)") + scale_x_discrete(name="") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(fig5)
ggsave(plot = fig5, file = paste("mean_ci_rz_hab.pdf"))

troph <- rma(yi = rz, vi = VarRz, mods = ~ -1 + trophic_position, data = dat)
tab6 <- data.frame(rz = troph$b, CI.LB = troph$ci.lb, CI.UB = troph$ci.ub)
tab6 <- data.frame(tab6, labels = levels(dat$trophic_position))
fig6 <- ggplot(tab6, aes(x = labels, y = rz)) + geom_point() + ggtitle("Rz grouped by trophic position") + scale_x_discrete(name="") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(fig6)
ggsave(plot = fig6, file = paste("mean_ci_rz_troph.pdf"))

hemi <- rma(yi = rz, vi = VarRz, mods = ~ -1 + hemisphere, data = dat)
tab7 <- data.frame(rz = hemi$b, CI.LB = hemi$ci.lb, CI.UB = hemi$ci.ub)
tab7 <- data.frame(tab7, labels = levels(dat$hemisphere))
fig7 <- ggplot(tab7, aes(x = labels, y = rz)) + geom_point() + ggtitle("Rz grouped by hemisphere") + scale_x_discrete(name="") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic()
print(fig7)
ggsave(plot = fig7, file = paste("mean_ci_rz_hemi.pdf"))

div <- rma(yi = rz, vi = VarRz, mods = ~ -1 + diversity, data = dat)
tab8 <- data.frame(rz = div$b, CI.LB = div$ci.lb, CI.UB = div$ci.ub)
tab8 <- data.frame(tab8, labels = levels(dat$diversity))
fig8 <- ggplot(tab8, aes(x = labels, y = rz)) + geom_point() + ggtitle("Rz grouped by diversity type") + scale_x_discrete(name="") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic()
print(fig8)
ggsave(plot = fig8, file = paste("mean_ci_rz_div.pdf"))

meas <- rma(yi = rz, vi = VarRz, mods = ~ -1 + measure_of_richness, data = dat)
tab9 <- data.frame(rz = meas$b, CI.LB = meas$ci.lb, CI.UB = meas$ci.ub)
tab9 <- data.frame(tab9, labels = levels(dat$measure_of_richness))
fig9 <- ggplot(tab9, aes(x = labels, y = rz)) + geom_point() + ggtitle("Rz grouped by measure of richness") + scale_x_discrete(name="") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic()
print(fig9)
ggsave(plot = fig9, file = paste("mean_ci_rz_meas.pdf"))

ocean <- rma(yi = rz, vi = VarRz, mods = ~ -1 + habitat, data = dat[which(dat$habitat == "Open ocean"), ])
forest(x = ocean)
birds <- rma(yi = rz, vi = VarRz, mods = ~ -1 + organism_group, data = dat[which(dat$organism_group == "birds"), ])
forest(x = birds)
herps <- rma(yi = rz, vi = VarRz, mods = ~ -1 + organism_group, data = dat[which(dat$organism_group == "herpetofauna"), ])
forest(x = herps)
dat[which(dat$organism_group == "birds"), ]
dat[which(dat$habitat == "Open ocean"), ]
dat[which(dat$organism_group == "herpetofauna"), ]


# attempting to figure out how to do least square means
recover.data.rma <- function(object, trms, data, ...) {
  fcall = object$call
  recover.data(fcall, trms, data,
               ...)
}

lsm.basis.rma <- function (object, trms, xlev, grid, ...) 
{
  list(X = object$X, bhat = object$b, nbasis = matrix(NA), V = object$vb, dffun = function(k, 
                                                                                           dfargs) NA, dfargs = list())
}

mods <- c("kingdom", "realm", "hemisphere", "measure.of.richness", "latitude.range", "diversity", "taxonomic.resolution")
lev <- list(kingdom = c("Animalia", "Bacteria", "Fungi", "Plantae", "Protista"), realm = c("terrestrial", "marine", "freshwater", "multiple"), hemisphere = c("N", "S", "both"), latitude.range = mean(dat$latitude.range), measure.of.richness = c("S", "index"), diversity = c("alpha", "beta", "gamma", "ranges"), taxonomic.resolution = c("species", "higher.rank"))
dat[ ,mods]

mods.slope.lm <- lm(formula = slope.b ~ kingdom + realm + hemisphere + measure.of.richness + latitude.range + diversity + taxonomic.resolution, data = subset.slope)
summary(mods.slope.lm)
str(mods.slope.lm)

newdat <- model.matrix(~ kingdom + realm + hemisphere + measure.of.richness + latitude.range + diversity + taxonomic.resolution, data = dat)
newdat[, c(2:5)] <- 1/length(levels(dat$kingdom))
newdat[, c(6:8)] <- 1/length(levels(dat$realm))
newdat[, c(9:10)] <- 1/length(levels(dat$hemisphere))
newdat[, 11] <- 1/length(levels(dat$measure.of.richness))
newdat[, "latitude.range"] <- mean(dat$latitude.range, na.rm = TRUE)
newdat[, c(13:15)] <- 1/length(levels(dat$diversity))
newdat[, 16] <- 1/length(levels(dat$taxonomic.resolution))
predict.rma(object = mods.slope, newmods = newdat[, -1])

if(i == 14 | i == 15 | i == 18 | i == 19) {
  model <- rma(yi = rz, vi = VarRz, mods = ~ -1 + dat[, i], data = dat)
  tab <- data.frame(rz = model$b, CI.LB = model$ci.lb, CI.UB = model$ci.ub)
  tab <- data.frame(tab, labels = num_lab[as.character(i)])
  fig <- ggplot(tab, aes(x = labels, y = rz)) + geom_point() + scale_x_discrete(name="") + geom_errorbar(aes(ymax = CI.UB, ymin = CI.LB, width = 0.2)) + theme_classic()
  print(fig)
  ggsave(plot = fig, file = paste("mean_ci_slope", i, ".pdf", sep=""))
}


dat$latitude_range_std <- dat$latitude_range/sd(dat$latitude_range, na.rm = TRUE)
dat$latitude_midpoint_std <- dat$latitude_midpoint/sd(dat$latitude_midpoint, na.rm = TRUE)
dat$longitude_range_std <- dat$longitude_range/sd(dat$longitude_range, na.rm = TRUE)
dat$longitude_midpoint_std <- dat$longitude_midpoint/sd(dat$longitude_midpoint, na.rm = TRUE)

full.model <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + organism_group + thermoregulation + dispersal_type + trophic_position + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + hemisphere + diversity, data = dat, random = rand)
full.model.noint <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + organism_group + thermoregulation + dispersal_type + trophic_position + realm + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + hemisphere + diversity - 1, data = dat, random = rand)
mods.a <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + hemisphere + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, data = dat, random = rand)
mods.a.noint <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + hemisphere + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity - 1, data = dat, random = rand)
mods.a2 <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + hemisphere + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, random = rand2, data = dat)
mods.a2.noint <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + hemisphere + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity - 1, random = rand2, data = dat)
mods.a3 <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + hemisphere + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, random = rand3, data = dat)
mods.a3.noint <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + hemisphere + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity - 1, random = rand3, data = dat)
mods.a4 <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + hemisphere + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity, random = rand4, data = dat)
mods.a4.noint <- rma.mv(yi = rz, V = VarRz, mods = ~ kingdom + realm + hemisphere + longitude_range + longitude_midpoint + latitude_range + latitude_midpoint + diversity - 1, random = rand4, data = dat)
paste("full model: ", round(aicc(full.model), digits = 2), "full model w/o int: ", round(aicc(full.model.noint), digits = 2), "red model rand: ", round(aicc(mods.a), digits = 2), "red model rand w/o int: ", round(aicc(mods.a.noint), digits = 2), "red model rand2: ", round(aicc(mods.a2), digits = 2), "red model rand2 w/o int: ", round(aicc(mods.a2.noint), digits = 2), "red model rand3: ", round(aicc(mods.a3), digits = 2), "red model rand3 w/o int: ", round(aicc(mods.a3.noint), digits = 2),  "red model rand4: ", round(aicc(mods.a4), digits = 2), "red model rand4 w/o int: ", round(aicc(mods.a4.noint), digits = 2))
# intercept does not influence aicc. I think intercepts should be removed because we are not interested in testing hyptoheses about significant differences between categories, but rather which categories are more important, as well as understanding the mean of the category conditional on the other variables.
# rand2 gives lowest aicc
# coefficients for the continuous variables do not differ between intercept/nointercept
summary(full.model)
summary(full.model.noint)
summary(mods.a2)
summary(mods.a2.noint)

#McFadden's pseudo R2
r2 <- c()
for (i in 1:nrow(rank.models)) {
  r2[i] <- 1 - (models@objects[[i]]$fit.stats[1, 1] / int.rand2$fit.stats[1, 1])
}
r2

#         therm.rz = H.therm$b, therm.LB = H.therm$ci.lb, therm.UB = H.therm$ci.ub, therm.lab = levels(factor(Hil$therm)))
#        realm.rz = H.realm$b, realm.LB = H.realm$ci.lb, realm.UB = H.realm$ci.ub, realm.lab = levels(factor(Hil$realm))) 
#       hab.rz = H.hab$b, hab.LB = H.hab$ci.lb, hab.UB = H.hab$ci.ub, hab.lab = levels(factor(Hil$hab)))
#      hemi.rz = H.hemi$b, hemi.LB = H.hemi$ci.lb, hemi.UB = H.hemi$ci.ub, hemi.lab = levels(factor(Hil$hemi)))
#     scale.rz = H.scale$b, scale.LB = H.scale$ci.lb, scale.UB = H.scale$ci.ub, scale.lab = levels(factor(Hil$scale))) 
#    div.rz = H.div$b, div.LB = H.div$ci.lb, div.UB = H.div$ci.ub, div.lab = levels(factor(Hil$div)))
#   troph.rz = H.troph$b, troph.LB = H.troph$ci.lb, troph.UB = H.troph$ci.ub, troph.lab = levels(factor(Hil$troph)))

test <- rma.mv(yi = rz, V = VarRz, mods = ~ latitude_midpoint_std + realm, data = dat, random = rand2)
summary(test)
anitest <- rma.mv(yi = rz, V = VarRz, mods = ~ latitude_range + latitude_midpoint_std + habitat + hemisphere, data = dat, random = rand2, subset = animalia)
summary(anitest)
platest <- rma.mv(yi = rz, V = VarRz, mods = ~ latitude_midpoint + habitat, data = dat, random = rand2, subset = plantae)
summary(platest)






