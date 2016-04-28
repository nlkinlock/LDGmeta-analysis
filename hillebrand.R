
Hillebrand <- gs_title("Hillebrand.csv") #load data from Hillebrand 2004
Hil <- gs_read_csv(Hillebrand, ws = "Hillebrand.csv", col_names = TRUE)
Hil <- as.data.frame(Hil)

for (i in 1:ncol(Hil)) {
  if (class(Hil[, i]) == "character") {
    Hil[, i] <- as.factor(Hil[, i])
  }
}
str(Hil)

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
                      
# Create data frame with habitat values that match Hillebrand and our data
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





