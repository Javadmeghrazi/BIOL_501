## Session01_introduction to R
## installing packagess

lapply (c("ape", "binom", "car", "emmeans", "lmerTest", "metafor", "MuMIn", "pwr", "visreg"), install.packages)
lapply (c("ape", "binom", "car", "emmeans", "lmerTest", "metafor", "MuMIn", "pwr", "visreg"), library, character.only = TRUE)

# Flying snakes-------------------------------

# importing data
GlideUndulation <- c(0.9, 1.4, 1.2, 1.2, 1.3, 2.0, 1.4, 1.6)

# drawing a histogram and driving basic statistics
hist (GlideUndulation, right = FALSE)
GlideUndulation_radian <- GlideUndulation * 2 * pi
Glide_mean <- sum (GlideUndulation_radian)/ length (GlideUndulation_radian)
Glide_sd <- sqrt (sum ((GlideUndulation_radian - Glide_mean)^2)/(length (GlideUndulation_radian)-1))
Glide_median <- mean (sort(GlideUndulation_radian)[c(length(GlideUndulation_radian) / 2, length(GlideUndulation_radian) / 2 + 1)])
Glide_SE <- Glide_sd / sqrt (length (GlideUndulation_radian))                 

# Missing data
Glide_mis <- c(GlideUndulation_radian, NA)
Glide_mis_mean <- mean (Glide_mis, na.rm = TRUE)
Glide_mis_sd <- sqrt (sum ((Glide_mis[!is.na(Glide_mis)] - Glide_mis_mean)^2)/(length (Glide_mis[!is.na(Glide_mis)])-1))
GLide_mis_SE <- Glide_mis_sd / sqrt(length (Glide_mis[!is.na(Glide_mis)]))

# Anolis lizards in a data frame ----------------------------------                     
anolis <- read.csv ("anolis.csv")                                    
anolis$Ecomorph [which (anolis$Ecomorph == "Trunk-Crown " )] <- "Trunk-Crown"
# striping the spaces automatically
anolis <- read.csv ("anolis.csv", na.string = "", strip.white = TRUE)  
anolis$Ecomorph [is.na(anolis$ecomorph)] <- "none"

## species exclusive to Jamaica
Jamaica_species <- anolis$Species [anolis$Island == "Jamaica"]
anolis [anolis$Species %in% Jamaica_species,]

Cuba_species <- anolis$Species [anolis$Island == "Cuba"]
length ( grep ("Cuba", anolis$Island))
# Q16
length(anolis$Species [anolis$Island %in% c("Jamaica", "Hispaniola", "Puerto Rico", "Cuba")])
# Q17 
table (anolis$Ecomorph[! anolis$Island %in% c("Jamaica", "Hispaniola", "Puerto Rico", "Cuba")] )
