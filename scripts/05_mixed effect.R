# Mixed effect models

# installing packagess------------

lapply (c("ggplot2","tidyverse","ape", "binom", "car", "emmeans", "lmerTest", "metafor", "MuMIn", "pwr", "visreg"), library, character.only = TRUE)

# reading the data
flycatcher <- read.csv ("flycatcher.csv", strip.white = TRUE)
flycatcher$year <- factor (flycatcher$year)
ggplot (flycatcher, aes (y = patch , x = year)) +
  geom_point(size = 4, col = "firebrick", alpha = 0.5) +
  labs (y = "parch size", x = "year" )+
  geom_line ( aes ( group = bird)) + 
  theme_classic()

# Fit a linear mixed effect model 
fly.lmer <- lmer (patch ~ 1 + (1|bird), data = flycatcher)
fly.lmer2 <- lmer (patch ~ 1 + (1|bird) + (1|year), data = flycatcher)
fly.lmer3 <- lmer (patch ~ year + (1|bird), data = flycatcher)
summary (fly.lmer)
plot (fly.lmer)
fly.var <- VarCorr(fly.lmer)
confint (fly.lmer)
fitted (fly.lmer)
anova (fly.lmer3)
plot (fly.lmer)
plot (resid (fly.lmer) ~ flycatcher$patch )
plot (fitted (fly.lmer) ~ flycatcher$patch )
# is the positive trend because of the regression to the mean?
# Goldies vision
goldfish <- read.csv ( "goldfish.csv", strip.white = TRUE)
ggplot (goldfish, aes (y = sensitivity , x = wavelength)) +
  geom_jitter(size = 3, col = "firebrick", alpha = 0.5, width = 0.15) +
  labs (y = "sensitivity", x = "wave length" )+
  geom_line ( aes ( group = fish)) + 
  theme_classic()

gold.lmer <- lmer (data = goldfish, sensitivity ~ wavelength + (1|fish))
plot (gold.lmer)
plot (fitted (gold.lmer) ~ goldfish$sensitivity)

ggplot (goldfish, aes (x = wavelength, y = sensitivity, color = fish))+
  geom_point ()+
  theme_classic()
summary (gold.lmer)
# why there is no variance explained by fish? because of boundry singular fit problem which arises when the number of datapoints is small. 
anova  (gold.lmer)
stripchart (data = goldfish, sensitivity ~ wavelength, vertical = TRUE,  method = "jitter",  ylim = c(-1,2))
stripchart (data = goldfish, fitted (gold.lmer) ~ wavelength, vertical = TRUE,  method = "jitter",  ylim = c(-1,2))
visreg (gold.lmer)


##Yukon yarrow
kluane <- read.csv ("kluane.csv", strip.white = TRUE)
kluane$treatment <- factor (kluane$treatment, levels = c("control", "exclosure", "fertilizer", "both"))
ggplot (data = kluane, aes (x = duration , y = phen.ach, color = duration))+
  geom_point (size = 3,  alpha = 0.5)+
  geom_line (aes (group = plot))+
  facet_wrap(~treatment, nrow = 1)+
  theme_classic()
kluane.lmer <- lmer (data = kluane, log(phen.ach) ~ treatment + duration + (1|plot))             
summary (kluane.lmer)
anova (kluane.lmer)
visreg (kluane.lmer, xvar = "treatment", by = "duration", overlay = TRUE)
visreg (kluane.lmer, xvar = "duration", by = "treatment")
kluane.lmer2 <- lmer (data = kluane, log(phen.ach) ~ treatment * duration + (1|plot))             
summary (kluane.lmer2)
anova (kluane.lmer2)
anova (kluane.lmer2, type = 3)
# why there is no difference between type 1 and type 3 anova? is it because we have designed the experiment
# and there is no correlation between treatment and duration?
# I should meet Dolph!
visreg (kluane.lmer2, xvar = "treatment", by = "duration", overlay = TRUE)
visreg (kluane.lmer2, xvar = "duration", by = "treatment")
plot (kluane.lmer2)

emmeans (kluane.lmer2,c("duration", "treatment"), data = kluane)
confint (kluane.lmer2)

plot (predict (kluane.lmer2) ~ log(kluane$phen.ach))
hist (log(kluane$phen.ach))    
hist (predict (kluane.lmer2))
# weird difference. Ask Dolph
grpMeans <- emmeans (kluane.lmer2, spec = c ("duration", "treatment"))
pairs (grpmeans)
plot (grpMeans, comparisons = TRUE)
# what does comparison show?
# it's important to check assumptions of ANOVA
hist (resid (kluane.lmer2))
