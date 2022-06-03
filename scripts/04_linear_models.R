# Workshop_04 linear models

lapply (c("ggplot2","tidyverse","ape", "binom", "car", "emmeans", "lmerTest", "metafor", "MuMIn", "pwr", "visreg"), library, character.only = TRUE)

# Data set 1 lions
# reading the data ----------------
lions <- read.csv ("lions.csv", na.string ="", strip.white = TRUE) 

# Drawing a scatter plot
ggplot (lions, aes (x = age, y = black))+
  geom_point()

# fit a linear model
# Q1
# age ~ black and black ~ age yield totally different results, when investigating residuals. 
lions.lm <- lm (data = lions, age ~ black)
# Q2
visreg (lions.lm)

# Q3
summary (lions.lm)
# Q4
confint (lions.lm)
# Q5
anova (lions.lm)
# Q6
plot (lions.lm)

# prediction 
lions.p <- predict(lions.lm, interval = "prediction") 
lions <- cbind.data.frame(lions.p, lions)
ggplot (lions, aes (y = age , x =black))+
  geom_point()+
  geom_smooth (method = "lm", se = TRUE)+
  theme(aspect.ratio = 0.80) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  theme_classic()

# Light and Circadian rhythms 
knees <- read.csv ("knees.csv", na.string = "", strip.white = TRUE)
knees$treatment <- factor ( knees$treatment)
#levels (knees$treatment) <- c("control", "knee", "eye") it does not work
knees$treatment <- factor ( knees$treatment, levels = c("control", "knee", "eyes") )
ggplot(knees, aes(treatment, shift)) +
  geom_jitter(color = "firebrick", size = 3, width = 0.15) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.1, position=position_nudge(x = 0.2)) +
  stat_summary(fun = mean, geom = "point", 
               size = 3, position=position_nudge(x = 0.2))

# fit a linear model
knees.lm  <- lm (data = knees, shift ~ treatment)
visreg(knees.lm, points.par = list(cex = 1.2, col = "red"))
plot (knees.lm)
model.matrix(knees.lm)
summary (knees.lm)
# P-value of the linear model is not true. we should do ANOVA
anova(knees.lm)
confint (knees.lm)
anova (knees.lm)
# Q7
emmeans (knees.lm, "treatment")


# Fly sex and longevity
flies <- read.csv ("fruitflies.csv", na.string = "", strip.white = TRUE)
flies$treatment <- factor(flies$treatment, levels = c("no females added", "1 pregnant female", "1 virgin female", "8 pregnant females", "8 virgin females"))

# Q6
ggplot (flies , aes (y = longevity.days , x = thorax.mm, color = treatment))+
  geom_point ()

flies.lm <- lm (data = flies, longevity.days ~ thorax.mm + treatment)
#plot (flies.lm)  
# don't include plot () in the code. It ruins your code!
# the variance of residuals is increasing
flies.log.lm <- lm (data = flies, log (longevity.days) ~thorax.mm + treatment)
flies.log.lm2 <- lm (data = flies, log (longevity.days) ~ treatment + thorax.mm)
flies.log.lm3 <- lm (data = flies, log (longevity.days) ~thorax.mm * treatment)
plot ( flies.log.lm3)
visreg (flies.log.lm3, xvar = "thorax.mm", type = "conditional", points.par = list(cex = 1.1, col = "firebrick"))
visreg (flies.log.lm3, xvar = "thorax.mm", by = "treatment", points.par = list(cex = 1.1, col = "firebrick") )
visreg (flies.log.lm, xvar = "treatment", points.par = list(cex = 1.1, col = "firebrick") )
visreg (flies.log.lm, xvar = "treatment", by = "thorax.mm", points.par = list(cex = 1.1, col = "firebrick") )
visreg (flies.log.lm3, xvar = "thorax.mm", by = "treatment", overlay = TRUE, band = FALSE)
summary (flies.log.lm)
confint (flies.log.lm)
anova (flies.log.lm)
anova (flies.log.lm3)
