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
lions.lm <- lm (data = lions, black ~ age)
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
ggplot (lions, aes (x = age , y =black))+
  geom_point()+
  geom_smooth (method = "lm", se = TRUE)+
  theme(aspect.ratio = 0.80) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  theme_classic()

# Light and Circadian rhythms 
knees <- read.csv ("knees.csv", na.string = "", strip.white = TRUE)
knees$treatment <- as.factor ( knees$treatment)

ggplot(knees, aes(treatment, shift)) +
  geom_jitter(color = "firebrick", size = 3, width = 0.15) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.1, position=position_nudge(x = 0.2)) +
  stat_summary(fun.y = mean, geom = "point", 
               size = 3, position=position_nudge(x = 0.2))

# fit a linear model
knees.lm  <- lm (data = knees, shift ~ treatment)
visreg(knees.lm, points.par = list(cex = 1.2, col = "red"))
plot (knees.lm)
model.matrix(knees.lm)

# P-value of the linear model is not true. we should do ANOVA
anova(knees.lm)
confint (knees.lm)
anova (knees.lm)
# Q7
emmeans (knees.lm, "treatment", data = knees)


# Fly sex and longevity
flies <- read.csv ("fruitflies.csv", na.string = "", strip.white = TRUE)
flies$treatment <- factor(flies$treatment)
levels (flies$treatment) <- c("no females added", "1 pregnant female", "1 virgin female", "8 pregnant female", "8 virgin females")
# Q6
ggplot (flies , aes (y = longevity.days , x = thorax.mm, color = treatment))+
  geom_point ()

flies.lm <- lm (data = flies, longevity.days ~ thorax.mm + treatment)
plot (flies.lm)  
# the variance of residuals is increasing
flies.log.lm <- lm (data = flies, log (longevity.days) ~thorax.mm + treatment)
flies.log.lm2 <- lm (data = flies, log (longevity.days) ~ treatment + thorax.mm)
plot ( flies.log.lm)
visreg (flies.log.lm, by = treatment)
visreg (flies.log.lm2)
  anova (flies.log.lm)
  anova (flies.log.lm2)
