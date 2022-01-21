# Session02_Graphs and tables
# installing packagess------------

lapply (c("tidyverse","ape", "binom", "car", "emmeans", "lmerTest", "metafor", "MuMIn", "pwr", "visreg"), install.packages)
lapply (c("ggplot2","tidyverse","ape", "binom", "car", "emmeans", "lmerTest", "metafor", "MuMIn", "pwr", "visreg"), library, character.only = TRUE)

# Data set 1 Mammal Body mass
# reading the data ----------------
mammals <- read.csv ("mammals.csv", na.string ="", strip.white = TRUE) 
# Q2
mammals$continent [which (mammals$continent == "Af")] <-"AF"
# Q1
mam_freq <- mammals %>% 
  filter (status == "extant") %>% 
  group_by(continent) %>% 
  count()

mam_freq$n <- mam_freq$n / length (mammals$continent)

# Q3
mammals %>% 
  filter (status == "extinct") %>% 
  count()
# Q4
table (mammals$continent, mammals$status)

# Q5: Australia

# Graphing frequency 
# Q1
barplot(table(mammals$continent),col = "firebrick", space = 0.2, cex.names = 1.2)
# or
ggplot(mammals, aes(x = continent)) +
  geom_bar(stat="count", fill = "firebrick") +
  labs(x = "continent", y = "number counted") +
  theme_classic()  +
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 12), aspect.ratio = 0.8)
# Q2

mammals$orderedcontinent <-  factor(mammals$continent,levels = names(sort(table(mammals$continent), decreasing = TRUE))) 

ggplot(mammals, aes(x = orderedcontinent)) +
  geom_bar(stat="count", fill = "firebrick") +
  labs(x = "continent", y = "number counted") 

# Q3
hist (mammals$mass.grams)
#Q4&5
mammals$mass.grams.log <- log (mammals$mass.grams)
hist (mammals$mass.grams.log)
# Q6
hist (mammals$mass.grams.log, breaks = seq(0, 20, 2))
# Q7 (0.5 or 1?)
hist (mammals$mass.grams.log, breaks = seq(0, 20, 1))
# Q8
ggplot(mammals , aes(x = mass.grams.log)) + 
  geom_histogram(fill = "firebrick", col = "black", binwidth = 0.5, 
                  closed = "left") + 
  labs(x = "The variable x", y = "Frequency") 

# Q10 # check why stat_function does not work
ggplot(mammals, aes(x = mass.grams.log)) + 
  geom_histogram(aes(y = ..density..), fill = "firebrick", col = "black", binwidth = 0.5, 
                 boundary = 0, closed = "left") + 
  stat_function(fun = dnorm, args = list(mean = mean(mammals$mass.grams.log, na.rm = TRUE), 
                                         sd = sd(mammals$mass.grams.log, na.rm = TRUE))) +
  theme_classic()

hist(mammals$mass.grams.log, prob = TRUE, right = FALSE)
m <- mean(mammals$mass.grams.log, na.rm = TRUE)
s <- sd(mammals$mass.grams.log, na.rm = TRUE)
xpts <- seq(from = min(mammals$mass.grams.log, na.rm=TRUE), 
            to = max(mammals$mass.grams.log, na.rm = TRUE), length.out = 101)
lines(dnorm(xpts, mean=m, sd=s) ~ xpts, col="red", lwd=2)
  
#Q9
ggplot(mammals, aes(sample = mass.grams.log)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic()

# Comparing frquency distributions
#Q1
boxplot(mass.grams.log ~ status, data = mammals, varwidth = FALSE, 
        ylab="mass.grams.log", col = "firebrick", cex.axis = 0.8, las = 1)
#Q3
boxplot(mass.grams.log ~ status, data = mammals, varwidth = TRUE, 
        ylab="mass.grams.log", col = "firebrick", cex.axis = 0.8, las = 1, main = "Mass comparison") 

ggplot(mammals, aes(x = status, y = mass.grams.log), main = "Mass comparison") +
  geom_boxplot(fill = "goldenrod1", varwidth = TRUE) + 
  theme_classic() + 
  theme( text = element_text(size = 14))
#Q4
ggplot (mammals, aes (x = status , y = mass.grams.log)) +
  geom_violin (fill = 20) +
  stat_summary (fun.y = mean, geom = "point", color = "black")+
  theme_classic()

# Q5
ggplot (mammals, aes (x = mass.grams.log)) +
  geom_histogram(fill = 2, binwidth = 0.5) + 
  theme_classic() +
  facet_wrap (~ status, ncol = 1, scales = "free_y")
# Q6
mam_median <- mammals %>% 
  group_by(status) %>%
  summarise (meedian = median (mass.grams.log, na.rm = TRUE))
  
# Data set 2: Fly sex and longevity
flies <- read.csv ("fruitflies.csv", na.string = "", strip.white = TRUE)

# How I analyze the data
# summarize the data
flies %>%
  group_by (treatment) %>% 
  summarise (mean = mean (longevity.days, na.rm = TRUE))
# drawing multiple histograms of longevity
ggplot (flies, aes (x = longevity.days))+
  geom_histogram(fill = 2, binwidth = 0.5)+
  theme_classic()+
  facet_wrap (~ treatment, ncol = 1, scale = "free_y")
# was not useful
# let's draw box plot
ggplot (flies , aes(x = treatment, y = longevity.days)) +
  geom_boxplot(fill = 5, position = position_dodge(width = 0.7)) +
  theme_classic ()
# strip chart
ggplot (flies, aes (x = treatment, y = longevity.days))+
  geom_jitter(color = 2, size = 3, width = 0.1)+
  stat_summary (fun.data = mean_se, geom = "errorbar", width= 0.2, positioin = position_nudge (x = 0.2))+
  stat_summary(fun.y = mean, geom = "point", 
               size = 3, position=position_nudge(x = 0)) +
  theme_classic() 

# scatter plot
ggplot (flies, aes (x = thorax.mm, y = longevity.days, color = treatment))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

  

  
