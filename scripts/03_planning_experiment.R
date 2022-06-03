## 03_Planning tools
lapply(c("ggplot2", "tidyr", "binom","dplyr","pwr"), library, character.only = TRUE)

# Random sampling warm-up ----------------
#Q1
status_inf <- sample (c("infected", "healthy"), size = 20, replace = TRUE, prob = c(0.5, 0.5))
table (status_inf)
# Q3
status_mate <- sample (c("mated", "unmated"), size = 18, replace = TRUE, prob = c(0.7, 0.3))
table (status_mate)
# Q5
normal_dist <- rnorm (30, mean = 0, sd = 2)
ggplot (as.data.frame(normal_dist), aes (normal_dist))+
  geom_histogram(binwidth = 2)

# plan for percision ----------------------------
# ** To avoid pseudoreplication, females are tested only once and males are replaced between tests.
# One idea is to collect data and use it to test the null hypothesis of no preference. If the null hypothesis is true, you should fail to reject it. However, this won’t be very convincing to your committee. Failing to reject a null hypothesis is inconclusive by itself. Maybe your test won’t have much power.

# Q1
spider_choice <- sample (c("success", "faliure"), size = 10, replace = TRUE, prob = c(0.5, 0.5))
spoder_CI <- binom.confint (length(which (spider_choice == "success")), length (spider_choice), method = "ac")

# Q2-Q6
CI_span <- c()
for (i in 1:5){
  spider_choice <- sample (c("success", "faliure"), size = 100, replace = TRUE, prob = c(0.5, 0.5))
  spider_CI <- binom.confint (length(which (spider_choice == "success")), length (spider_choice), method = "ac")
  CI_span [i] <- spider_CI$upper - spider_CI$lower
}
# CI_span is highly reproducible

# Plan for power--------------------------
p_value <- c()
for (i in 1:1000){
  spider_choice <- sample (c("success", "faliure"), size = 40, replace = TRUE, prob = c(0.8, 0.2))
  z <- binom.test (length(which (spider_choice == "success")), length (spider_choice), p = 0.5 )
  p_value [i] <- z$p.value
}
# p_value_mean <- mean (p_value)
# p_value_sd <- sd (p_value)
rejection_freq <- length (which(p_value < 0.05))/length (p_value)                       
table (p_value)
# mean of p_value is not important because a single large p_value affect it 
# much more than many small p_values. instead mean of log (p_value/(1-(p_value))) is something.

# Power and sample size ----------------------------
h <- ES.h (0.5, 0.8)
pwr.p.test (h, power = 0.8)

# Plan 2x2 experiment ----------------
# Q1-3
treatment <- c(rep("control", 30), rep("egg-removal", 30))
control <- sample (c("malaria", "no-malaria"), size = 30, replace = TRUE, prob = c(0.2, 0.8))
egg <- sample (c("malaria", "no-malaria"), size = 30, replace = TRUE, prob = c(0.5, 0.5))
df_1 <- data.frame (treatment,status = c(control, egg))
# Q4
table_df_1 <- table (df_1)
table_df_1 <- table_df_1 / 30
# why should we divide by sum (table_df_1)
w <- ES.w2 (table_df_1/sum (table_df_1))
pwr.chisq.test(w, df = 1, N = 60)
pwr.chisq.test(w, df = 1, power = 0.8)

# Plan a 2 treatment experiment -----------------
control <- round (rnorm (20, mean = 10, sd = sqrt(10)))
treatment <- round (rnorm (20, mean = 15, sd = sqrt(10)))
df_2 <- data.frame (control, treatment) %>% 
  pivot_longer(cols = 1:2, names_to = "treatment", values_to = "number_of_species")

ggplot (df_2, aes(number_of_species))+
  geom_histogram(binwidth = 3)+
  theme_classic()+
  facet_wrap(~treatment, nrow = 2)

mean <- df_2 %>% group_by(treatment) %>% summarise(mean = mean(number_of_species))
delta = mean$mean [2] - mean$mean[1] 

power.t.test(n = 20, delta = delta, sd = 10)
power.t.test(power = 0.8, delta = delta, sd = 10)
