#########First year project#############
library(tidyverse)
library(datasets)
library(tidyr)
library(dplyr)
library(modelr)
library(SimDesign)
library(RItools)
library(DeclareDesign)
library(plotrix)
library(estimatr)
library(Metrics)

setwd("~/Desktop/U of I 2020/PS 597")

envdat <- read.csv("FYProject.csv", header = TRUE)
View(envdat)

wrkdat <- envdat[3:329,]

################recoding treatment (0 = environmental conservation, 1 = energy independence)
wrkdat$FL_8_DO <- recode(wrkdat$FL_8_DO, "Treatment1" = 0, "Treatment2" = 1)


###Recoding NEP scale: nature-oriented items
wrkdat$Q8_1 <- recode(wrkdat$Q8_1, "Strongly agree" = 5, "Agree" = 4, "Unsure" = 3,
                      "Disagree" = 2, "Strongly Disagree" = 1)

wrkdat$Q8_3 <- recode(wrkdat$Q8_3, "Strongly agree" = 5, "Agree" = 4, "Unsure" = 3,
                      "Disagree" = 2, "Strongly Disagree" = 1)

wrkdat$Q8_5 <- recode(wrkdat$Q8_5, "Strongly agree" = 5, "Agree" = 4, "Unsure" = 3,
                      "Disagree" = 2, "Strongly Disagree" = 1)

wrkdat$Q8_7 <- recode(wrkdat$Q8_7, "Strongly agree" = 5, "Agree" = 4, "Unsure" = 3,
                      "Disagree" = 2, "Strongly Disagree" = 1)

wrkdat$Q8_10 <- recode(wrkdat$Q8_10, "Strongly agree" = 5, "Agree" = 4, "Unsure" = 3,
                      "Disagree" = 2, "Strongly Disagree" = 1)

wrkdat$Q8_12 <- recode(wrkdat$Q8_12, "Strongly agree" = 5, "Agree" = 4, "Unsure" = 3,
                      "Disagree" = 2, "Strongly Disagree" = 1)

wrkdat$Q8_14 <- recode(wrkdat$Q8_14, "Strongly agree" = 5, "Agree" = 4, "Unsure" = 3,
                      "Disagree" = 2, "Strongly Disagree" = 1)

wrkdat$Q8_16 <- recode(wrkdat$Q8_16, "Strongly agree" = 5, "Agree" = 4, "Unsure" = 3,
                      "Disagree" = 2, "Strongly Disagree" = 1)

###Recoding NEP scale: human use-oriented items
wrkdat$Q8_2 <- recode(wrkdat$Q8_2, "Strongly agree" = 1, "Agree" = 2, "Unsure" = 3,
                      "Disagree" = 4, "Strongly Disagree" = 5)

wrkdat$Q8_4 <- recode(wrkdat$Q8_4, "Strongly agree" = 1, "Agree" = 2, "Unsure" = 3,
                      "Disagree" = 4, "Strongly Disagree" = 5)

wrkdat$Q8_6 <- recode(wrkdat$Q8_6, "Strongly agree" = 1, "Agree" = 2, "Unsure" = 3,
                      "Disagree" = 4, "Strongly Disagree" = 5)

wrkdat$Q8_9 <- recode(wrkdat$Q8_9, "Strongly agree" = 1, "Agree" = 2, "Unsure" = 3,
                      "Disagree" = 4, "Strongly Disagree" = 5)

wrkdat$Q8_11 <- recode(wrkdat$Q8_11, "Strongly agree" = 1, "Agree" = 2, "Unsure" = 3,
                      "Disagree" = 4, "Strongly Disagree" = 5)

wrkdat$Q8_13 <- recode(wrkdat$Q8_13, "Strongly agree" = 1, "Agree" = 2, "Unsure" = 3,
                       "Disagree" = 4, "Strongly Disagree" = 5)

wrkdat$Q8_15 <- recode(wrkdat$Q8_15, "Strongly agree" = 1, "Agree" = 2, "Unsure" = 3,
                       "Disagree" = 4, "Strongly Disagree" = 5)

##############make all of the columns numeric
wrkdat$Q8_1 <- as.numeric(wrkdat$Q8_1)
wrkdat$Q8_2 <- as.numeric(wrkdat$Q8_2)
wrkdat$Q8_3 <- as.numeric(wrkdat$Q8_3)
wrkdat$Q8_4 <- as.numeric(wrkdat$Q8_4)
wrkdat$Q8_5 <- as.numeric(wrkdat$Q8_5)
wrkdat$Q8_6 <- as.numeric(wrkdat$Q8_6)
wrkdat$Q8_7 <- as.numeric(wrkdat$Q8_7)
wrkdat$Q8_9 <- as.numeric(wrkdat$Q8_9)
wrkdat$Q8_10 <- as.numeric(wrkdat$Q8_10)
wrkdat$Q8_11 <- as.numeric(wrkdat$Q8_11)
wrkdat$Q8_12 <- as.numeric(wrkdat$Q8_12)
wrkdat$Q8_13 <- as.numeric(wrkdat$Q8_13)
wrkdat$Q8_14 <- as.numeric(wrkdat$Q8_14)
wrkdat$Q8_15 <- as.numeric(wrkdat$Q8_15)
wrkdat$Q8_16 <- as.numeric(wrkdat$Q8_16)

#############create the NEP total variable
wrkdat$NEP_total <- rowSums(wrkdat[ , c(17:23, 25:32)], na.rm=TRUE)

############create and add the support index
wrkdat$Q4 <- recode(wrkdat$Q4, "Very Likely" = 5, "Likely" = 4, 
                    "Neither Unlikely Nor Likely" = 3, "Unlikely" = 2, "Very" = 1)

wrkdat$Q20 <- recode(wrkdat$Q20, "Very Likely" = 5, "Likely" = 4, 
                    "Neither Unlikely Nor Likely" = 3, "Unlikely" = 2, "Very" = 1)

wrkdat$support <- rowSums(wrkdat[ , c(15:16)], na.rm=TRUE)

#########convert variables to factor
wrkdat$FL_8_DO <- as.factor(wrkdat$FL_8_DO)
wrkdat$Q9 <- as.factor(wrkdat$Q9)
wrkdat$Q10 <- as.factor(wrkdat$Q10)
wrkdat$Q11 <- as.factor(wrkdat$Q11)
wrkdat$Q12 <- as.factor(wrkdat$Q12)
wrkdat$Q13 <- as.factor(wrkdat$Q13)
wrkdat$Q14 <- as.factor(wrkdat$Q14)
wrkdat$Q15 <- as.factor(wrkdat$Q15)
wrkdat$Q18 <- as.factor(wrkdat$Q18)
wrkdat$Q20.1 <- as.factor(wrkdat$Q20.1)

######separating into two conditions
treatment1 <- wrkdat %>% filter(FL_8_DO == 0)

treatment2 <- wrkdat %>% filter(FL_8_DO == 1)

############checking for covariate balance
xb1 <- xBalance(support~ Q10 + Q11 + Q12 + Q13 + Q14 
                + Q15 + Q18 + Q20.1, na.rm= FALSE, data = wrkdat)
############No notable difference in the samples

###########Is there a difference in support between the groups?
lm_treat1 <- lm(support ~ FL_8_DO, data = wrkdat)
summary(lm_treat1)

rmse(lm_treat1, wrkdat)
bias(lm_treat1)

#########Is there a difference in values in between the two treatment groups
difference_in_means(
  NEP_total~FL_8_DO,
  wrkdat,
  ci = TRUE,
  alpha = 0.05
) ############no, there is not, so priming likely wasn't present

#########difference in means between the two groups
difference_in_means(
  support~FL_8_DO,
  wrkdat,
  ci = TRUE,
  alpha = 0.05
)
lm_robust(support ~ FL_8_DO, data = wrkdat)
########calculate the power of a difference of means test
1- power.t.test(n = 327, delta = -.002356, sd = .20272, sig.level = 0.05)

#############a bootstrap that calculates p values for difference in frame support

boot_327 <- replicate(1000, {
  dat_327 <- wrkdat[sample(1:nrow(wrkdat), 327, replace = TRUE), ]
  t.test(support ~ FL_8_DO, data = dat_327)$p.value
})

FPR <- sum(boot_327 <.05, na.rm=TRUE)/(sum(boot_327 <.05, na.rm=TRUE)+
                                         sum(boot_327 >.05, na.rm=TRUE))
#####the FP rate is .051

#####################bootstrap to calculate bias and MSE of frame support
boot_diff_support <- replicate(1000, {
  diff1 <- treatment1[sample(1:nrow(treatment1), 327, replace = TRUE), ]
  diff2 <- treatment2[sample(1:nrow(treatment2), 327, replace = TRUE), ]
  mean(diff1$support) - mean(diff2$support)
})

alpha = .05
quantile(boot_diff_support, c(alpha/2, 1 - alpha/2))
bias1 <- bias(.002, boot_diff_support) ######bias of test
var1 <- var(boot_diff_support)  ######variance of test
MSE1 <- (bias1)^2 + var1
##########################################################################

######test for power
1 - pwr.norm.test(d = -.0017, n = 327, sig.level = 0.05)


###########bootstrapping for the difference in coefficients of the linear regression line
boot_line_diff <- replicate(1000, {
  trt1_samp <- treatment1[sample(1:nrow(treatment1), 327, replace = TRUE), ]
  trt1_lm <- lm(support ~ NEP_total, data = trt1_samp)
  trt1_sum <- summary(trt1_lm)
  trt2_samp <- treatment2[sample(1:nrow(treatment2), 327, replace = TRUE), ]
  trt2_lm <- lm(support ~ NEP_total, data = trt2_samp)
  trt2_sum <- summary(trt2_lm)
  trt1_sum$coefficients[2,1] - trt2_sum$coefficients[2,1] 
})

quantile(boot_line_diff, c(alpha/2, 1 - alpha/2))
bias2 <- bias(-.002, boot_line_diff)
var2 <- var(boot_line_diff)
MSE2 <- (bias2)^2 + var2

#Actual treatment effect
actual1 <- lm(support ~ NEP_total, data = treatment1)
actual2 <- lm(support ~ NEP_total, data = treatment2)
actual1_sum <- summary(actual1)
actual2_sum <- summary(actual2)

actual1_sum$coefficients[2,1] - actual2_sum$coefficients[2,1]

#################power for difference in linear model slopes
1 - power.t.test(n = 327, delta = -.002109, sd = .20272, sig.level = 0.05)

#################false positive rate for difference in linear model slopes

boot_line_p <- replicate(1000, {
  samp <- factor(sample(rep(c("A","B"), each = 100)))
  d1 <- data.frame(y = c(2,5)[as.numeric(samp)] + (0.5 * (1:100)) + rnorm(100),
                  x = 1:100,
                   g = samp)
  m1 <- lm(y ~ x * g, data = d1)
  m1.null <- lm(y ~ x + g, data = d1)
  result <- anova(m1.null, m1)
  result[2,6]
})

FPR2 <- sum(boot_line_p <.05, na.rm=TRUE)/(sum(boot_line_p <.05, na.rm=TRUE)+
                                         sum(boot_line_p >.05, na.rm=TRUE))

##########################################################################
#########Just a few tests to see what the data is doing
#########Treatment 1 is environmental conservation; treatment 2 is energy independence
treatment1 <- wrkdat %>% filter(FL_8_DO == "Treatment1")

treatment2 <- wrkdat %>% filter(FL_8_DO == "Treatment2")

mean(treatment1$support)
mean(treatment2$support)

cor.test(treatment1$NEP_total, treatment1$support)
cor.test(treatment2$NEP_total, treatment2$support)


########trying DeclareDesign, not successfully
pop <- declare_population(wrkdat)
outcomes <- declare_potential_outcomes(Y_Z_0 = wrkdat$support, Y_Z_1 = Y_Z_0 + 2)
theassignment <- declare_assignment(prob = 0.5)
est1 <- declare_estimator(mean(Y_Z_1 - Y_Z_0), inquiry = 'Average TE') 
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

revealY <- declare_reveal(Y,Z)

design <- pop +  outcomes + estimand  + theassignment + est1 + revealY
draw_data(design)

simulation_df <- simulate_design(design)

default_diagnosands <- declare_diagnosands(
  bias = mean(est1 - estimand),
  rmse = sqrt(mean((est1 - estimand) ^ 2)),
  power = mean(p.value <= .05),
  coverage = mean(estimand <= conf.high & estimand >= conf.low),
  mean_estimate = mean(est1),
  sd_estimate = sd(est1),
  mean_se = mean(std.error),
  mean_inquiry = mean(estimand)
)

diagnosis <- diagnose_design(design,
                             bootstrap_sims =100,  diagnosands = default_diagnosands)


###################this declare design code no longer works today, although it used to
design <-    
  declare_population(wrkdat) +
  declare_potential_outcomes(Y ~ support * 2 * Z + support)  +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(prob = 0.5) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, model= lm_robust, inquiry= "ATE")

diagnosands1 <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05),
  coverage = mean(estimand <= conf.high & estimand >= conf.low),
  type_s_rate = mean((sign(estimate) != sign(estimand))[p.value < alpha]),
  mean_se = mean(std.error)
)

diagnosis <- diagnose_design(design, diagnosands = diagnosands1, sims=1000)
diagnosis

