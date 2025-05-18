# Code structured on the basis of Aiforia's Anniina Manninen and
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html

# packages for the analysis
library(survival)
library(ggsurvfit)
library(gtsummary)
library(dplyr)
library(ggplot2)
library(ggpubr)

# load the ki67 dataframe from disk and load it to ki67 variable
ki67 <- readRDS('KI67_1.rds')

ki67 <- KI67_1

ki67 <- ki67 %>%
mutate(group = ifelse(ki67_group > 0, 'Positive', 'Moderate'))
  

# The Surv() function from the {survival} package creates a survival object 
# for use as the response in a model formula, + if censored
# check first ten

Surv(ki67$endtime, ki67$end_event)[1:10]

# The survfit() function creates survival curves using the Kaplan-Meier 
# survival curve for the entire cohort, assign it to object s1, str to check it

# time: timepoints at which the curve has a step, i.e. at least one event 
# surv: estimate of survival at the corresponding time

s1 <- survfit(Surv(endtime, end_event) ~ 1, data = ki67)
str(s1)

# use ggsurvfit to draw curves, first for the whole shebang 
# then for positive and negative groups separately (not informative or neg)
# add the confidence interval 
# and numbers at risk in a table below the x-axis

survfit2(Surv(endtime, end_event) ~ 1, data = ki67) %>% 
  ggsurvfit() +
  labs(
    x = "Follow up time, months",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() + 
  add_risktable(size = 8, 
                theme = theme_risktable_default(axis.text.y.size = 22,
                                                plot.title.size = 22)) +
  theme_pubr(base_size = 22) 





fit2 <- survfit2(Surv(endtime, end_event) ~ group, data = ki67) %>% 
  ggsurvfit() +
  labs(
    x = "Follow up time, months",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() + 
  add_risktable(size = 8, 
                theme = theme_risktable_default(axis.text.y.size = 22, 
                                                plot.title.size = 22)) +
  theme_pubr(base_size = 22) + 
  theme(legend.text = element_text(size = 22, color = "black")) +
  add_legend_title('Ki-67% group')

 

fit2

# change labels
fit2 + scale_color_hue(labels = c("Moderate", "Positive")) +
  scale_fill_discrete(labels = c("Moderate", "Positive"))

KI67_1$

lLab <- gsub("x=","", c('Moderate', 'Positive'))

# 5y overall surv as a table
survfit2(Surv(endtime, end_event) ~ ki67_group, data = ki67)  %>% 
  tbl_survfit(
    times = 60,
    label_header = "**5-year survival (95% CI)**"
  )

# 1 and 2 year, groups
survfit(Surv(endtime, end_event) ~ ki67_group, data = ki67) %>% 
  tbl_survfit(
   # probs = 0.5,
    times = c(12, 24, 48, 60),
    label_header = "**{time} Month**"
  )

# 1 and 2 year, whole
survfit(Surv(endtime, end_event) ~ 1, data = ki67) %>% 
  tbl_survfit(
    # probs = 0.5,
    times = c(12, 24, 48, 60),
    label_header = "**{time} Month**"
  )

# median plotting, but:
# no sense to make such, as there isn't such
# number of deaths and time is the key in here
tbl_survfit_ex2 <- tbl_survfit(
  ki67,
  y = Surv(endtime, end_event),
  include = ki67_group,
  probs = 0.5,
  label_header = "**Median Survival**"
)
tbl_survfit_ex2


#  test whether there was a difference in survival time according to group
# see https://www.westga.edu/academics/research/vrc/assets/docs/ChiSquareTest_LectureNotes.pdf
survdiff(Surv(endtime, end_event) ~ ki67_group, data = ki67)




# cox won't probably do nothing, as the control group has 0 events 
# see https://stackoverflow.com/questions/19369314/r-coxph-warning-loglik-converged-before-variable


# fit univariable and multivariable regression models that have survival outcomes.
coxph(Surv(Endtime, End_event) ~ ki67_group, data = ki67)
# coefficient of 20 would be unrealistically large in most instances,
# my data 19,57


# exponentiate set to TRUE to return the hazard ratio rather than the log hazard ratio:
coxph(Surv(Endtime, End_event) ~ ki67_group, data = ki67) %>% 
  tbl_regression(exp = TRUE) 

# Cox regression model interest:
# is a hazard ratio (HR) between two groups at any particular point in time
# It is not a risk!!
# instantaneous rate of occurrence of the event of interest in those who are still at risk for the event. 
# A HR < 1 indicates reduced hazard of death whereas a HR > 1 indicates an increased hazard of death
# So the HR = 0.59 implies that 0.59 times as many females are dying as males, at any given time. 
# Stated differently, females have a significantly lower hazard of death than males in these data




