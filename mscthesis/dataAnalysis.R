# packages for the analysis
library(survival)
library(ggsurvfit)
library(gtsummary)
library(dplyr)
library(ggplot2)



# The Surv() function from the {survival} package creates a survival object 
# for use as the response in a model formula, + if censored
# check first ten

Surv(ki67$Endtime, ki67$End_event)[1:10]

# The survfit() function creates survival curves using the Kaplan-Meier 
# survival curve for the entire cohort, assign it to object s1, str to check it

# time: timepoints at which the curve has a step, i.e. at least one event 
# surv: estimate of survival at the corresponding time

s1 <- survfit(Surv(Endtime, End_event) ~ 1, data = ki67)
str(s1)

# use ggsurvfit to draw curves, first for the whole shebang 
# then for positive and negative groups separately (not informative or neg)
# add the confidence interval 
# and numbers at risk in a table below the x-axis

survfit2(Surv(Endtime, End_event) ~ 1, data = ki67) %>% 
  ggsurvfit() +
  labs(
    x = "Months",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() + 
  add_risktable()

survfit2(Surv(Endtime, End_event) ~ KI67_group, data = ki67) %>% 
  ggsurvfit() +
  labs(
    x = "Months",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() + 
  add_risktable()

# 5y overall surv as a table 
survfit2(Surv(Endtime, End_event) ~ KI67_group, data = ki67)  %>% 
  tbl_survfit(
    times = 60,
    label_header = "**5-year survival (95% CI)**"
  )

# 1 and 2 year
survfit(Surv(Endtime, End_event) ~ 1, data = ki67) %>% 
  tbl_survfit(
   # probs = 0.5,
    times = c(12, 24),
    label_header = "**{time} Month**"
  )

# median plotting, but:
# Median survival is the time it takes to reach 50% survival. 
# If more than 50% of the subjects are alive at the end of the study, 
# then the median survival time is simply not defined.
tbl_survfit_ex2 <- tbl_survfit(
  ki67,
  y = Surv(Endtime, End_event),
  include = KI67_group,
  probs = 0.5,
  label_header = "**Median Survival**"
)
tbl_survfit_ex2


#  test whether there was a difference in survival time according to group
# see https://www.westga.edu/academics/research/vrc/assets/docs/ChiSquareTest_LectureNotes.pdf
survdiff(Surv(Endtime, End_event) ~ KI67_group, data = ki67)




# cox won't probably do nothing, as the control group has 0 events 
# see https://stackoverflow.com/questions/19369314/r-coxph-warning-loglik-converged-before-variable



# taulukolla jotain, en tiia mita 

joku <- table(ki67$End_event, ki67$KI67_group, ki67$Ref_perc)

# fit univariable and multivariable regression models that have survival outcomes.
coxph(Surv(Endtime, End_event) ~ KI67_group, data = ki67)
# coefficient of 20 would be unrealistically large in most instances,
# my data 19,57


# exponentiate set to TRUE to return the hazard ratio rather than the log hazard ratio:
coxph(Surv(Endtime, End_event) ~ KI67_group, data = ki67) %>% 
  tbl_regression(exp = TRUE) 

# Cox regression model interest:
# is a hazard ratio (HR) between two groups at any particular point in time
# It is not a risk!!
# instantaneous rate of occurrence of the event of interest in those who are still at risk for the event. 
# A HR < 1 indicates reduced hazard of death whereas a HR > 1 indicates an increased hazard of death
# So the HR = 0.59 implies that 0.59 times as many females are dying as males, at any given time. 
# Stated differently, females have a significantly lower hazard of death than males in these data
