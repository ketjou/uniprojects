# Following the examples of 
# Cattaneo & Titiunik (2021) in NBER Summer Institute Methods Lectures, July 2021
# https://github.com/rdpackages-replication/CT_2021_NBER/blob/main/CT_2021_NBER.R
# Andrew Heiss from the Andrew Young School of Policy Studies (2020)
# https://evalf20.classes.andrewheiss.com/example/rdd/


# First load the libraries needed 

library(ggplot2) # plotting the models
library(rdlocrand)# local randomization methods for RDD
library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(broom)  # Convert models to data frames
library(rdrobust)  # For robust nonparametric regression discontinuity
library(rddensity)  # For nonparametric regression discontinuity density tests
library(modelsummary)  # Create side-by-side regression tables
library(Hmisc) # for on the fly calculations to the plots
library(TeachingDemos)# to save output into txt files
library(ggpubr)

# read the previously saved file
KI67_1 <- readRDS('KI67_2.rds')

# if not numeric:
is_all_numeric <- function(x) {!any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)}
KI67_1 <- KI67_1 %>%
mutate_if(is_all_numeric,as.numeric)
sapply(KI67_1, class)


# Calculate case fatality frequencies (CFR) based on HUS reference values: 
# first group the observations by the percentage value
# and then calculate the CFR's for the groups, and mean age for comparison
cfrs = KI67_1  %>% group_by(pos_ref_perc)  %>%
  summarise(total_deaths = sum(end_event), mean_age = mean(age_at_sampling), 
            koko = n(),
            .groups = 'drop')  %>% 
  mutate(risk = (total_deaths/sum(KI67_1$ki67_group==1)*100))

# print the cfr's

txtStart(file = "./output/ref_cfrs.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
cfrs %>% as_tibble() %>% print(n=Inf)
txtStop()


# Merging by the reference percentage copies the CFR's for all observations
all_cfrs <- merge(cfrs, KI67_1, by='pos_ref_perc')

# Cattaneo & Titiunik recommend attaching DF columns to vector variables:
# Y is the outcome, the CFR
Y <-all_cfrs$risk

# X is the running variable, the KI67% by HUS
X <- all_cfrs$pos_ref_perc

# C is the cutoff of the running variable
C <- 14

# Cattaneo & Titiunik recommend to always normalize the cutoff to zero 
# by centering the running variable to avoid confusion
R <- X - C

# KI67 group, pos (T) or moderate (F)
positive_ki67 <- (X>C) 

# attach the centered value and T/F to the dataframe just in case
all_cfrs$ki67_centered <- R 

all_cfrs$is_positive <- positive_ki67 

all_cfrs <- all_cfrs %>%
  mutate(group = ifelse(ki67_group > 0, 'Positive', 'Moderate'))

# At first do some overview for the data:

# 1. determine if the design is fuzzy or sharp

# OBS! The scattergram is confusing, AVOID IT

plot1 <-ggplot(all_cfrs, aes(x = X, y = positive_ki67, color = positive_ki67)) +
  # Make points small and semi-transparent for better visualization
  geom_point(size = 3, alpha = 0.5, 
             position = position_jitter(width = 0, height = 0.25, seed = 1234)) + 
  # Add vertical line to cutoff
  geom_vline(xintercept = C) + 
  # Add labels
  labs(x = "Ki-67% from HUS", y = "Belongs to positives")  + 
  # Hide the color legend (explanatory box), since it's redundant
  guides(color = 'none') + theme_pubr(base_size = 22) 

# change the colors to corresbond to those in barplot
plot1ab <- plot1b + 
  scale_color_manual(values=c("blue", "red"))

# instead use stacked barplot
# no need to determine the y, ggplot will group it automatically

plot1b <- ggplot(data = all_cfrs, 
                 aes(x = group, fill = as.factor(ki67_group), 
                     show.legend = FALSE)) + geom_bar() + 
  scale_fill_manual(values=c("blue", "red")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))

# change labels and theme
plot1ba <- plot1b + coord_flip() + theme_pubr(base_size = 22) +
  theme(legend.position = "none") +
  labs(y = "Number of observations", x = "Group")  

# 2. visualize the continuity around cutoff 
# aka. check for discontinuity in running variable around cut-point

# make a dataframe out of the R and attach column name v1 to it
tempdata = as.data.frame(R); colnames(tempdata) = c("v1");

# Determine styles, make two plots and combine them to one, 
plot2 <- ggplot(data=tempdata, aes(tempdata$v1)) + theme_bw(base_size = 17) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), 
                 breaks = seq(min(R,na.rm = TRUE), 0, 1), 
                 fill = "blue", col = "black", alpha = 1) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), 
                 breaks = seq(1, max(R,na.rm = TRUE), 1), 
                 fill = "red", col = "black", alpha = 1) +
  labs(x = "Centered Ki-67% from HUS", y = "Observations") + 
  geom_vline(xintercept = 0.0, color = "black")

plot2a <- plot2 + theme_pubr(base_size = 22) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 2)) +
  scale_x_continuous(limits = c(-15, 85), breaks = seq(-15, 85, 5)) 




ggarrange(plot1ba,plot2a, 
          labels = c("A", "B"),
          font.label = list(size = 22),
          ncol = 1, nrow = 2)


# 3. verify sharpness with a table,
# it'll output the outliers not belonging to the group
# sharp because no pos with under 15 and no moderate with over 15

txtStart(file = "./output/ref_sharpFuzzyGroups.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
all_cfrs %>% 
  group_by(ki67_group, pos_ref_perc >= 15) %>% 
  dplyr::summarize(count = n())
txtStop()

# 4. density test to check for manipulation

txtStart(file = "./output/ref_density.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
test_density <- rddensity(X,C)
summary(test_density)
txtStop()

# plot the density test
plot_density_test <- rdplotdensity(rdd = test_density, 
                                   X = all_cfrs$pos_ref_perc, 
                                   # This adds both points and lines and colors the CI region 
                                   type = "both", CIcol = c('black', 'magenta'), 
                                   xlabel = 'Ki-67%, HUS pathologist', ylabel = 'Density')

plot_density_test$Estplot + theme_pubr(base_size = 12)+ theme(legend.position = "none")

# CI's are overlapping, but huge. Nonetheless:
# The p-value for the size of that overlap is larger than 0.05 (0.5558), 
#so we don’t have good evidence that there’s a significant difference 
#between the two lines. Based on this plot and the t-statistic, we’re 
#probably safe in saying that there’s no manipulation or bunching.


# Proceed to check if there's a difference between the groups -->
# Check for discontinuity in outcome across running variable

## Local linear methods ####

# see if there’s a discontinuity and visualize the difference between groups
# can be done to original data or to the centered data as in latter
ggplot(all_cfrs, aes(x = ki67, y = risk, color = as.factor(ki67_group))) +
  geom_point(size = 0.5, alpha = 0.5) + 
  # Add a line based on a linear model for scoring greater than or equal to 15
  geom_smooth(data = filter(all_cfrs, pos_ref_perc > 14), method = "lm") +
  # Add a line based on a linear model for the people scoring 15 or more
  geom_smooth(data = filter(all_cfrs, pos_ref_perc <= 14), method = "lm") +
  geom_vline(xintercept = 14) +
  labs(x = "Ki-67% by HUS", y = "Case Fatlity Rate", color = "Ki-67") +
  theme_pubr(base_size = 22)

# Plot indicates that there's a cap, but cautiousness is advised as
# we are using _really_ small dataset

# Try to estimate the size of the gap, first parametrically by using linear reg.

# model fitted to whole data 
# usually wring as we should be only interested about the difference around C
model_simple <- lm(risk ~ ki67_centered + is_positive,
                   data = filter(all_cfrs))
# observe the model
txtStart(file = "./output/ref_simple_lm_model.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
tidy(model_simple)
txtStop()

# As mentioned, we are interested about the area around the C, thus we should 
# fit the model using bandwiths around the C.
# The size of the dataset becomes problem, when using bw's

# Small C's wont work, use bigger
# positiveki67TRUE = being positive for ki67
# increases CFR by XX points.

# like 15
model_bw_15 <- lm(risk ~ ki67_centered + is_positive,
                  data = filter(all_cfrs,
                                ki67_centered >= -15 & 
                                  ki67_centered <= 15))
tidy(model_bw_15)

# Change to 20
model_bw_20 <- lm(risk ~ ki67_centered + is_positive,
                  data = filter(all_cfrs,
                                ki67_centered >= -20 & 
                                  ki67_centered <= 20))
tidy(model_bw_20)



# Here we could compare the outcomes

modelsummary(list("Full data" = model_simple,
                  "Bandwidth = 20" = model_bw_20,
                  "Bandwidth = 15" = model_bw_15))


# both models in one plot

  ggplot(all_cfrs, aes(x = ki67_centered, y = risk, color = is_positive)) +
  geom_point(size = 0.5, alpha = 0.5) +
    
  # Add a line based on a linear model for scoring greater than or equal to 0
  geom_smooth(data = filter(all_cfrs, pos_ref_perc > 0), method = "lm",  
              se = T, size = 2) +
  # Add a line based on a linear model for the people scoring 0 or more
  geom_smooth(data = filter(all_cfrs, pos_ref_perc <= 0), method = "lm",
              se = T, size = 2) +
    
  # Add a line based on a linear model for scoring greater than or equal to 20
  geom_smooth(data = filter(all_cfrs, ki67_centered >= -20), method = "lm", 
              se = T,linetype = "dashed", size = 1) +
  # Add a line based on a linear model for the people scoring 20 or more
  geom_smooth(data = filter(all_cfrs,  ki67_centered <= 20), method = "lm",
              se = T, linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0) +
  labs(x = "Ki-67% by HUS", y = "Case Fatality Rate", color = "Ki-67") +
  scale_color_manual(name = "Ki-67% ≥ 14%", values=c("black", "magenta"))  +
  geom_vline(xintercept = 0) +
  theme_pubr(base_size = 22)

## Local Polynomial Methods ####

# The rdrobust package allows the nonparametric estimation for the data
# Cattaneo & Titiunik recommend 1st order polynomial for more realistic results


# coef indicates the change, 8 point bump is statistically significant 
# (p < 0.001; the 95% confidence interval definitely doesn’t ever include 0
# plot the 1st order polynomial RDD

# change the kernel to get some funky dataplots
out2 <- rdplot(Y, R, kernel="epanechnikov", p=1, ci = TRUE,
               x.label = "", y.label = "CFR %",
               title = 'RDD with Epanechnikov kernel'); summary(out2)
#out2$rdplot +
# labs(x = "Ki-67% by HUS", y = "CFR %")

out3 <- rdplot(Y, R, kernel="uni", p=1, ci = TRUE, 
               x.label = "", y.label = "CFR %", 
               title = 'RDD with uniform kernel'); summary(out3)

rdplot(Y, R, kernel="uni", p=1, ci = TRUE, 
       x.label = "Ki-67% by HUS", y.label = "CFR %", 
       title = 'RDD with uniform kernel', binselect="esmv")
#out3$rdplot +
# labs(x = "Ki-67% by HUS", y = "CFR %")

out4 <- rdplot(Y, R, kernel="triangular", p=1, ci = TRUE, 
               x.label = "Ki-67% by HUS", y.label = "CFR %",
               title = 'RDD with triangular kernel'); summary(out4)

epa <- out2$rdplot + theme_pubr(base_size = 12)
uni <- out3$rdplot + theme_pubr(base_size = 12)
tri <- out4$rdplot + theme_pubr(base_size = 12)

# attach the plots to a list, remember to select the plot with $
my_plot_list <- list(epa, uni,tri)



ggarrange(plotlist = my_plot_list,
          labels = c("A", "B", 'C'),
          font.label = list(size = 12),
          ncol = 1, nrow = 3)

# Presumably the limitations in the data cause the rdrobust to fail with default
# settings and rbwselect wont be able to calculate the proper window size
# adjusting the kernel and heteroskedasticity-consistent standard errors
# enable using rbwselect (values listed in the output)

# ie. adjusting only the bwselect won't make any results
summary(rdrobust(Y, X, C, bwselect = 'cercomb2'))



# with rdrobust and polynomial of 1
# this means that R will not try to fit a straight line to the data
# instead it’ll curve around the points and 
# try to fit everything as smoothly as possible.


### Uniform (ie. unweighted) kernel ####
#nn
txtStart(file = './output/ref_uni_nn.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="uni", vce="nn")) 

txtStop()

# Hayes & Cai (2007) state that: "...small sample sizes, the standard errors from HC0
# are quite biased, usually downward, and this results in
# overly liberal inferences in regression models"
txtStart(file = './output/ref_uni_hc0.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="uni", vce="hc0")) 

txtStop()

# hc1 weighted
txtStart(file = './output/ref_uni_hc1.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="uni", vce="hc1")) 

txtStop()

# hc2 weighted
txtStart(file = './output/ref_uni_hc2.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="uni", vce="hc2")) 

txtStop()

# hc3 weighted 
# Hayes & Cai (2007): "... simulation results also
# suggest the superiority of HC3 over its predecessors
# ... HC3 can have a liberal bias in very small samples

# hc3 weighted
txtStart(file = './output/ref_uni_hc3.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="uni", vce="hc3")) 

txtStop()

### Triangular (ie. weighted and default) kernel ####

# Manually adjusting the bw
summary(rdrobust(Y, X, C, h = 50))

# nn wont work with triangular
txtStart(file = './output/ref_tri_nn.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out <- summary(rdrobust(Y, X, C, kernel="triangular", vce="nn"))

txtStop()

#hc0
txtStart(file = './output/ref_tri_hc0.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out <- summary(rdrobust(Y, X, C, kernel="triangular", vce="hc0")) 

txtStop()

#hc1
txtStart(file = './output/ref_tri_hc1.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="triangular", vce="hc1")) 

txtStop()

#hc2
txtStart(file = './output/ref_tri_hc2.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="triangular", vce="hc2")) 

txtStop()

#hc3
txtStart(file = './output/ref_tri_hc3.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="triangular", vce="hc3")) 

txtStop()

### Epanechnikov (ie. weighted and according to wiki, the best) kernel ####
# Epanechnikov (more distant observations have less weight following a curve) 


# nn wont work 
summary(rdrobust(Y, R, C, kernel="epanechnikov", vce="nn")) 

#hc0
txtStart(file = './output/ref_epa_hc0.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="epanechnikov", vce="hc0")) 
txtStop()


#hc1

txtStart(file = './output/ref_epa_hc1.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="epanechnikov", vce="hc1")) 

txtStop()


#hc2
txtStart(file = './output/ref_epa_hc2.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="epanechnikov", vce="hc2")) 

txtStop() 

#hc3
txtStart(file = './output/ref_epa_hc3.txt',
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
summary(rdrobust(Y, X, C, kernel="epanechnikov", vce="hc3")) 

txtStop()


## Local Randomization Methods ####
# an optimal window can be searched with rdwinselect
# though it's hard when the data is so small

# these summarize the problem of small data, window is quite random
# and adjusting parameters is difficult as there's not enough points
# nonetheless uniform kernel gives some suggestion
tmp1 <- rdwinselect(R, Y, wmin=7, cutoff = 0
                   ,wstep=1,reps=5000, plot = TRUE,
                   kernel = "uniform")

tmp2 <- rdwinselect(X, Y, cutoff = 14,wmin = 7,
                   wstep=1,reps=5000, plot = TRUE,
                   p = 0, kernel = "uniform")

tmp1

# to non centered data with a non adjusted win size
rdrandinf(Y, X, C, reps=1000, kernel = 'uniform')

# to non centered data with a non adjusted win size and epan kernel (no diff)
rdrandinf(Y, X, C, reps=1000, kernel = 'epan')

# to non centered data with an adjusted win size (no difference)
rdrandinf(Y, X, C, wl = -1, wr = 29, reps=1000, kernel = 'epan')

# to centered data with a non adjusted win size, note that it uses
# the length of all data as a window
rdrandinf(Y, R, reps=1000)

# to  centered data with rdwinselect[ed] win size
# note it uses the centered values
rdrandinf(Y, R, wl = -15, wr = 15, reps=10000, stat="all")


## Falsification results ####
#Placebo cutoff 
# reveals that our data is not valid as it gives meaningful jumps everywhere
c30 <- rdplot(Y, X, p=1, kernel ="uniform",  c=28, x.label = "", 
       y.label = "CFR%", 
       title = 'RDD using placebo cutoff 28', binselect="esmv")

c40 <- rdplot(Y, X, p=1, kernel ="uniform",  c=42, x.label = "", 
       y.label = "CFR&", 
       title = 'RDD using placebo cutoff 42', binselect="esmv")

agerdd <- rdplot(all_cfrs$age_at_sampling, X, p=1,  c=14, ci = TRUE, 
                 kernel = 'uniform',x.label = "Ki-67% by HUS", 
                 y.label = "Age at sampling", 
                 title = 'RDD using age as an outcome', 
                 binselect="esmv")

# attach the plots to a list, remember to select the plot with $

triple <- c30$rdplot + theme_pubr(base_size = 12)
quatro <- c40$rdplot + theme_pubr(base_size = 12)
ages <- agerdd$rdplot + theme_pubr(base_size = 12)

falseplots <- list(triple,quatro,ages)



mainplot <- ggarrange(plotlist = falseplots,
          labels = c("A", "B", 'C'),
          font.label = list(size = 12),
          ncol = 1, nrow = 3)

mainplot

#annotate_figure(mainplot, top = text_grob("Falsification plots using uniform kernel", 
 #               color = "black", face = "bold", size = 14))


summary(rdrobust(all_cfrs$age_at_sampling, X, C, kernel="epanechnikov", vce="hc3")) 

# do bar plots, because:

# The quasi-experimental nature of the RDD implies that groups are 
# similar with respect to all measured and unmeasured factor
# https://murraylax.org/rtutorials/barplots.html

p3<-ggplot(data=all_cfrs, aes(x=R, y=neg_cell)) +
  stat_summary(fun.data=mean_sdl, geom="bar")
p3


# in sample size etc. estimation errors:
# be zero, or below some very small threshold); this means, essentially, 
# that your data are too noisy/small to estimate a full covariance matrix.
