# THIS CODE IS MY OWN WORK, IT WAS WRITTEN WITHOUT CONSULTING

# STUDENT NAME: Jouni Kettunen 

#### TASK1 ####

# get an overview of the Theoph dataset with help
help(Theoph)

# the dose is given in the datasets "Dose" column, only timespan and n are mentioned in text
# "Twelve subjects were given oral doses of theophylline then serum concentrations were measured at 11 time points over the next 25 hours."

#returns the sturcture of dataset and telling it's data.frame and etc.
str(Theoph)


#inspecting the first six values
head(Theoph)

# get a summary of the statistics 
summary(Theoph)

#### TASK2 ####

# save the reference to the dataset to a local variable for further manipulation
Theo_data <- Theoph

# At first I only managed to do the plot that was in the help page

# here's what they use in the example of the help page of Theoph (except show.given is FALSE)
coplot(conc ~ Time | Subject, data = Theoph, show.given = TRUE)
# in a way I think this is quite handy, but not that clear by default

# Then I saw that you used the ggplot and decided to have a go

# Load ggplot and dplyr to use piping
library(ggplot2)
library(dplyr)

# pipe the dataframe, group it and color it by the Subjects. All in one graph.
ggplot(Theo_data %>% group_by(Subject), 
       aes(x = Time, y = conc, color = Subject)) + geom_line() + ggtitle("Teophilin concentrations")
# This is the best!


# For loop is still a mystery, it has been always the most difficult thing to me in all languages

par(mfrow = c(3,4))

# I understand the example
for (i in 1:12){
  plot(i*c(1:10))
}
# but how to implement, no idea.

# But during these desperate coding years I've tried to rely on Mr. Google and Stackoverflow. 
# And here's what I found out. Not that I truly understand, but still.

# Make canvas in size 3x4
par(mfrow = c(3, 4))

# Make an ordered data frame
Theo_data2 <- Theoph%>% group_by(Subject)

# for x in unique() gives all unique values (levels should work also)
for (cat in unique(Theo_data2$Subject)){
  
  # this subsets the data by selecting the corresponding Subject
  d <- subset(Theo_data2, Theo_data2$Subject == cat)
  
  # and this plots the subjects data conc~time manner
  plot(d$Time, d$conc, xlim = range(d$Time), ylim = range(d$conc),
       
       # couldn't extract the Subject info from the data frame, so I used the looping variable
       # this naturally works only in these 1 to something plots
       main = paste("Plot of Subject: ", cat))
}

# return the canvas size
par(mfrow = c(1,1))

#### TASK3 ####

# Three individual dataframes

# dose is constant so no need for max and mean, just pick the values with either or
theoDose <- aggregate(list(Dose = Theo_data$Dose),by=list(Subject = Theo_data$Subject),mean)

# Max and Mean for the concentration
theoMaxC <- aggregate(list(maxC = Theo_data$conc),by=list(Subject = Theo_data$Subject),max)
theomeanC <- aggregate(list(meanC = Theo_data$conc),by=list(Subject = Theo_data$Subject),mean)

### TASK 4 ###

# Fo easier operations, combine the three data frames (only value columns needed from last 2)

doseConcentration <- data.frame(theoDose, maxC = theoMaxC$maxC, meanC = theomeanC$meanC)

# Investigate the relationships of dose vs concentration with ggplot

# plot dose vs. max concentration, gray area (to my understanding) is the SD
ggplot(doseConcentration %>% group_by(Subject) 
       ,(aes(x = maxC, y = Dose))) + geom_point() + geom_smooth(method = "lm") 

# Plot dose vs mean concentration. No need to re-pipe, as the frame is already in
ggplot(doseConcentration,(aes(x = meanC, y = Dose))) + geom_point() + geom_smooth(method = "lm")

# Really ugly looking dispersion with several outliers!

# Relationship between max and mean concentration
ggplot(doseConcentration,(aes(x = meanC, y = maxC))) + geom_point() + geom_smooth(method = "lm")
# This is something to be expected and to occur. Nicely plotted around the line.

### Task 5 & 6 ###

# After googling decided to use easier way to combine correlation and plot directly

# load to make some graphs and correlations
library(ggpubr)

# pearson correlation and the graph for the values
ggscatter(doseConcentration, x = "meanC", y = "maxC", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "Concentration mean", ylab = "Concentration maximum")

ggscatter(doseConcentration, x = "meanC", y = "Dose", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean concentration", ylab = "Dose")

# for some reason the colors dont work properly and the graphs are bit archaic 
ggscatter(doseConcentration, x = "maxC", y = "Dose", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "Max concentration", ylab = "Dose", palette = c("springfield"))

# to sum it up: only meanc and maxC had a statistically significant correlation.
# rest two had a weak positive correlation

