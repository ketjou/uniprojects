# THIS CODE IS MY OWN WORK, IT WAS WRITTEN WITHOUT CONSULTING

# STUDENT NAME: Jouni Kettunen




# A data frame with 25 rows and 3 columns

elisa.data <- data.frame(matrix(NA, nrow = 25, ncol = 3))

# name the columns
names(elisa.data)[1] <- "label"
names(elisa.data)[2]<- "OD"
names(elisa.data)[3] <- "conc"

# fill the first column with names
elisa.data$label <- c(rep("ref", 5), rep("treat",10), rep("cont",10))

# a random number for optical density
my_OD <- c(runif(1, min=1, max=100))

# five known values (controls)
# diluted by 10
for (i in 2:5){
  my_OD[i] <- my_OD[i-1]/10
}
# attach them to the OD column
elisa.data$OD[1:5] <- my_OD

# fill the rest part of OD with random numbers
elisa.data$OD[6:25] <- runif(20)

# an imaginary concentration
# and diluttions of it by 10
ref_conc <- 150 
for (i in 2:5) {
  ref_conc[i] <- ref_conc[i-1]/10
}
#attach the values to the reference column
elisa.data$conc[1:5] <- ref_conc 

# load DRC library
library(drc)

# create a model for the measurements
elisamodel <- drm(formula = OD ~ conc, 
                  data = elisa.data[1:5,], fct = LL.4())


# a new data frame with values from the original dataset 
elisa.data_predicted <- elisa.data[6:25,]

# fill the concentration column with values predicted on the basis of known values
elisa.data_predicted$conc <- predict(elisamodel, data.frame(OD=elisa.data$OD[6:25]))

# create a boxplot to compare the groups 
boxplot(conc ~label, data = elisa.data_predicted)

