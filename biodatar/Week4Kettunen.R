# THIS CODE IS MY OWN WORK, IT WAS WRITTEN WITHOUT CONSULTING

# STUDENT NAME: Jouni Kettunen

# set a new working directory for this week
setwd("~/BioR21/Week4")

# create a matrix with 100000 rows and 20 columns and fill it with random numbers
# had to use calculator to check the amount of needed numbers
mymat <- matrix(rnorm(2000000), ncol = 20, byrow = TRUE)

# empty vectors for the p-values
p5 <- c()
p10 <- c()

#for loop to calculate the t-test with 5 first rows
for (i in 1:nrow(mymat)) {
  k <- (t.test(mymat[i,1:5],mymat[i,6:10]))
  output <- k$p.value
  p5 <- c(p5, output)
}
# attach the p-values holding p5 vector to matrix
mymat <- cbind(mymat, p5 = p5)

#adjust the p-value and attach it to the matrix
mymat <- cbind(mymat, p5adjust = (p.adjust(p5)))


#for loop to calculate the t-test with 10 rows
for (i in 1:nrow(mymat)) {
  x <- (t.test(mymat[i,1:10],mymat[i,11:20]))
  output <- x$p.value
  p10 <- c(p10, output)
}

# attach the p-values holding p10 vector to matrix
mymat <- cbind(mymat, p10 = p10)
#adjust the p-value and attach it to the matrix
mymat <- cbind(mymat, p10adjust = (p.adjust(p10)))

#transform the matrix to dataframe for easier handling
myframe <- as.data.frame(mymat)

#find the number of significant p-values 
sum(myframe$p5<0.05)
sum(myframe$p10<0.05)
sum(myframe$p5adjust<0.05)
sum(myframe$p10adjust<0.05)


### These are some alternative solutions I tried ###

# I created a subset of the dataframe, but this didn't
# work as I thought. If i use AND operator between columns I don't get
# any values as the condition is impossible. 
# If I use the OR, I get also some values that aren't < 0.05

newframe <- subset(myframe, p5 < 0.05 | p10 < 0.05 | p5adjust < 0.05 | p10adjust < 0.05,
                   select = c("p5", "p10", "p5adjust", "p10adjust"))

# the frame is filtered to 9043 rows 
nrow(newframe)

# and the values are here
sum(newframe$p5<0.05)

#selecting columns one by one would've been possibility
this <- subset(myframe, p10adjust < 0.05, select = "p10adjust")

