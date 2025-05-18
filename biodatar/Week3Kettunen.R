# THIS CODE IS MY OWN WORK, IT WAS WRITTEN WITHOUT CONSULTING

# STUDENT NAME: Jouni Kettunen




setwd("~/BioR21/Week3")

# First thought that these had to be separately created
# and thus left them be such

# 100.000 random numbers
vals <- rnorm(1000000)
# create a matrix containing random numbers
mym <- matrix(vals, ncol = 10, byrow = TRUE)

# just to test the structure of the matrix

a <- mym[1:5]
b <- mym[6:10]

testin <- t.test(mym[1:5], mym[6:10])

testin$p.value

# ctrl + i for indentation

#set up a counter for the values
counter = 0
# system.time to measure how long the code runs
system.time(
  # loop from 1 to number of rows
  for (i in 1:nrow(mym)) {
    # again I thought that we needed a vector for latter purpose
    # while the oneliner was nice solutio, I left this as such as it was
    # to show independent thinking
    
    # take values from every row(1 - number of rows) column-wise from 1-5 and 6-10
    savedtest <- t.test(mym[i,1:5],mym[i,6:10])
    # if the p-value is significant
    if (savedtest$p.value < 0.05) {
      #increase the counter by 1
      counter = counter +1
    }
    #print(mym[i,1:5])
  })
 print(c("Number of items", counter))
