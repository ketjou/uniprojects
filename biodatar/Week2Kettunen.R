# THIS CODE IS MY OWN WORK, IT WAS WRITTEN WITHOUT CONSULTING
# STUDENT NAME: JOUNI KETTUNEN

# this wasn't that hard to do on my own
# for some reason I have often these kind of "ignition problems"

# like looping: I've done it in Python and Java and R basic course
# but for some reason felt like someone had pressen red button
# and erased all my knowledge


# set a new working directory for the exercises
setwd("~/BioR21/Week2")
# used the R-studios automatic function to get the .csv file
# import dataset -> from text -> heading yes -> row names automatic 
furin_data <- read.delim("~/BioR21/furin_data.csv")

# empty vectors a & b
# bit stupid names, but didn't want to change 
# as I had them already defined
a <- c()
b <- c()
#start for loop, range needs to be a vector
# i.e. 1 to number of rows
for (i in 1:nrow(furin_data)) {
  # assign the filter value to a variable
  k <- furin_data[i,3]/furin_data[i,5]
  # use the variable for filtering
  if((k > 2) | (k < 0.5)) {
    # save the k to a local variable
    outputk <- k
    # concatenate the previously defined vectors and local variables
    b <- c (b, outputk)
    # b holds the values
    nams <- furin_data$NAME[i]
    # a holds the names
    a <- c(a,nams)
    # finally print the names that fullfill the conditions
    print(furin_data$NAME[i])
  }
}

mydf <- data.frame(fraction = b)
#create a data frame with 1 column (ratio values)

# JUST FOR PRACTICE, CODE BELOW


help("make.names")
# use make.names to transform duplicates to unique ids
# data frame can't have duplicate id's
rownames(mydf) = make.names(a, unique = TRUE)


#rename the data frame 1st column (note that starts from 1 not 0)
names(mydf)[1] <- "ratio"
#write the .csv to a data frame
write.csv(mydf, "filteredDF.csv")

