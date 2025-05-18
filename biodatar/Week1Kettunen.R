# THIS CODE IS MY OWN WORK, IT WAS WRITTEN WITHOUT CONSULTING

# STUDENT NAME: JOUNI KETTUNEN 

# I use RStudio so I just clicked from the right corner below
# and set my working directory to BioR21 folder
setwd("~/BioR21/Week1")

# imported the .csv via ready made tools
# right top corner: import dataset, text, tab separators
# heading yes and Automatic (first columns)
furin.file <- read.delim("~/BioR21/furin_data.csv")

# get the six first rows
head(furin.file)

# get the last six rows
tail(furin.file)

# display the structure of furin.file
str(furin.file)

# whole data plotted
plot(furin.file)


# getting help
help("jpeg")

#just "randomly" setting some values and seeing what happens
jpeg(filename = "Naive.WT.1_plot.jpg",
     width = 480, height = 480, units = "cm", pointsize = 10,
     quality = 75, res = NA)
# Naive.WT.1 plotted
plot(furin.file$Naive.WT.1)
#shutting the jpeg
dev.off()
# and the file is saved

# might this be better than just saving the image straightly
# from the plots -tab?
