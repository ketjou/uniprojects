# THIS CODE IS MY OWN WORK, IT WAS WRITTEN WITHOUT CONSULTING

# STUDENT NAME: Jouni Kettunen 


# generate a vector with the given accessions
FGF2s <- c("NP_001997.5", "NP_001103711.1", "NP_990764.1", "NP_062178.1", "NP_032032.1", "XP_003432529.1")

library(rentrez)

otherFGF2 <- entrez_fetch(db = "protein", id = FGF2s, rettype="fasta")

# make it into list to write into computer
anotherFGF2 <- list(otherFGF2)

#use the data.tables library to write file
library(data.table)

# write the file to computer¨with fwrite (Stack Overflow tells that it is fastest method)
fwrite(anotherFGF2, file = "fastaFGF2.fasta")

# use the seqinr to
library(seqinr)

# read the file from computer
FGF2fasta <- read.fasta(file = "fastaFGF2.fasta",seqtype = "AA")

# get the headers
getAnnot(FGF2fasta)

library(stringr)

# tell that we are searching []
chars <- '\\[.+]'

# extract the names from the annotation
names <- str_extract(unlist(getAnnot(FGF2fasta)),chars)

# remove the [ brackets ]
names <- gsub(pattern = "[[:punct:]]",names, replacement="")
# the "[[:punct:]]" contains the all punctuation marks (incl. []) in reg ex

# use the biomaRt library to get data
library(biomaRt)

# make a mart object before use
mart <- useMart("ENSEMBL_MART_ENSEMBL",dataset="hsapiens_gene_ensembl")

# save the data to vector
toStats <- getSequence(FGF2fasta)

# make a 3x2 canvas
par(mfrow=c(3,2))

# create plots with loop
# title creates the headers
for (i in 1:6) {
  AAstat(toStats[[i]])
  title(names[i])
}
# change back to 1x1 canvas
par(mfrow=c(1,1))

# edit:
# I tried this with the ape package as it is also a list formatted
# and voilá, works so beautifully

# trace(read.GenBank, edit = TRUE)
# FGF2seq<-read.GenBank(access.nb = FGF2s,as.character=T)
#for (i in 1:6) {
 # AAstat(toupper(FGF2seq[[i]]))
  # title(names[i])
#}