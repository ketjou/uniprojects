
# first set the working directory to be from local folder
setwd("~/KI67RDD")


# Aiforia has it's own library to handle image analysis data 
# built by Ville Koponen

# load the library
library(aiforiaAPIutils)

# set username for the subscription
username <- 'jouni.kettunen@aiforia.com'

# set the url address for the rign cloud environment
api_url <- 'https://api-cloudaidev.aiforia.com/'

# set the credentials needed for login, will ask password in popup
token <- getToken(username = username, api_url = api_url)


# set the subscription name to use
subscription_name <- 'BreastCancerSuite'

# image analysis in cloud are done in batches 
# one needs to get the batch id's to get the correct data
batches_cloudaidev <- getBatches(token, subscription_name, api_url = api_url)

# data is returned and saved as a data frame, can be confirmed by
data.class(batches_cloudaidev)

# to get the id of the batch needed, filter the results by name column 
# name Jouni was used in the run, hence the search for pattern Jouni

# load stringr library for filtering
library(stringr)

# get only the run where name Jouni was used
jouniRun <- batches_cloudaidev[str_detect(batches_cloudaidev$name, "Jouni"), ]
# attach the id from batchID to batch_id
batch_id <- jouniRun$batchId

# "52617b58-1189-4425-92e3-89948c77c50a"

# get the results from the run
# leave metrics and regions out, they aren't needed
# Values positive and negative appear only in the ellipses aka. objects
batchResults <- getBatchResults(token, subscription_name, 
                                api_url = api_url,
                                batch_id = batch_id,
                                get_coordinates = FALSE,
                                result_filter = list(classes = c('Positive', 'Negative')))

# save the file for future needs
saveRDS(batchResults, file = 'allKIs.rds')