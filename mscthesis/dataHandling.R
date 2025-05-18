# NOTE:most probably an easier way exists, but this works too

# This chunk here does it all for you:

# first read from the gzip file 
# use list apply to apply function to all lists
# remove geometry file from the columns
# retrieve only slide name and class label from objects
# bind the column rows, use table to count values
# save to data frame, group them by name, count percentages 
# of pos and neg, attach percent to a new column, 
# round value as whole number with two decimal places,finally save the data

library(dplyr)

# open the saved file as a gzi index
kis <- gzfile('allKIs.rds')

all_ki_freqs <- 
  readRDS(kis) %>%
  lapply(. %>% .[["ellipses"]] %>%
           removeGeometries() %>%
           select(slide_name, class_label)) %>%
  bind_rows() %>%
  table() %>%
  as.data.frame() %>%
  group_by(slide_name) %>%
  mutate(AI_perc = Freq / sum(Freq)*100) %>%
  mutate_if(is.numeric, round, 2)


# save the file for future needs
saveRDS(all_ki_freqs, file = 'ki_freqs.rds')

# read the previously saved file
ki_freqs <- readRDS('ki_freqs.rds')

# this is done a bit on the hard way, but idea is to get
# negative and positive cells separately

posi <- subset(ki_freqs, class_label == 'Positive')
neg <- subset(ki_freqs, class_label == 'Negative')

# into same dataframe, combined by slide name column
posneg <- merge(posi, neg, by = 'slide_name')

# Rename the columns and drop label names
posneg <-  plyr::rename(posneg, c('Freq.x' = 'pos_cell', 'perc.x' = 'pos_perc', 'Freq.y' = 'neg_cell', 
              'perc.y' = 'neg_perc')) %>% select(-2,-5)

# for some reason can get dplyr to work, hence basic R solution for sum
posneg$cell_totalsum <- posneg$pos_cell+posneg$neg_cell

# for the final df, some additional info is needed
# open the sheet for extracting data
library(readxl)
Data_Ki67 <- read_excel("Data_Ki67.xlsx")
survival_times <- read_excel("survival_times.xlsx")

# make a selection of the columns to be used with dpylr

sub_Ki67 <-
  select(Data_Ki67, 'Project_ID', 'AI WSI Image Name', Diagnosis,
         '5_y_survival','HUS Reference', 'age_at_sampling')

# survival info needs to be filtered to get only values of interest
clean_surv <- survival_times %>%
  filter(ID %in% sub_Ki67$Project_ID) %>%
  select(ID, 'sample_age_when_deceased')

# ages are in character format, change to numeric
clean_surv$sample_age_when_deceased <- as.numeric(clean_surv$sample_age_when_deceased)

# merge DF's in two pieces as the -AI frame is missing case id's
 
inter_df <- merge(clean_surv, sub_Ki67, by.x='ID',
                  by.y='Project_ID')

final_df <- merge(posneg, inter_df, by.x='slide_name',
                  by.y='AI WSI Image Name')


# generate 129 random ID's with numbers and capital letters
n <- 129
random_id <- stri_rand_shuffle(stri_paste(
  stri_rand_strings(n, 1, '[0-9]'),
  stri_rand_strings(n, 1, '[A-Z]'),
  stri_rand_strings(n, sample(2,129, replace=TRUE), '[A-Z0-9]')
  ))

# select reference ID and new id columns to display the coding
ID_keys <- final_df %>% mutate(id2 = random_id) %>%  
  select(7,13)

# Make an end event column to hold 0 and 1, 
# depending if the event occurred (1) or not (0), 
# replace all that aren't NA with 1 and NA's with 0.
# Make a group column to hold 0 and 1 depending
# if HUS reference value is above 0.14 it's positive (1), else negative (0).
# Make end time column presented in months, assign NA's with 60 months (5*12),
# else assign 12 * variables value.
# Assign the new ID's to column ID2 and 
# drop the slide name, reference ID, class label, 5y survival
# Move the ID2 column to first.

cleaned_final <- final_df %>% 
  mutate(end_event = ifelse(is.na(sample_age_when_deceased), 0, 1)) %>%
  mutate(ki67_group = ifelse(`HUS Reference`>= 0.14, 1, 0)) %>%
  mutate(id2 = random_id) %>%
  mutate(endtime = ifelse(is.na(sample_age_when_deceased), 60, 
                       sample_age_when_deceased*12)) %>% 
  mutate_if(is.numeric, round, 2) %>%
  select(-1, -7,-10) %>%
  relocate(id2) 

# having some problems again with dplyr so decided to do the hard way
# change the column to be in same format as in AI percentages
cleaned_final$`HUS Reference` = cleaned_final$`HUS Reference`*100
names(cleaned_final)[9] <- 'pos_ref_perc'


# save both files for future needs
saveRDS(cleaned_final, file = 'cleaned_KI67.rds')
saveRDS(ID_keys, file = 'ID_keys.rds')

#Mieti uudelleen####
# Olisiko Case fatality rate ratkaisu #####
# https://en.wikipedia.org/wiki/Case_fatality_rate
# just grouping


cfrs = KI67  %>% group_by(pos_ref_perc)  %>%
  summarise(total_deaths = sum(end_event), mean_age = mean(age_at_sampling), 
            koko = n(),
            .groups = 'drop')  %>% 
  mutate(riski = (total_deaths/sum(KI67$ki67_group==1)*100))

all_cfrs <- merge(cfrs, KI67, by='pos_ref_perc')


         