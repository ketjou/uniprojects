# packages for the analysis
library(psych)
library(dplyr)
library(ggplot2)
library(ggpubr)
# read previous dataset and attach it to variable

KI67_1<- readRDS('cleaned_KI67.rds')


# if not numeric:
is_all_numeric <- function(x) 
  {!any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)}

KI67_1 <- KI67_1 %>%
  mutate_if(is_all_numeric,as.numeric)
sapply(KI67_1, class)


#overall descriptive statisics
txtStart(file = "./output/groupCharacteristics.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
describe(KI67_1)
txtStop()

# get some descriptive statisics based on Ki67 group
describeBy(KI67_1, group = KI67_1$ki67_group)

describeBy(KI67_1, group = KI67_1$end_event)

# geom_boxplot proposes several arguments to custom appearance
p <- ggplot(KI67_1, aes(x=as.factor(ki67_group), y=age_at_sampling,)) + 
  geom_boxplot(
    
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
    
  )

# adjust the font size and the labels text
p1 <- p + labs(x = "Ki-67% group", y = 'Age at sampling') + 
   theme_pubr(base_size = 22)+theme(legend.position="top") +
  scale_x_discrete(breaks = c("0", "1"), 
                   labels = c("Moderate", "Positive"))

p1


p2 <- ggplot(KI67_1, aes(x=as.factor(ki67_group), y=pos_perc,)) + 
  geom_boxplot(
    
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    
    # Notch?
    notch=FALSE,
    #notchwidth = 0.8,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
    
  )

p2 <- p2 + labs(x = "Ki-67% group", y = 'Ki-67% by AI') + 
  theme_pubr(base_size = 22) +
  scale_x_discrete(breaks = c("0", "1"), 
                   labels = c("Moderate", "Positive"))


p3 <- ggplot(KI67_1, aes(x=as.factor(ki67_group), y=pos_ref_perc,)) + 
  geom_boxplot(
    
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    
    # Notch?
    notch=FALSE,
    #notchwidth = 0.8,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
    
  )

p3 <- p3 + labs(x = "Ki-67% group", y = 'Ki-67% by HUS') + 
  theme_pubr(base_size = 22) +  
  scale_x_discrete(breaks = c("0", "1"), labels = c("Moderate", "Positive"))

ggarrange(p2,p3, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


ggplot(KI67_1, aes(x=as.factor(ki67_group), y=neg_perc,)) + 
  geom_boxplot(
    
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
  )

# group by algorithm value
algo_groups <- KI67_1 %>% 
  mutate(algo_group = ifelse(`pos_perc`>=14.5, 1, 0))

# get some descriptive statisics based on Ki67 group
describeBy(algo_groups, group = algo_groups$algo_group)


p + scale_x_discrete(breaks = c("0", "1"), 
                     labels = c("Moderate", "Positive"))
