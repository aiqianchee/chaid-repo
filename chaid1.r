# install.packages("CHAID", repos="http://R-Forge.R-project.org")
setwd("C://Users/AiQian/Desktop/chaid-local-repo")
setwd("C://Users/AiQian/Desktop/CIDDS-local/traffic/ExternalServer")

if ("CHAID" %in% rownames(installed.packages())  == FALSE) {
  install.packages("CHAID")
}

library('CHAID')
library('partykit')
library('grid')
library('libcoin')
library('csvread')
library('magrittr')
library('dplyr')

cidds <- read.csv(file = 'cidds.csv', header = TRUE, sep = ",")
head(cidds)
str(cidds)

# building the model
cidds %>%
  select_if(is.factor) %>%
  ncol # 10 factors 
cidds %>%
  select_if(is.numeric) %>%
  ncol # 6 integers
cidds %>%
  select_if(function(col)
    length(unique(col)) <= 5 & is.integer(col)) %>%
  head 

cidds %>%
  select_if(function(col)
    length(unique(col)) <= 8 & is.integer(col)) %>%
  head

cidds %>%
  mutate(
    Packets = integer(Packets),
    Duration = integer(Duration),
    Byte = integer(Bytes)
  ) %>% 
  str(cidds)

cidds <- cidds %>%
  mutate_if(function(col) length(unique(col)) <= 2 & is.integer(col), as.factor)
summary(cidds)

newcidds <- cidds %>% 
  select_if(is.factor)

ctrl <- chaid_control(minsplit = 2, minbucket = 2, alpha2 = .05, alpha4 = .05)
chaidmar <- chaid(class ~ ., data = newcidds, control = ctrl)
# memory.size(max = FALSE)
# memory.limit(size = 2500)

print(chaidmar)
plot(chaidmar)

plot(chaidmar, type = 'simple')

# evaluation of the model
library(gmodels)
trainToBishan <- predict(chaidmar, helpme)
CrossTable(helpme$MaritalStatus, trainToBishan,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))