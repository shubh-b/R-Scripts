packages = c("readxl", "text2vec", "data.table", "dplyr", "naivebayes")
package.check <-  suppressMessages(lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}))

## Browse and import the excel file in .xlsx format with available 
# column names 'Index', 'Title', 'Category','Description' for training set.
dat0 <- suppressWarnings(read_excel(file.choose()))

# Create a data.table object.
dat <- as.data.table(data.frame(Index = (dat0$Index), 
                                 Info = paste(dat0$Title, '||',dat0$Description), 
                                 Category = dat0$Category))

dat$Index <- as.character(dat$Index)

train <- dat

## Construct preprocessing and tokenization function
it_train <- itoken(as.character(train$Info), 
                   preprocessor = tolower, 
                   tokenizer = word_tokenizer, 
                   ids = as.character(train$Index), 
                   progressbar = FALSE)

vocab.train <- create_vocabulary(it_train)

## Construct a document-term matrix (DTM)
vectorizer.train <- vocab_vectorizer(vocab.train)
dtm_train <- create_dtm(it_train, vectorizer.train)

## word frame construction prior to category classification with merged 'Index' column
# and 'Category' column.
doc.frame <- list()
for(i in 1:nrow(dtm_train)){
  doc.frame[[i]] <- (dtm_train[i,]) 
}
word.frame0 <- data.frame(Index = rownames(dtm_train), do.call('rbind', doc.frame))
word.frame0$Index <- as.character(word.frame0$Index)
word.frame <- left_join(word.frame0, train[,-c('Info')], by = 'Index')


## Preparation of Naïve Bayes Classifier
laplace <- 1
mnb <- multinomial_naive_bayes(x = as.matrix(word.frame %>% select(-c('Index', 'Category'))),
                               y = as.vector(word.frame$Category), laplace = laplace)



## Predicting Categories for the test set.
# Browse and import test set with columns 'Index', 'Title', 'Description'
dat1 <- suppressWarnings(read_excel(file.choose()))

test <- as.data.table(data.frame(Index = (dat1$Index), 
                                Info = paste(dat1$Title, '||', dat1$Description)))

test$Index <- as.character(test$Index)


## Construct preprocessing and tokenization function for test set.
it_test <- itoken(as.character(test$Info), 
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer, 
                  ids = as.character(test$Index), 
                  progressbar = FALSE)

vocab.test <- create_vocabulary(it_test)

## Construct a document-term matrix (DTM)
vectorizer.test <- vocab_vectorizer(vocab.test)
dtm_test <- create_dtm(it_test, vectorizer.test)

## word frame construction prior to category classification.
doc.frame.test <- list()
for(i in 1:nrow(dtm_test)){
  doc.frame.test[[i]] <- (dtm_test[i,]) 
}
word.frame.test0 <- data.frame(Index = rownames(dtm_test), do.call('rbind', doc.frame.test))
word.frame.test0$Index <- as.character(word.frame.test0$Index)
word.frame.test <- left_join(word.frame.test0, test[,-c('Info')], by = 'Index')

## Category prediction
pred <- suppressWarnings(predict(mnb, newdata = as.matrix(word.frame.test %>% select(-c('Index'))), 
                                 type = "class"))
output <- data.frame(dat1, Predicted.Category = pred)
View(output)
