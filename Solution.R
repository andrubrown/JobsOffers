library("RTextTools")
library("tm")

#----------- Input - Describe data
data_train = read.csv("C:/Users/VAIO/Desktop/stagedatascientistmultiposting/data_train.csv", encoding = "UTF-8")
data_train = data_train[,-3]

data_test = read.csv("C:/Users/VAIO/Desktop/stagedatascientistmultiposting/data_test.csv", encoding = "UTF-8")
colnames(data_test)[3] = "job_id"
data_test[,3] = rep(0,times=dim(data_test)[1])

data = rbind(data_train, data_test)

#----------- Create the Document-Term matrix - Handelling Data
# Treatment of some special characters
data$title = as.character(data$title)
data$title <- gsub("$", " ", data$title)
data$title <- gsub("-", " ", data$title)
data$title <- gsub("/", " ", data$title)
data$title <- gsub("d'", "", data$title)
data$title <- gsub("D'", "", data$title)

doc_matrix <- create_matrix(data$title, language="french", minWordLength = 2, removeNumbers=TRUE,
                            removePunctuation = TRUE, toLower = TRUE, removeStopwords = TRUE,
                            stemWords=TRUE, removeSparseTerms=.999)
# Coeff removeSparseTerms = .999 give the good result in terms of precision's algorithm on test data

# Remove more stopwords
remove_my_stopwords <- function(own_stw, dtm){
  ind <- sapply( own_stw, 
                 function(x, words){
                   if(any(x==words)) return(which(x==words)) else return(NA)
                 },
                 words=colnames(dtm) )
  return(dtm[ ,-c(na.omit(ind))])  
}
stw = c("altern","alternancecharg","apprentissag","candidatur",
        "cdd","cdi","mois","stag","stagiair","offre","contrat")
doc_matrix<-remove_my_stopwords(own_stw=stw, dtm=doc_matrix)

dim(doc_matrix)
colnames(doc_matrix)

#----------- Create Container : Train - Test set
container <- create_container(doc_matrix, data$job_id, trainSize=1:12000,
                              testSize=12001:12300, virgin=TRUE)

#----------- Training models 
# We use these 4 models because it require less CPU, also the result is relatively good
SVM <- train_model(container,"SVM")             # Support Vector Machine
GLMNET <- train_model(container,"GLMNET")       # Lasso and Elastic-Net Regularized Generalized Linear Models
MAXENT <- train_model(container,"MAXENT")       # Maximum entropy
SLDA <- train_model(container,"SLDA")           # scaled linear discriminant analysis


#----------- Classifying data using trained models
SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA)


#-----------  Ensemble Learning to enhance predictability - Analytics - Testing algorithm accuracy
# After analysing the result, we choose Ensemble Learning - which using 4 methods together
# which give the coeff Recall up to ~ 90%
analytics <- create_analytics(container,
                              cbind(SVM_CLASSIFY, GLMNET_CLASSIFY, MAXENT_CLASSIFY, SLDA_CLASSIFY) )
summary(analytics)

#----------- Exporting data
result = cbind (data[12001:12300,"id"],analytics@document_summary$CONSENSUS_CODE)
colnames(result) = c("id","job")
write.csv(result, "result.csv", row.names = FALSE)
