# Installation des packages
install.packages("ggplot2", dep = TRUE)
install.packages("plotly", dep = TRUE)
install.packages("caret", dep = TRUE)

# Load des library 
library("ggplot2")
library("plotly")
library("caret")

# Lecture du fichier de données
bank_data = read.csv("bank.csv", sep = ";")


# Recherche d'influence entre les variables explicatives et la 
# variable y (souscription à un service bancaire)
graph1 <- ggplot(bank_data, aes(x=y, y=duration, fill=y)) + geom_boxplot()
graph1 <- graph1 + ggtitle("L'influence de le variable duration (durée de l'appel) 
sur la variable souscription y") 
# graph1 <- graph1 + theme(plot.title = element_text(hjust = 0.5)) 
graph1
# Nous remarquons que plus la variable duration (durée de l'appel)  est grande 
# plus il y a une probabilité que le client souscrit à un service bancaire

graph2 <- ggplot(bank_data, aes(x=y, y=age, fill=y)) + geom_boxplot()
graph2 <- graph2 + ggtitle("L'influence de le variable age sur la variable souscription y")
graph2 <- ggplotly(graph2)
graph2
# Il n'y pas trop une grande différence entre la distribution d'age des souscrits (yes) et celle des non souscrits (no). 
# Apparemment, ce n'est pas l'age qui définit si le client va souscrire à un service bancaire

graph3 <- ggplot(bank_data, aes(y, fill=contact)) + geom_bar() 
graph3 <- graph3 + ggtitle("L'influence de la variable contact sur la variable souscription y")
graph3
# Peut-etre la variable contact a un effet sur le fait que le client 
# va souscrire ou non à un service bancaire. On voit qu'il y a plus de chance
# que le client ne souscrit pas à un service bancaire s'il répond avec un
# téléphone mobile (cellular). De meme avec un téléphone fixe (telephone)


graph4 <- ggplot(bank_data, aes(y, fill=marital)) + geom_bar()
graph4 <- graph4 + ggtitle("L'influence de la varible marital sur la variable souscription y")
graph4
# Peut-etre la variable marital a un effet sur le fait que le client 
# va souscrire ou non à un service bancaire. On voit qu'il y a plus de chance
# que le client ne souscrit pas à un service bancaire s'il est divorcé. 

graph5 <- ggplot(bank_data, aes(y, fill=housing)) + geom_bar()
graph5 <- graph5 + ggtitle("L'influence de la variable housing sur la variable souscription y")
graph5 <- graph5 + xlab("y (souscription)")
graph5
# Peut-etre la variable housing a un effet sur le fait que le client 
# va souscrire ou non à un service bancaire. On voit qu'il y a plus de chance
# que le client ne souscrit pas à un service bancaire s'il n'a pas de logement(housing).

graph6 <- ggplot(bank_data, aes(y, fill=loan)) + geom_bar()
graph6 <- graph6 + ggtitle("L'influence de la variable loan sur la variable souscription y")
graph6 <- graph6 + xlab("y (souscription)")
graph6
# Peut-etre la variable loan a un effet sur le fait que le client 
# va souscrire ou non à un service bancaire. On voit qu'il y a plus de chance
# que le client ne souscrit pas à un service bancaire s'il a fait un pret bancaire.


# Transformation des variables catégorielles en variable numérique 
dummy_variables=dummyVars(~., data=bank_data)
dummy_variables_data=predict(dummy_variables, newdata=bank_data)
dummy_variables_data=as.data.frame(dummy_variables_data)

dummy_variables_data$"Souscription"=ifelse(dummy_variables_data$"yno" == 1, "No", "Yes")
dummy_variables_data$"yno"=NULL
dummy_variables_data$"yyes"=NULL

# Création des jeux de données d'entrainement et de test
set.seed(3033)
training_size=floor(0.7*nrow(dummy_variables_data))
indices=sample(seq_len(nrow(dummy_variables_data)), size=training_size)
data_bank.train=dummy_variables_data[indices,]
data_bank.test=dummy_variables_data[-indices,]

dim(data_bank.train)
dim(data_bank.test)

# Normalisation des données
data_preprocess_value=preProcess(data_bank.train, method=c("center","scale"))
data_bank.train.scaled=predict(data_preprocess_value,data_bank.train)
data_bank.test.scaled=predict(data_preprocess_value,data_bank.test)

# Caret - downsample et upsample
table(data_bank.train.scaled[,"Souscription"])

set.seed(3033)
'%ni%' = Negate("%in%")

# downsample
data_bank.train.scaled.downsample=downSample(x=data_bank.train.scaled[,colnames(data_bank.train.scaled) %ni% "Souscription"], y=as.factor(data_bank.train.scaled$"Souscription"))
names(data_bank.train.scaled.downsample)[names(data_bank.train.scaled.downsample) == "Class"]="Souscription"
table(data_bank.train.scaled.downsample[,"Souscription"])

# upsample
data_bank.train.scaled.upsample=upSample(x=data_bank.train.scaled[,colnames(data_bank.train.scaled) %ni% "Souscription"], y=as.factor(data_bank.train.scaled$"Souscription"))
names(data_bank.train.scaled.upsample)[names(data_bank.train.scaled.upsample) == "Class"]="Souscription"
table(data_bank.train.scaled.upsample[,"Souscription"])

# modélisation avec naive bayes
set.seed(3033)
trainControl_data=trainControl(method="repeatedcv", number=10, repeats=3)
naive_bayes_desequilibree=train(Souscription ~., data=data_bank.train.scaled, method='naive_bayes', preProcess=NULL, 
                                trControl = trainControl_data)

print(naive_bayes_desequilibree)

# prédiction avec notre modèle sur le jeu de données tests
prediction_naive_bayes_desequilibree=predict(naive_bayes_desequilibree, newdata=data_bank.test.scaled[,-ncol(data_bank.test.scaled)])

# création de la matrice de confusion
confusionMatrix(prediction_naive_bayes_desequilibree, as.factor(data_bank.test.scaled[,ncol(data_bank.test.scaled)]))


# modélisation avec naive bayes sur les données downsamplé
set.seed(3033)
trainControl_data=trainControl(method="repeatedcv", number=10, repeats=3)
naive_bayes_downsample=train(Souscription ~., data=data_bank.train.scaled.downsample, method="naive_bayes", preProcess=NULL,
                             trControl = trainControl_data)

print(naive_bayes_downsample)

# prédiction avec notre modèle sur le jeu de données tests
prediction_naive_bayes_downsample=predict(naive_bayes_downsample, newdata=data_bank.test.scaled[,-ncol(data_bank.test.scaled)])

# création de la matrice de confusion
confusionMatrix(prediction_naive_bayes_downsample, as.factor(data_bank.test.scaled[,ncol(data_bank.test.scaled)]))

# modélisation avec SVM
set.seed(3033)
trainControl_data=trainControl(method="repeatedcv", number=10, repeats=3)
SVM_desequilibree=train(Souscription ~., data=data_bank.train.scaled, method="svmLinear", preProcess=NULL)

print(SVM_desequilibree)

# prédiction avec notre modèle sur le jeu de données tests
prediction_SVM_desequilibree=predict(SVM_desequilibree, newdata=data_bank.test.scaled[,-ncol(data_bank.test.scaled)])

# création de la matrice de confusion
confusionMatrix(prediction_SVM_desequilibree, as.factor(data_bank.test.scaled[,ncol(data_bank.test.scaled)]))

# Les variables prédictives les plus importantes 
varImp(naive_bayes_downsample, scale=F)
