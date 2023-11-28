
#----drzewa decyzyjne----
library(rpart) #tu jest algorytm CART
library(rpart.plot)
library(mlbench)# pakiet z danymi w R
library(rattle)
library(dplyr)


#wine <- read.csv2("//vmfrze01/dsm/=Sebastian=/UR/Zadania projektowe/Wine quality/winequality.csv")
wine <- rattle::wine

# przekoduj wartości zmiennej Type na 'katania', 'bolonia' i 'toskania'





levels(wine$Type) <- c('katania','bolonia','toskania')

# zrób wykresy pudełkowe wszystkich zmiennych numerycznych względem zmiennej Type
# które zmienne byłyby dobrymi predyktorami zmiennej Type?


boxplot(wine$Alcohol~wine$Type)
boxplot(wine$Ash~wine$Type)
boxplot(wine$Malic~wine$Type)
boxplot(wine$Magnesium~wine$Type)
boxplot(wine$Flavanoids~wine$Type)
boxplot(wine$Nonflavanoids~wine$Type)
boxplot(wine$Proanthocyanins~wine$Type)
boxplot(wine$Color~wine$Type)
boxplot(wine$Hue~wine$Type)
boxplot(wine$Dilution~wine$Type)
boxplot(wine$Proline~wine$Type)
boxplot(wine$Phenols~wine$Type)
boxplot(wine$Alcalinity~wine$Type)





# narysuj wykres punktowy zmiennych Dilution i Proline, zrób legendę






plot(wine$Dilution[wine$Type=="toskania"],wine$Proline[wine$Type=="toskania"],
     xlim=c(min(wine$Dilution),max(wine$Dilution)),
     ylim=c(min(wine$Proline),max(wine$Proline)),
     xlab = "Dilution",
     ylab = "Proline",
     col="red",
     pch=15)
points(wine$Dilution[wine$Type=="bolonia"],wine$Proline[wine$Type=="bolonia"],col="blue",pch=15)
points(wine$Dilution[wine$Type=="katania"],wine$Proline[wine$Type=="katania"],col="green",pch=15)
legend(x="topleft",legend = c("toskania","bolonia","katania"),pch = 15,col=c("red","green","blue"))



# drzewo decyzyjne
wineType <- rpart(Type~.,	# formuła postaci y~x1+x2 
                  data=wine, # nazwa ramki danych skąd brane są zmienne
                  method="class", # dla problemu regresji wybieramy 'anova', dla klasyfikacji wybieramy  'class'
                  model=TRUE, # czy zachować kopię modelu?
                  #control=rpart.control(minsplit=2,minbucket =1,cp=0.001,xval=10), # parametry algorytmu definiowane przez obiekt rpart.control
                  na.action=na.rpart # co robić z NA?
) 


rpart.plot(wineType)


# ocena modelu 
rsq.rpart(wineType) #1 - R2 modelu oraz R2 w walidacji krzyżowej, 2 - odsetek błędnych predykcji
plotcp(wineType) # cp - minimalna wymagana zmiana R2/Accuracy, aby dokonać podziału drzewa
printcp(wineType) # Root node error - porównaj z 1-max(table(wine$Type))/nrow(wine)
summary(wineType)



# sprawdź dokładność predykcji modelu

predict(wineType,wine[,-1],type = "prob")
predict(wineType,wine[,-1],type = "class")

table(wine$Type,predict(wineType,wine[,-1],type = "class"))
caret::confusionMatrix(wine$Type,predict(wineType,wine[,-1],type = "class"))



# ZADANIE

Załaduj plik 'HCV data.csv'. Zmienna celu to 'Category'

1) utwórz zmienną 'Category2', która równa jest Category dla etykiet "Blood Donor" a w pozostałych przypadkach "Liver disease"
2) oglądnij dane i oceń wstępnie, które zmienne dobrze dyskryminują zmienną 'Category' oraz 'Category2'
3) zbuduj drzewa decyzyjne dla zmiennej 'Category' oraz 'Category2'
4) oceń jakość modelu
5) zbuduj model kNN dla zmiennej 'Category' oraz 'Category2'
6) oceń jakość modelu