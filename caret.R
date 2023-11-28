# co można wykonać w pakiecie caret
  # preprocessing
  # tuning hiperparametrów
  # wybór metryki i funkcji kryterium
  # walidacja


library(caret)
names(getModelInfo()) #zakres dostępnych funkcji
modelLookup('knn') #przeznaczenie i hiperparametry modelu
getModelInfo(model = 'knn')
modelLookup('rf') # w RandomForest można ustawiać mtry jak i ntree, ale nie wszystkie parametry można tuningować w caret

#################################

train(x = , # predyktory
      y = , # zmienna objaśniana
      # układ x,y może być zamieniony na form i data  (wzór i dane)
      method = "rf",
      preProcess = NULL, # wektor tekstowy opisujący metodę standaryzacji kolejnych predyktorów
      weights = NULL, # wagi instancji (metoda musi umieć je uwzględniać)
      metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
      maximize = ifelse(metric %in% c("RMSE", "logLoss", "MAE", "logLoss"), FALSE, TRUE),
      trControl = trainControl(),
      tuneGrid = NULL, # ramka danych z wartościami hiperparametrów do sprawdzenia, nazwy muszą być takie jak w caret
      tuneLength = ifelse(trControl$method == "none", 1, 3) # wystarczy podać liczbę wypróbowanych wartości hiperparametru
      )

# metrics
  # RMSE" oraz "Rsquared" dla regresji,  "Accuracy" i "Kappa" dla klasyfikacji
  # dostępna jest opcja własnej statystyki podsumuwujacej poprzez summaryfunction

# tuning
model <- train(x=iris[,-5],y=iris$Species,method='knn',tuneLength=5) #nie mam pomysłu na tuneGrid
model$results

model <- train(x=iris[,-5],y=iris$Species,method='knn',tuneGrid=data.frame(k=1:15)) #mam pomysł na tuneGrid
model$results


# preprocessing
# dostępne opcje to 
  # "BoxCox", "YeoJohnson", "expoTrans", 
  # "center" - odejmuje średnią, 
  # "scale" - dzieli przez odchylenie standardowe, 
  # "range" wraz z argumentem rangeBounds - unitaryzuje do przedziału zadanego przez rangeBounds
  # "knnImpute", "bagImpute", "medianImpute" - imputacja 
  # "pca", "ica", "spatialSign", 
  # "corr" wraz z parametrem cutoff - usuwa zmienne zbyt mocno skorelowane z pozostałymi wg cutoff
  # "zv" - usuwa stałe, 
  # "nzv" - usuwa quasi-stałe wg parametru uniqueCut albo freqCut, szczegóły w opisie funkcji nearZeroVar
  # "conditionalX" - sprawdz rozkłada predyktorów względem unikalnych wartości Y (factor), usuwa X które mają jedną unikalną wartość w ramach jakiejś klasy Y

preProcess(iris,method = c('center','scale')) # zwraca listę parametrów przetworzenia danych
predict(preProcess(iris,method = c('center','scale')),iris) %>% head() # zwraca ramkę danych

model <- train(x=iris[,-5],y=iris$Species,method='knn',tuneGrid=data.frame(k=1:5),
               preProcess = c('center','scale'))

# dorzućmy kilka słabych predyktorów
iris2=iris
iris2$const=12
iris2$quasiconst=12*(1+rnorm(150,0,0.001))
iris2$correlated=iris2$Petal.Length+3
iris2$conditional=c(rep(1,50),rnorm(100,30,3))

head(iris2)

predict(preProcess(iris2,method = c('zv')),newdata=iris2) %>% head() # zwraca ramkę danych
predict(preProcess(iris2,method = c('nzv','corr')),iris2) %>% head() # zwraca ramkę danych
iris2[1,1:2]=NA
predict(preProcess(iris2,method = c('nzv','conditionalX'),outcomes=Species),iris2) %>% head() 
predict(preProcess(iris2,method = c('nzv','medianImpute'),outcomes=Species),iris2) %>% head() 


predict(preProcess(iris2,method = c('center','scale')),iris2) %>% head() 
predict(preProcess(iris2,method = c('nzv','center','scale')),iris2) %>% head() 

model <- train(Species~., data=iris, method="lvq", preProcess=c('nzv','center','scale'), tuneLength=5)

# Kolejność operacji: 
  # zero-variance filter, near-zero variance filter, correlation filter, 
  # Box-Cox/Yeo-Johnson/exponential transformation, centering, scaling, range, 
  # imputation, PCA, ICA then spatial sign. 



# walidacja
  # metodę walidacji i jej parametru możemy zapisać poprzez dodatkową funkcję trainControl
  # dostępne metody:
    # boot", "boot632", "optimism_boot", "boot_all", 
    # "cv", "repeatedcv", 
    # "LOOCV", "LGOCV" (for repeated training/test splits), 
    # "none" (only fits one model to the entire training set), 
    # "oob" (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models), timeslice, 
    # "adaptive_cv", "adaptive_boot" or "adaptive_LGOCV"
  # number - liczba partycji zbioru albo liczba iteracji resamplingu
  # repeats - liczba k-krotnych walidacji
  # p - odsetek zbioru jaki ma stanowić zbiór treningowy


control <- trainControl(method="repeatedcv", number=10, repeats=3)
control <- trainControl(method="boot", number=100)
control <- trainControl(method="LOOCV")

# uczymy model
model <- train(Species~., data=iris, method="lvq", trControl=control, tuneLength=5)

# podsumowanie
print(model)
plot(model)


