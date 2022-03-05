### KAFIL ELKHADIR ####KHADIM

##### Bien montrer les différentes sections des modeles de l'application 4 modeles et une partie exploratoire en R et Rshiny
#DecisionTree : 2
#RandomForest : 0
#RegressionLogistique : 1
#DataAnalysis : 3
#Reseaux de neuronnes : 4


#Installer les librairies avant de les utiliser 
library(tidyverse)
library(randomForest)
library(caTools)
library(caret)
library(shiny)
library(shinythemes)
library(data.table)
# on fait appel à la classe réseaux de neuronnes 
source('Classe-reseaux-neuronnes.R') # séparation du modele de réseaux de neuronnes dans un autre fichier 


#Importation des jeux de donnees changer le chemin 
train <- read.csv("C:/Users/kafelk/Desktop/projet concepts- Phase 1/Phase-final/train.csv")



############################################# Data Featuring ou preprocessing #######################################################

# modifier le type des donnees suivant la description du jeux de donees 
# et remplacer le vide par des valeurs manquantes
train[ train == ""] <-NA
train$Gender<-as.factor(train$Gender)
train$Married<-as.factor(train$Married)
train$Dependents<-as.factor(train$Dependents)
train$Education<-as.factor(train$Education)
train$Self_Employed<-as.factor(train$Self_Employed)
train$Property_Area<-as.factor(train$Property_Area)
train$Loan_Status<-as.factor(train$Loan_Status)
train$Credit_History<-as.factor(train$Credit_History)



#types de variables
#str(train)

#nombre de lignes et colonnes
#nrow(train)
#ncol(train)

# Exploration de jeux de donnees ici meme ou regarder c'est deja fait dans l'application

#summary(train)

# Pas vraiment besoin du loan_id aucune influnce on l'enleve du train et test
train<-train[-1]

# remplacer les valeurs manquantes par la valeur la plus frequente ou mode via la fonction tres 
# connu getmode
getmode <- function(v) {
  v<-na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## via la libraire tidyverse on fait le remplacement au lieu de plusieurs boucles imbriques 

train <- train %>% mutate(across(everything(), ~replace_na(.x, getmode(.x))))





########################################################### Random Forest 0 ######################################################

# division du jeux de donnÃÂ©es en train et test pour essaye de voir la performance du modele
smp_size <- floor(0.8 * nrow(train))

set.seed(7)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)
train1<-train[train_ind,]
test1<-train[-train_ind,]

# modele via la librairie de Randomforest qui choisit les bons parametres automatiquements 
model <- randomForest(
  Loan_Status ~ .,
  data=train1
)


#Performance du modele 

head(test1[-12])

pred = predict(model, newdata=test1[-12])

#### Tres performant la matrice de confusion est affiche dans l'application
cm = confusionMatrix(table(test1[,12], pred))


k<-data.frame(cm$overall)

k1<-cbind(row.names(k),cm$overall)











#################################### Regression logistique 1 #################################################


# une fonction fit nous aidant a ameliorer les parametres de la courbe 
fitControl<-trainControl(method="cv",number=10,savePredictions=TRUE)

# nous creons un modele de regression logitique fitter avec les bon parametres via la librairie Caret 
lr_model<-train(factor(Loan_Status)~ .,data=train1,method="glm",family=binomial(),trControl=fitControl)

#visualisons le modele
summary(lr_model)

# verfions nos prÃ©dictions sur les donÃ©es de test
prediction_lr<-predict(lr_model,test1)

test1$Prediction<- prediction_lr

# Regardons maintenant si notre modele est performant et c'est le cas 

tab<-table(test1$Prediction,test1$Loan_Status)

cm_regression_logistique<-confusionMatrix(tab)


perf_reg<-data.frame(cm_regression_logistique$overall)

perf_reg1<-cbind(row.names(perf_reg),cm_regression_logistique$overall)




















##########################################    UI : partie interface de l'application#####################################################

ui <- 
  navbarPage("Projet_Accord_Pret", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
             
             tabPanel("Exploration du jeu de donnees",
                      HTML("<h3>Resume et exploration statistique Apres preprocessing </h3>"),
               mainPanel(
                 dataTableOutput('DataSet'),
                 verbatimTextOutput("Descriptive")
                 
               )
             ),
             
             
             
             # Interface pour la régression logistique
             
             
             tabPanel("RegressionLogistique",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("'Loan Approved ?1", br(),
                                   sidebarPanel(
                                     HTML("<h3>Input parameters</h3>"),
                                     
                                     selectInput("Gender1", label = "GENDER", 
                                                 choices = list("FEMALE" = "Female", "MALE" = "Male"), 
                                                 selected = "Male"),
                                     selectInput("Married1", label = "MARRIED", 
                                                 choices = list("NO" = "No", "YES" = "Yes"), 
                                                 selected = "No"),
                                     selectInput("Dependents1", label = "DEPE", 
                                                 choices = list("NONE" = "0", "ONE" = "1","TWO"="2","THREE_MORE"="3+"), 
                                                 selected = "NONE"),
                                     selectInput("Education1", label = "EDUCATION", 
                                                 choices = list("DIPLOMA" = "Graduate", "NO_DIPLOMA" = "Not Graduate"), 
                                                 selected = "DIPLOMA"),
                                     selectInput("Self_Employed1", label = "SELF-EMPLOYED", 
                                                 choices = list("NO" = "No", "YES" = "Yes"), 
                                                 selected = "No"),
                                     sliderInput("ApplicantIncome1", label = "Application Income",value=min(train1$ApplicantIncome),
                                                 min = min(train1$ApplicantIncome),
                                                 max = max(train1$ApplicantIncome) ),
                                     sliderInput("CoapplicantIncome1", label = "Coapplicant Income", value=min(train1$CoapplicantIncome),
                                                 min = min(train1$CoapplicantIncome),
                                                 max = max(train1$CoapplicantIncome) ),
                                     sliderInput("LoanAmount1", label = "Loan Amount", value=min(train1$LoanAmount),
                                                 min = min(train1$LoanAmount),
                                                 max = max(train1$LoanAmount) ),
                                     sliderInput("Loan_Amount_Term1", label = "Loan Amount Term",value = 360,
                                                 min = min(train1$Loan_Amount_Term),
                                                 max = max(train1$Loan_Amount_Term) ),
                                     selectInput("Credit_History1", label = "Credit History", 
                                                 choices = list("Guidelines-NOT" = "0", "Guidelines-OK"="1")),
                                     selectInput("Property_Area1", label = "AREA", 
                                                 choices = list("URBAN" = "Urban", "SEMI"="Semiurban","RURAL"="Rural")),
                                     
                                     actionButton("submitbutton1", "Submit", class = "btn btn-primary")
                                   ),
                                   
                                   mainPanel(
                                     tags$label(h3('Sorties Regression')), # Status/Output Text Box
                                     verbatimTextOutput('contents1'),
                                     tableOutput('tabledata1'), # Prediction results table
                                     verbatimTextOutput('contentsperf1'),
                                     dataTableOutput('performancemodel1')
                                     
                                   ))))),
             
             
             
             
             
             
             # Interface pour le RandomForest
             
             
             tabPanel("Random-Forest",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("'Loan Approved ?", br(),
                                   sidebarPanel(
                                     HTML("<h3>Input parameters</h3>"),
                                     
                                     selectInput("Gender", label = "GENDER", 
                                                 choices = list("FEMALE" = "Female", "MALE" = "Male"), 
                                                 selected = "Male"),
                                     selectInput("Married", label = "MARRIED", 
                                                 choices = list("NO" = "No", "YES" = "Yes"), 
                                                 selected = "No"),
                                     selectInput("Dependents", label = "DEPE", 
                                                 choices = list("NONE" = "0", "ONE" = "1","TWO"="2","THREE_MORE"="3+"), 
                                                 selected = "NONE"),
                                     selectInput("Education", label = "EDUCATION", 
                                                 choices = list("DIPLOMA" = "Graduate", "NO_DIPLOMA" = "Not Graduate"), 
                                                 selected = "DIPLOMA"),
                                     selectInput("Self_Employed", label = "SELF-EMPLOYED", 
                                                 choices = list("NO" = "No", "YES" = "Yes"), 
                                                 selected = "No"),
                                     sliderInput("ApplicantIncome", label = "Application Income",value=min(train1$ApplicantIncome),
                                                 min = min(train1$ApplicantIncome),
                                                 max = max(train1$ApplicantIncome) ),
                                     sliderInput("CoapplicantIncome", label = "Coapplicant Income", value=min(train1$CoapplicantIncome),
                                                 min = min(train1$CoapplicantIncome),
                                                 max = max(train1$CoapplicantIncome) ),
                                     sliderInput("LoanAmount", label = "Loan Amount", value=min(train1$LoanAmount),
                                                 min = min(train1$LoanAmount),
                                                 max = max(train1$LoanAmount) ),
                                     sliderInput("Loan_Amount_Term", label = "Loan Amount Term",value = 360,
                                                 min = min(train1$Loan_Amount_Term),
                                                 max = max(train1$Loan_Amount_Term) ),
                                     selectInput("Credit_History", label = "Credit History", 
                                                 choices = list("Guidelines-NOT" = "0", "Guidelines-OK"="1")),
                                     selectInput("Property_Area", label = "AREA", 
                                                 choices = list("URBAN" = "Urban", "SEMI"="Semiurban","RURAL"="Rural")),
                                     
                                     actionButton(inputId = "submitbutton", "Submit", class = "btn btn-primary")
                                   ),
                                   
                                   mainPanel(
                                     tags$label(h3('Sorties Random Forest')), # Status/Output Text Box
                                     verbatimTextOutput('contents'),
                                     tableOutput('tabledata'), # Prediction results table
                                     verbatimTextOutput('contentsperf'),
                                     dataTableOutput('performancemodel')
                                     
                                     
                                   ))))),
             
             
             # Interface du réseaux de neuronnes ne comprend que la performance et l'image du neuronne
             tabPanel("Performance reseaux de neuronnes",
                      HTML("<h3>Apres transformation de la DATA </h3>"),
                      mainPanel(
                        plotOutput("neuronne"),
                        dataTableOutput("performancemodel2")
                        
                      )
             ))




#################################################### SERVER ##########################################                        

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # prepration du dataframe d entree utlisateur 
    df <- data.frame(
      Name = c("Gender",
               "Married",
               "Dependents",
               "Education",
               "Self_Employed",
               "ApplicantIncome",
               "CoapplicantIncome",
               "LoanAmount",
               "Loan_Amount_Term",
               "Credit_History",
               "Property_Area"),
      Value = as.character(c(input$Gender,
                             input$Married,
                             input$Dependents,
                             input$Education,
                             input$Self_Employed,
                             input$ApplicantIncome,
                             input$CoapplicantIncome,
                             input$LoanAmount,
                             input$Loan_Amount_Term,
                             input$Credit_History,
                             input$Property_Area)),
      stringsAsFactors = FALSE)
    # une colonne qu'on rajoute et c'est elle qui va renvoyer nos predictions
    Loan_Status <- "Loan_Status"
    df <- rbind(df, Loan_Status)
    # le modele est entrainé sur des dataframe et doit recvoir pareil
    input <- transpose(df)
    # sauvegrader les input des utilisateurs 
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    # retransformation des variables pour l'application final shiny
    test$Gender<-factor(test$Gender, levels = c("Female", "Male"))
    test$Married<-factor(test$Married, levels = c("No", "Yes"))
    test$Dependents<-factor(test$Dependents, levels=c("0","1","2","3+"))
    test$Education<-factor(test$Education,levels=c("Graduate","Not Graduate"))
    test$Self_Employed<-factor(test$Self_Employed,levels=c("No","Yes"))
    test$Property_Area<-factor(test$Property_Area,levels=c("Rural","Semiurban","Urban" ))
    test$Credit_History<-factor(test$Credit_History,levels=c("0","1"))
    test$CoapplicantIncome<-as.numeric(test$CoapplicantIncome)
    
    Output <- data.frame(Prediction=predict(model,test[-12]), round(predict(model,test,type="prob"), 3))
    #Output<-data.frame(test[-12])
    print(Output)
    
  })
  
  
  
  
  
  # Input Data
  datasetInput1 <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("Gender",
               "Married",
               "Dependents",
               "Education",
               "Self_Employed",
               "ApplicantIncome",
               "CoapplicantIncome",
               "LoanAmount",
               "Loan_Amount_Term",
               "Credit_History",
               "Property_Area"),
      Value = as.character(c(input$Gender1,
                             input$Married1,
                             input$Dependents1,
                             input$Education1,
                             input$Self_Employed1,
                             input$ApplicantIncome1,
                             input$CoapplicantIncome1,
                             input$LoanAmount1,
                             input$Loan_Amount_Term1,
                             input$Credit_History1,
                             input$Property_Area1)),
      stringsAsFactors = FALSE)
    
    Loan_Status <- "Loan_Status"
    df <- rbind(df, Loan_Status)
    input <- transpose(df)
    write.table(input,"input1.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input1", ".csv", sep=""), header = TRUE)
    
    test$Gender<-factor(test$Gender, levels = c("Female", "Male"))
    test$Married<-factor(test$Married, levels = c("No", "Yes"))
    test$Dependents<-factor(test$Dependents, levels=c("0","1","2","3+"))
    test$Education<-factor(test$Education,levels=c("Graduate","Not Graduate"))
    test$Self_Employed<-factor(test$Self_Employed,levels=c("No","Yes"))
    test$Property_Area<-factor(test$Property_Area,levels=c("Rural","Semiurban","Urban" ))
    #test$Loan_Status<-factor(test$Loan_Status,levels=c("N","Y"))
    test$Credit_History<-factor(test$Credit_History,levels=c("0","1"))
    test$CoapplicantIncome<-as.numeric(test$CoapplicantIncome)
    Output <- data.frame(Prediction=predict(lr_model,test[-12]), round(predict(lr_model,test,type="prob"), 3))
    #Output<-data.frame(test[-12])
    print(Output)
    
  })
  
  
  
  ########## nos outputs qui vont faire appel aux données R et server et les afficher aux UI 
  ##### Voir bien l'architecture dans le rapport
  
  output$neuronne <- renderPlot({
    plot(nn)
  })
  
  
  
  
  output$performancemodel2<-renderDataTable({
    if (input$submitbutton>0) { 
      isolate(data.frame(r2)) 
    } 
  })
  
  # Status/Output Text Box
  output$contentsperf <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Performance du modele a titre indicatif") 
    } else {
      return("Performance du modele en attente du serveur")
    }
  })
  
  #
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Prediction effectuee.") 
    } else {
      return("Serveur pret pour la prediction du modele")
    }
  })
  
  #
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  
  
  
  output$performancemodel1<-renderDataTable({
    if (input$submitbutton>0) { 
      isolate(data.frame(perf_reg1)) 
    } 
  })
  
  # 
  output$contentsperf1 <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Performance du modele a titre indicatif") 
    } else {
      return("Performance du modele en attente du serveur")
    }
  })
  
  
  
  
  
  # 
  output$contents1 <- renderPrint({
    if (input$submitbutton1>0) { 
      isolate("Prediction effectuee.") 
    } else {
      return("Serveur pret pour la prediction du modele")
    }
  })
  
  # 
  output$tabledata1 <- renderTable({
    if (input$submitbutton1>0) { 
      isolate(datasetInput1()) 
    } 
  })

  output$DataSet<-renderDataTable({
                train1
  })

  output$Descriptive <- renderPrint({
   summary(train1)

  })
  
  output$performancemodel<-renderDataTable({
    if (input$submitbutton>0) { 
      isolate(data.frame(k1)) 
    } 
  })
  
  
  
}

## et efin nous lancons l'application 


shinyApp(ui = ui, server = server)