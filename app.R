##Importing necessary Libraries for the Dashboard

library(rsconnect)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)


##Creating the UI part of the dashboard where the user will input the necessary answers to the questions

ui <- fluidPage(
    dashboardPage(
    dashboardHeader(title="Disease",
                    dropdownMenu(type = "messages",
                                 messageItem(
                                     from = "Personal Doctor",
                                     message = "Please schedule an Appointment if there is any issue"
                                 ),
                                 messageItem(
                                     from = "New User",
                                     message = "How do I use the Dashboard?",
                                     icon = icon("question"),
                                     time = "13:45"
                                 ),
                                 messageItem(
                                     from = "Support",
                                     message = "The new server is ready.",
                                     icon = icon("life-ring"),
                                     time = "2020-05-13"
                                 )
                    ),
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                     text = "7 new users today",
                                     icon("users")
                                 ),
                                 notificationItem(
                                     text = "5 Doctors are available ",
                                     icon("hospital"),
                                     status = "success"
                                 ),
                                 notificationItem(
                                     text = "Server load at 97%",
                                     icon = icon("exclamation-triangle"),
                                     status = "warning"
                                 )
                    ),
                    dropdownMenu(type = "tasks", badgeStatus = "success",
                                 taskItem(value = 90, color = "green",
                                          "Heart Disease"
                                 )
                    )),
    
    # Creating the Sidebar Content
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Heart Disease",tabName = "Heart Disease",icon=icon("stream"))
        )
    ),
    dashboardBody(
        
      
    ## Creating Value Boxes to display user inputs
      
        fluidRow(
            valueBoxOutput("age"),
            valueBoxOutput("gender"),
            valueBoxOutput("bmi"),
            valueBoxOutput("weight"),
            valueBoxOutput("height"),
            
        ),
        
        ### Creating boxes for user inputs
        
        box(
            title = "Please Fill in the Following to Get the Likelihood of Having Heart Disease",status="primary",background="navy",collapsible = TRUE, solidHeader = TRUE,height=700,
            numericInput(inputId='age', 
                         label='1.Please Enter your Age:', 
                         value=22,min = NA, 
                         max = NA, step = NA,
                         width = NULL),
            numericInput(inputId='weight', 
                         label='2.Please Enter your Weight (in Kilograms):', 
                         value=65,min = NA, 
                         max = NA, step = NA,
                         width = NULL),
            numericInput(inputId='height', 
                         label='3.Please Enter your Height (in Metres):', 
                         value=1.5,min = NA, 
                         max = NA, step = NA,
                         width = NULL),
            checkboxGroupInput(inputId='sex', 
                               label='4.Please check your gender:', 
                               c('Female','Male'), 
                               selected = 'Female', 
                               inline = FALSE,width = NULL),
            numericInput('trestbps',label='5.Please Enter your resting blood pressure in mm/Hg:',
                         min=50,
                         max=200,
                         value = 120),
            numericInput(inputId = 'oldpeak',label='6.Please enter your exercise induced CT:',
                         value=1.2,
                         min=0,
                         max=NA,
                         step=NA,
                         width=NULL),
            checkboxGroupInput(inputId = 'ca',label='7.On a Scale of 0-4,Have you had any blood circulation problems?',
                               c('Four','Three','Two','One','Zero'),selected=NULL,
                               inline=FALSE,
                               width=NULL)
        ),
       
        ### Creating a box to display the Probability/ Likelihood of an individual having heart disease
        
        box(
            title="Your Probability of Having Heart Disease is:",status="danger",background="navy", solidHeader = TRUE,
            textOutput("Pred")
        ),
        
      
            box(
                title = "Please Fill in the Following to Get the Likelihood of Having Heart Disease",status="primary",background="navy",collapsible = TRUE,solidHeader = TRUE,height=600,
                checkboxGroupInput(inputId = 'slope',label='8.How can you describe your DT depression?:',
                                   c('Upslopping','Flat','Downslopping'),selected=NULL,
                                   inline=FALSE,width=NULL),
                numericInput(inputId = 'thalach',label='9.what is the maximum heart rate you can achieve?',
                             value=100,
                             min=71,
                             max=NA,
                             step=NA,
                             width=NULL),
                checkboxGroupInput(inputId = 'exang',label='10.Have you experienced any chest pains after exercising?',
                                   c('No','Yes'),selected=NULL,
                                   inline = FALSE,
                                   width = NULL),
                
                checkboxGroupInput(inputId = 'thal',label = '11.How can you describe the nature of your blood?',
                                   c('Unknown','Normal','Fixed defect','Reversible defect'),
                                   selected=NULL,
                                   inline = FALSE,
                                   width=NULL)
            )
        
        )
    )
)
## Creating the Server part of the dashboard

server <- function(input, output,session) { 
    
  ##Creating the value box outputs and the colours
    output$age <- renderValueBox({
        valueBox(
            paste0(input$age), "Age", icon = icon("address-card"),
            color = "light-blue"
        )
    })
    
    output$gender <- renderValueBox({
        valueBox(
            paste0(input$sex),"Gender",icon = icon("user-circle"),
            color = "orange"
        )
    })
    
    output$bmi <- renderValueBox({
        valueBox(
            paste0(input$weight/input$height^2),"Body Mass Index (BMI)",icon = icon("heart"),
            color = "red"
        )
    })
    
    output$weight <- renderValueBox({
        valueBox(
            paste0(input$weight),"Weight",icon = icon("ambulance"),
            color = "teal"
        )
    })
    output$height <- renderValueBox({
        valueBox(
            paste0(input$height),"Height",icon = icon("thumbs-up"),
            color = "olive"
        )
    })
    
    
    ## Creating a dataframe of the user inputs to be used for prediction
    
    datashiny<-reactive({
        req(input$sex)
        req(input$ca)
        req(input$age)
        req(input$slope)
        req(input$exang)
        req(input$thalach)
        req(input$thal)
        
        data.frame(
            age=input$age,
            sex=input$sex,
            trestbps=as.numeric(input$trestbps),
            thalach=input$thalach,
            exang=input$exang,
            oldpeak=input$oldpeak,
            slope=input$slope,
            ca=input$ca,
            thal=input$thal
        )
    })
    
    
    
    ## Importing the classifiers used in the analysis
    
    model_lr=readRDS("data/model_lr.rds")
    model_rf=readRDS("data/model_rf.rds")
    model_gbm=readRDS("data/model_gbm.rds")
    
    
    ### Predicting first using the random forest classifier we had created and selecting the pobability of having Heart Disease
   
     rf= reactive({
       
       predict(object=model_rf,datashiny(),type = 'prob')[[2]]
        
    })
    
    ### Predicting first using the logistic regression classifier we had created and selecting the pobability of having Heart Disease
   
    lr=reactive(({
        
       predict(object=model_lr,datashiny(),type = 'prob')[[2]]
        
    }))
  
   ## Creating a dataframe of the predictions of random forest classifier and Logistic classifier
   ## Using the dataframe created, which are the predictors of the GBM model, and predicting using the the GBM Model to give the probability/likelihood of having heart disease.
   
    Pred<-reactive({
        
        predict(object=model_gbm,data.frame(OOF_pred_lr=lr(),OOF_pred_rf=rf()),type='prob')[[2]]
        
    })
    
    
    output$Pred <- renderPrint(Pred())
    
    
}

shinyApp(ui, server)

