library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Demonstrating t tests"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("choose_dataset"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      uiOutput("filechoose"),
      uiOutput("cboxheader"),
      br(),
      uiOutput("cboxpaired"),
      uiOutput("cboxvar"),
      br(),
      uiOutput("alternate"),
      br(),
      uiOutput("linkmsg")
      ),
    
#     mainPanel(uiOutput('contents'),
#               tableOutput('tbl'),
#               uiOutput('contents2'))
    
    mainPanel(
      tabsetPanel(id = 'maintabs',
        tabPanel("Main", uiOutput('contents'),
                 tableOutput('tbl'),
                 uiOutput('contents2')
#                  uiOutput('usagemain')
                 ), 
        tabPanel("Box Plot", plotOutput("boxplot")),
        tabPanel("Usage Guide", uiOutput("usage")), 
        tabPanel("Credits and other Links", uiOutput("credits")),
        tabPanel("Limitations", uiOutput("limits"))
      )
    )
  )
  
))

#       selectInput("option", label = "Select an Option:", 
#                   choices = c("Paired t-test Example", "Two Group t-test Example", "R Dataset: mtcars", "Upload Data"))
