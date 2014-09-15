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
      uiOutput("hr"),
      uiOutput("text"),
      uiOutput("cboxpaired"),
      uiOutput("cboxvar"),
      br(),
      uiOutput("alternate"),
      br(),
      uiOutput("linkmsg")
      ),
    
    mainPanel(
      tabsetPanel(id = 'maintabs',
        tabPanel("Main", uiOutput('contents'),
                 tableOutput('tbl'),
                 uiOutput('contents2')
                 ), 
        tabPanel("Box Plot", plotOutput("boxplot")),
        tabPanel("Usage Guide", uiOutput("usage")), 
        tabPanel("Credits and other Links", uiOutput("credits")),
        tabPanel("Limitations", uiOutput("limits"))
      )
    )
  )
))
