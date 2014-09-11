library(shiny)

# Paired t-test example data
reg = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
prem = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)

# Two group t-test example data
control = c(91, 87, 99, 77, 88, 91)
treat = c(101, 110, 103, 93, 99, 104)

# mtcars dataset
data(mtcars)

shinyServer(function(input,output) {
  
  # Output for App Usage Guide tab
  output$usage<-renderUI(
    withTags({
      div(class = "usage",
          h3("Notes on using this Shiny App"),
          br(),
          p("This Shiny App demonstrates the use of t-tests to compare means and also enables users to
            upload their data and run t-test on the data."),
          p("Demonstration of the application of t-test is done using the following three examples:"),
          ol(li("Paired t-test"), li("Two Sample t-test"), li("Two Sample t-test using R's", b("mtcars"), "dataset")),
          p("The app provides a drop down list as shown in the image below"),
          img(src = "pic5.png", width = 150, height = 300),
          p(b("Note:"),"The fourth option is the drop down list enables the user to upload their data and run
            t-test on the uploaded data. The", b("Upload File"), "file chooser becomes available once the user
            selects to upload a file, along with the provision to specify paired or two sample test, whether the
            input file has a header or not and the type of alternative hypothesis: not equal to, greater than or less than"),
          
          p(b("Note"), "that the app currently handles .csv file only. Only the first two rows of data are accessed and
            the optional header are accessed. The application currently ignores the header if present")
          
      )
    })
  )
  
  # Output for Credits tab
  output$credits<-renderUI({
    withTags({
      div(
        h4("Credits and other Links"),
        p("The two examples were taken from the following URL:", a(href="http://www.stat.columbia.edu/~martin/W2024/R2.pdf", 
                                                                   "http://www.stat.columbia.edu/~martin/W2024/R2.pdf."),
          "This was among the top Google search results!!"),

        p("A sample file for use with the 'Upload Data' option and tested with this application can be found ",
                a("here.", href = "traintimes.csv"), " The data for this file was obtained from this ",
                a("webpage.", href = "http://www.r-bloggers.com/paired-students-t-test/"))
        
        )
    })  
  })
  
  # Output for limitations tab
  output$limits<-renderUI({
    withTags({
      div(
        h4("Limitations"),
        ol(
          li("By default, the application runs the", b("Paired t-test"), " example, when launched. This makes it slow at startup"),
          li("The links on the", b("Credits and other Links"), " tab take you to external sites. When returning back from those
             sites, the user is taken back to the", b("Output"), " tab, rather than the", b("Credits and other Links"), " tab, which
             can be annoying and needs to be fixed"))
        )
    })
  })
  
  # Renders the left hand navigation panel
  output$choose_dataset <- renderUI({
    selectInput("option", "Data set", c("Paired t-test Example", "Two Group t-test Example", "R Dataset: mtcars", "Upload Data"),
                selected = "Paired t-test Example")
  })

  # Dynamic input element (1) for "Upload Data" option
  # File Choose box
  output$filechoose<-renderUI({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             fileInput('inputFile', "Upload File")
           }
    )
  })

  # Dynamic input element (2) for "Upload Data" option
  # Check box to specify if input file has header
  # Default is No Header
  output$cboxheader<-renderUI({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             checkboxInput('header', "Header Present")
           }
    )
  })
  
  # Dynamic input element (3) for "Upload Data" option
  # Check box to specify if Paired t-test is to be 
  # performed. Default is Two Group test
  output$cboxpaired<-renderUI({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             checkboxInput('paired', "Paired")
           }
    )
  })
  
  # Dynamic input element (4) for "Upload Data" option
  # Check box to specify if variance is equal in case 
  # of two group test. Default is not equal
  output$cboxvar<-renderUI({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             checkboxInput('variance', "Equal Variance")
           }
    )
  })
  
  # Dynamic input element (5) for "Upload Data" option
  # Radio buttons to specify type of test: two sided 
  # or less than or greater than.
  # Default is two sided
  output$alternate<-renderUI ({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             radioButtons("alternative", "Alternative",
                 c("Not Equal" = "two.sided",
                   "Less Than" = "less",
                   "Greater Than" = "greater"), selected = "two.sided")
           }
    )
  })
  
  # Dynamic input element (6) for "Upload Data" option
  # Text message and link to sample data file for upload
  output$linkmsg<-renderUI ({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             withTags({
               div(
                 p("A sample file tested with this application can be obtained from",a("here.", href = "traintimes.csv"))
                 )
             })
           }
    )
  })
  
  # The output is rendered in 3 parts. This is part 1
  # It just display an introductory message. Part 2 below
  # renders the data in a table format. Part 3 after that
  # displays the P-value and a conclusion.
  # TODO: Combine three parts into one.
  output$contents<-renderUI({
        
    if(is.null(input$option))
      return()
    
            switch(input$option,
                   "Paired t-test Example" = {
                      withTags({
                        div(class="intro",
                          h5("Paired t-test example"),
                          p("We are given data about mileage achieved by cars when using Regular 
                             and Premium gasoline. The same car was tested on both types of gasoline and
                             mileage data was captured. Hence a Paired t-test is appropriate"),
                                    
                          p("The Null Hypothesis for this t-test is that gasoline type doesn't impact milesage.
                            The Alternative Hypothesis is that Premium Gasoline yields more miles than Regular gasoline"),
                          h5("Input Data:")
                          )
                        })
                      },
                      "Two Group t-test Example" = {
                        withTags({
                          div(class="intro",
                          h5("Two Group t-test example"),
                          p("We are given data about response time (ms) of two groups of patients: one group was administered a drug 
                             (treatment) and another group that was administered a placebo(control). Since two different groups are involved, 
                             a two sample t-test is appropriate"),
                                    
                            p("The Null Hypothesis for this t-test is that the treatment doesn't impact response times.
                              The Alternative Hypothesis is that treatment does impact response times"),
                              h5("Input Data:")
                              )
                            })
                          },        
                      "R Dataset: mtcars" = {
                        withTags({
                           div(class="intro",
                           h5("Two Group t-test example for dataset mtcars"),
                           p("We use data from the ", b("mtcars"), " dataset to determine if there is difference between mileage yield 
                             when comparing cars having automatic transmission with cars having manual transmission."),  
                           p("The Null Hypothesis for this t-test is that there is no diffrence between mileage, while the
                             Alterate hypothesis is that there is difference between mileage yeild of cars having manual transmission
                             as compared to cars having automatic transmission."),
                           h5("Input Data:"),
                           p(b("Note"), " that only the first 10 rows of data are shown, for brevity.")
                           )
                        })
                      },
                      "Upload Data" = {
                           if(is.null(input$inputFile))
                             withTags({
                               h5("Please upload data")
                             })
                          else {
                            ttype<-"Two group t-test"
                            if (input$paired)
                              ttype<-"Paired t-test"
                            withTags({
                              div(class = "intro",
                              h5(ttype),
                              br(),
                              h5("Input Data:")
                              )
                            })
                          }
                       }        
                    )
  })

  output$tbl<-renderTable({
    
    if(is.null(input$option))
      return()
    
    data<-switch(input$option, 
                   "Paired t-test Example" = {
                     rnames = c("Regular", "Premium")
                     cnames = c("Car1","Car2","Car3","Car4","Car5","Car6","Car7","Car8","Car9","Car10")
                     as.data.frame(matrix(c(reg,prem), nrow = 2, byrow = TRUE,  dimnames = list(rnames, cnames)))
                   },
                   "Two Group t-test Example" = {
                     rnames = c("Control", "Treatment")
                     cnames = c("S1","S2","S3","S4","S5","S6")
                     as.data.frame(matrix(c(control,treat), nrow = 2, byrow = TRUE,  dimnames = list(rnames, cnames)))
                   },
                   "R Dataset: mtcars" = {
                     d<-mtcars[,c("mpg", "am")]
                     row.names(d)<-NULL
                     names(d)<-c("Mileage", "Transmission")
                     d$Transmission[d$Transmission == 1.0]<-"Automatic"
                     d$Transmission[d$Transmission == 0.0]<-"Manual"
                     data<-d[1:10,]
                   },
                   "Upload Data" = {
                     if(is.null(input$inputFile))
                       return()
                     df<-read.csv(input$inputFile$datapath, header = input$header)
                     
                   }
               )
             })        
  
  output$contents2<-renderUI({
    
    if(is.null(input$option))
      return()
    
    switch(input$option, 
           "Paired t-test Example" = {
             withTags({
               div(
                 br(),
                 p(b("P Value:"), round(t.test(reg, prem, paired = TRUE, alternative = "less")$p.value,5)),
                 p("Since the p-value is less than 0.05, we reject the Null Hypothesis of no diffence in
                   mileage achieved between Regular and Premium gasoline.")
               )
            })
           },
           "Two Group t-test Example" = {
             withTags({
               div(
                 br(),
                 p(b("P Value:"), round(t.test(control, treat, paired = FALSE, var.equal = TRUE, alternative = "less")$p.value,5)),
                 p("Since the p-value is less than 0.05, we reject the Null Hypothesis that there is no change
                   in response time due to treatment.")
               )
             })
           },
           "R Dataset: mtcars" = {
             withTags({
               div(
                 br(),
                 p(b("P Value:"), round(t.test(mtcars$mpg[mtcars$am == 1],mtcars$mpg[mtcars$am == 0], 
                                               paired = FALSE, alternative = 'two.sided', var.equal = FALSE)$p.value,5)),
                 p("Since the p-value is less than 0.05, we reject the Null Hypothesis that cars with automatic transmission
                   have same or similar mileage as those with manual transmission."),
                 p(b("Note:"), "This conclusion is based on just the t-test. Additional tests should be performed to
                   check for the presence of confounding variables to verify or overturn the outcome of this test.")
               )
             })
           },
           "Upload Data" = {
             if(is.null(input$inputFile))
               return()

             df1<-read.csv(input$inputFile$datapath, header = input$header)
             ttestres<-t.test(as.numeric(df1[1,]),as.numeric(df1[2,]),
                              paired = input$paired, alternative = input$alternative, var.equal = input$variance)
             withTags({
               div(
                 br(),
                 p(b("T-statistic:"), round(ttestres$statistic,5)),
                 p(b("P Value:"), round(ttestres$p.value,5))
                 )
             })
           }
        )
  })
})
