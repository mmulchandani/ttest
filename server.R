library(shiny)
library(googleVis)  # unused currently

# Paired Samples t-test example data
reg = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
prem = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)

# Independent Samples t-test example data
control = c(91, 87, 99, 77, 88, 91)
treat = c(101, 110, 103, 93, 99, 104)

# mtcars dataset
data(mtcars)
d<-mtcars[,c("mpg", "am")]
row.names(d)<-NULL
names(d)<-c("Mileage", "Transmission")
d$Transmission[d$Transmission == 1.0]<-"Automatic"
d$Transmission[d$Transmission == 0.0]<-"Manual"
d<-d[1:10,]

# Saving options associated with fileInput
# Required since UI is dynamically build
# Default options
pairedOption<-FALSE
equalVarOption<-FALSE
headerOption<-FALSE
altHOption<-"two.sided"

shinyServer(function(input,output, session) {
  
  # Output for App Usage Guide tab
  
  usageGuidelines<-renderUI(
    withTags({
      div(class = "usage",
          h3("Notes on using this Shiny App"),
          br(),
          p("This Shiny App demonstrates, using examples, some of typical applications of Student's t-test. 
            In particular, it demonstates the use of", b("Paired Samples t-test"), "and", b("Independent Samples t-test"), "and how
            these tests can be used to compare two sets of data (either paired data or data from two independent groups) and determine
            if there is a statistically significant difference between the two sets of data. This App also has a provision 
            to enable users to upload their data and execute the appropriate t-test on the uploaded data."),
          p("Information about Students's t-tests can be found ",
                  a("here.", href = "http://en.wikipedia.org/wiki/Student's_t-test", target = "_blank")),
          br(),
          p("Demonstration of the application of t-test is done using the following three examples:"),
          ol(li("Paired Samples t-test Example"), li("Independent Samples t-test Example"), li("Independent Samples t-test using R's", b("mtcars"), "dataset")),
          p("Users can use a drop down list to select the different examples, as shown in the image below"),
          img(src = "pic8.png", width = 200, height = 400),
          br(),
          br(),
          p(b("Note:"),"The fifth option in the drop down list enables the user to upload their data and run
            t-test on the uploaded data. The", b("Upload File"), "file chooser becomes available once the user
            selects to upload data. The user can specify if the file has a header or not (default: No header). 
            In addition to the file chooser, the App provides options to specify paired samples or independent samples test 
            (default: independent samples test), select the type of alternative hypothesis (default: Not equal or two sided), 
            and to specify if two sets of data have equal variance (default: not equal); note that the final option 
            applies only to the independent samples test and does not affect the paired samples test."),
          p("The App 'remembers' the last uploaded file including the options specified for t-test for the uploaded data"),
          p(b("Note"), "that the app currently handles .csv files only. Only the first two columns of data and
            the optional header are accessed."),
          p("Links to two sample files, one with a header and another without a header, are provided in the",
             b("Credits and other Links"), "tab. Instructions to download files are also provided. Files that 
                do not conform to the format can produce unpredictable results."),
          br(),    
          h5("Outputs"),
          p("The output of the t-test is shown in the", b("Main"), "panel. A boxplot for the data is shown in
             the", b("Boxplot"), "panel."),
          br(),
          p("Finally, please look up the", b("Limitations"), "tab that describes some of the limitations of this App.")
      )
    })
  )
  
  output$usage<-usageGuidelines
  output$usagemain<-usageGuidelines  # for future use
  
  # Output for Credits tab
  output$credits<-renderUI({
    withTags({
      div(
        h4("Credits and other Links"),
        p("The first two examples were from this ", 
          a('document.', href="http://www.stat.columbia.edu/~martin/W2024/R2.pdf", target = "_blank"),
          "This was among the top Google search results!!"),
        br(),
        p("Here are links to two sample files for use with the",b("Upload Data"),"option"),
        ol(
          li(a("Sample File with Header.", 
               href = "https://github.com/mmulchandani/ttest/blob/master/traintimeshead.csv", target = "_blank")),
          li(a("Sample File without Header.", 
               href = "https://github.com/mmulchandani/ttest/blob/master/traintimes.csv", target = "_blank"))
        ),
        p("Clicking on the link will open a new window. On that window, locate the", b("Raw"), "button. Clicking on the",
          b("Raw"), "button will open another window with data in plain text format. Save this to a file"),
        p("The data for these sample files was obtained from this ",
                a("webpage.", href = "http://www.r-bloggers.com/paired-students-t-test/", target = "_blank"))      
        )
    })  
  })
  
  # Output for limitations tab
  output$limits<-renderUI({
    withTags({
      div(
        h4("Limitations"),
        ol(
          li("The App currently doesn't demonstrate one sample t-test."),
          li("The", b("Upload Data"), "feature has not been extensively tested with all different types
             of data. It has actually been minimally tested with a simple dataset such as", 
             a("this.", href = "https://github.com/mmulchandani/ttest/blob/master/traintimes.csv", target = "_blank"),
             "In case you have a dataset that this App fails on, please email the same to me at mmulchandani84@gmail.com.")
        )
      )
    })
  })
  
  # Renders the left hand navigation panel
  output$choose_dataset <- renderUI({
    selectInput("option", "Choose Dataset", c("Welcome Message", 
                                              "Paired Samples t-test Example", 
                                              "Independent Samples t-test Example", 
                                              "R Dataset: mtcars", 
                                              "Upload Data"),
                selected = "Welcome Message")
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
             checkboxInput('header', "Header Present", value = headerOption)
           }
    )
  })
  
  # Dynamic input element (2a) for "Upload Data" option
  # Horizontal Rule
  output$hr<-renderUI({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             tags$hr(style="height:1px;border-width:0;background-color:blue")
           }
    )
  })
  
  # Dynamic input element (3) for "Upload Data" option
  # Text
  output$text<-renderUI({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             "t-test options:"
           }
    )
  })
  
  # Dynamic input element (3a) for "Upload Data" option
  # Check box to specify if Paired t-test is to be 
  # performed. Default is Two Group test
  output$cboxpaired<-renderUI({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             checkboxInput('paired', "Paired", value = pairedOption)
           }
    )
  })
  
  # Dynamic input element (3b) for "Upload Data" option
  # Check box to specify if variance is equal in case 
  # of two group test. Default is not equal
  output$cboxvar<-renderUI({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             checkboxInput('variance', "Equal Variance", value = equalVarOption)
           }
    )
  })
  
  # Dynamic input element (4) for "Upload Data" option
  # Radio buttons to specify type of test: two sided 
  # or less than or greater than.
  # Default is two sided
  output$alternate<-renderUI ({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             radioButtons("alternative", "Alternative Hypothesis",
                 c("Not Equal" = "two.sided",
                   "Less Than" = "less",
                   "Greater Than" = "greater"), selected = altHOption)
           }
    )
  })
  
  # Dynamic input element (5) for "Upload Data" option
  # Text message and link to sample data file for upload
  output$linkmsg<-renderUI ({
    
    if(is.null(input$option))
      return()
    
    switch(input$option,
           "Upload Data" = {
             withTags({
               div(
                 p("A sample file tested with this application can be obtained from",
                   a("here.", href = "https://github.com/mmulchandani/ttest/blob/master/traintimeshead.csv",
                     target = "_blank"))
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
      "Welcome Message" = {
         withTags({
           div(
             h3("Welcome"),
             p("This is the Main Panel. All output is displayed in this panel and the", b("Box Plot"), "panel. As the
                name suggests, the", b("Box Plot"), "panel plots a boxplot of the data."),
             p("Given below are some notes on getting started with this application. You can also access these notes
               from the", b("Usage Guide"), "tab."),
             p("When you are ready, please select an option from the drop down list."),
             br(),
             h3("Notes on using this Shiny App"),
             br(),
             p("This Shiny App demonstrates, using examples, some of typical applications of Student's t-test. 
               In particular, it demonstates the use of", b("Paired Samples t-test"), "and", b("Independent Samples t-test"), "and how
               these tests can be used to compare two sets of data (either paired data or data from two independent groups) and determine
               if there is a statistically significant difference between the two sets of data. This App also has a provision 
               to enable users to upload their data and execute the appropriate t-test on the uploaded data."),
             p("Information about Students's t-tests can be found ",
                  a("here.", href = "http://en.wikipedia.org/wiki/Student's_t-test", target = "_blank")),
             br(),
             p("Demonstration of the application of t-test is done using the following three examples:"),
             ol(li("Paired Samples t-test Example"), li("Independent Samples t-test Example"), li("Independent Samples t-test using R's", b("mtcars"), "dataset")),
             p("Users can use a drop down list to select the different examples, as shown in the image below"),
             img(src = "pic8.png", width = 200, height = 400),
             br(),
             br(),
             p(b("Note:"),"The fifth option in the drop down list enables the user to upload their data and run
               t-test on the uploaded data. The", b("Upload File"), "file chooser becomes available once the user
               selects to upload data. The user can specify if the file has a header or not (default: No header). 
               In addition to the file chooser, the App provides options to specify paired samples or independent samples test 
               (default: independent samples test), select the type of alternative hypothesis (default: Not equal or two sided), 
               and to specify if two sets of data have equal variance (default: not equal); note that the final option 
               applies only to the independent samples test and does not affect the paired samples test."),
             p("The App 'remembers' the last uploaded file including the options specified for t-test for the uploaded data"),
             p(b("Note"), "that the app currently handles .csv files only. Only the first two columns of data and
               the optional header are accessed."),
             p("Links to two sample files, one with a header and another without a header, are provided in the",
             b("Credits and other Links"), "tab. Instructions to download files are also provided. Files that 
                do not conform to the format can produce unpredictable results."),
             br(),
             h5("Outputs"),
             p("The output of the t-test is shown in the", b("Main"), "panel. A boxplot for the data is shown in
                the", b("Boxplot"), "panel."),
             br(),
             p("Finally, please look up the", b("Limitations"), "tab that describes some of the limitations of this App.")
           )
         })
      },
      "Paired Samples t-test Example" = {
        withTags({
          div(class="intro",
              h5("Paired Samples t-test Example"),
              p("We are given data about mileage achieved by cars when using Regular 
                and Premium gasoline. The same car was tested on both types of gasoline and
                mileage data was captured. Hence a Paired Samples t-test Example is appropriate."),
                                    
                p("The Null Hypothesis for this t-test is that gasoline type doesn't change the mileage yield.
                  The Alternative Hypothesis is that Premium Gasoline yields more miles than Regular gasoline."),
                h5("Input Data:")
              )
          })
        },
      "Independent Samples t-test Example" = {
        withTags({
          div(class="intro",
              h5("Two Group t-test example"),
              p("We are given data about response time (ms) of two groups of patients: one group was administered a drug 
                (treatment) and another group that was administered a placebo(control). Since two different groups are involved, 
                 a two sample t-test is appropriate."),
              p("The Null Hypothesis for this t-test is that the treatment doesn't impact response times.
                The Alternative Hypothesis is that treatment does impact response times."),
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
              p("The Null Hypothesis for this t-test is that there is no difference between mileage, while the
                 Alterate hypothesis is that there is difference between mileage yield of cars having manual transmission
                 as compared to cars having automatic transmission."),
              h5("Input Data:"),
              p(b("Note"), " that only the first 10 rows of data are shown, for brevity.")
              )
          })
        },
      "Upload Data" = {
        if(is.null(input$inputFile))
          withTags({
            h5("Click on the", b("Browse"), "button to select and upload a file")
            })
        else {
          ttype<-"Independent Samples t-test"
          if (input$paired)
            ttype<-"Paired Samples t-test"
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
    
    switch(input$option, 
      "Paired Samples t-test Example" = {
        rnames = c("Regular", "Premium")
        cnames = c("Car1","Car2","Car3","Car4","Car5","Car6","Car7","Car8","Car9","Car10")
        as.data.frame(matrix(c(reg,prem), nrow = 2, byrow = TRUE,  dimnames = list(rnames, cnames)))
        },
      "Independent Samples t-test Example" = {
        rnames = c("Control", "Treatment")
        cnames = c("S1","S2","S3","S4","S5","S6")
        as.data.frame(matrix(c(control,treat), nrow = 2, byrow = TRUE,  dimnames = list(rnames, cnames)))
        },
      "R Dataset: mtcars" = {
        d
        },
      "Upload Data" = {
        headerOption<<-input$header
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
      "Paired Samples t-test Example" = {
        withTags({
          div(
            br(),
            p(b("P Value:"), round(t.test(reg, prem, paired = TRUE, alternative = "less")$p.value,5)),
            p("Since the p-value is less than 0.05, we reject the Null Hypothesis of no difference in
               mileage yield obtained from Regular and Premium gasoline.")
          )
        })
      },
      "Independent Samples t-test Example" = {
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
        pairedOption<<-input$paired
        altHOption<-input$alternative
        varEqualOption<-input$variance
        if(is.null(input$inputFile))
          return()
        
        df1<-read.csv(input$inputFile$datapath, header = input$header)
        ttestres<-t.test(as.numeric(df1[,1]),as.numeric(df1[,2]),
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

  output$boxplot<-renderPlot({
  
  if(is.null(input$option))
    return()
  
  switch(input$option, 
    "Paired Samples t-test Example" = {           
      boxplot(reg - prem, col = 'tan', main = "Box Plot of Difference between Regular and Premium",
                   ylab = "Difference in Mileage", names = c("Difference"))
    },
    "Independent Samples t-test Example" = {
      boxplot(control, treat, col = 'salmon', main = "Box Plot of Control vs Treatment",
                   ylab = "Response Time", names = c("Control", "Treatment"))
    },
    "R Dataset: mtcars" = {
      boxplot(mtcars$mpg[mtcars$am == 0], mtcars$mpg[mtcars$am == 1], col = 'orange',
              ylab = "Mileage", names = c("Automatic", "Manual"),
              main = "Box Plot: Mileage of Automatic vs Manual Transmission")
    },
    "Upload Data" = {
      if(is.null(input$inputFile))
        return()
      
      df1<-read.csv(input$inputFile$datapath, header = input$header)
      if(!is.null(input$header)) {
        colnames<-names(df1)
      }
      else {
        colnames<-c("Var1", "Var2")
      }
      
      if(input$paired) {
        if(input$header) {
          title<-paste("Box Plot of Difference between", names(df1)[1], "and", names(df1)[2], sep = " ")
        }
        else {
          title<-"Box Plot of Difference between two measurements"
        }
        boxplot(as.numeric(df1[1,]) - as.numeric(df1[2,]), 
                names = c("Difference"), ylab = "Value", col = 'lightblue', main = title)
      }
      else {
        if(input$header) {
          title<-paste("Box Plot of", names(df1)[1], "vs", names(df1)[2], sep = " ")
        }
        else {
          title<-"Box Plot of Var1 vs Var2"
        }
  
        boxplot(as.numeric(df1[1,]),
                 as.numeric(df1[2,]), 
                 names = colnames, ylab = "Value", col = 'lightblue', main = title)
      }
    }
  )
  })

  # To switch to 'Main' tab, when a new option is selected
  observe({
  
    input$option
    updateTabsetPanel(session, "maintabs", selected = "Main")
    
    })
})
