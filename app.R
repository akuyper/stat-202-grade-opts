## app.R ##
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)

letter_grade_std <- function(score){
  cut_pts <- c(-Inf, 59.4, 69.4, 73.4, 76.4, 79.4, 83.4, 86.4, 89.4, 93.4, Inf)
  labs <- c("F", "D", "C-", "C", "C+", "B-", "B", "B+", "A-", "A")
  
  as.character(cut(score, breaks = cut_pts, labels = labs))
}


ui <- dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(
    title = "Data Science II (STAT 301-2) Grading Options - Winter 2020",
    titleWidth = 550
  ),
  
  dashboardSidebar(
    width = 355,
    div(style="margin-left: 16px", 
        br(),
        h3("Grading Options"),
        hr(),
        h4("Opt to submit a final report"),
        p("The grade category weights are the same as those outlined in
          the course syllabus which is provided below in more detail."),
        p("If a student's course grade would have been better by not submitting a final report,
          then the student will receive the better course grade."),
        "5% -- Attendance", br(),
        "10% -- Participation", br(),
        "60% -- Labs", br(),
        "2% -- Data Memo", br(),
        "5% -- Final Presentation", br(),
        "18% -- Final Report", br(),
        hr(),
        h4("Opt not to submit a final report"),
        p("The grade category weights change by simply taking the weight final report 
        and add it to the weight of the Labs category."),
        "5% -- Attendance", br(),
        "10% -- Participation", br(),
        strong("78% -- Labs"), br(),
        "2% -- Data Memo", br(),
        "5% -- Final Presentation", br(),
        strong("0% -- Final Report")
    )
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 6,
             
             # 
             box(
               title = "Grade Categories", status = "info", solidHeader = TRUE,
               width = NULL,
               
               p("Enter your grade information for each of the categories below.
                 You can find the information in the Grade section of the course 
                 Canvas page."),
               p("For the exams you will enter the actual/estiamted scores for the exams. 
                 For the other categories you will need to enter your grade percentage
                 for the respective categories.")
             ),
             
             
             # final exam info
             box(
               title = "Final Project Scores", status = "info", solidHeader = TRUE,
               collapsible = TRUE, width = NULL, collapsed = TRUE,
               
               # p("Enter estimated scores for each of portion of the final exam. 
               #   A good starting estimate would be the grade achieved on the corresponding 
               #   portion of the midterm exam."),
               
               sliderInput(
                 inputId = "final_report_slider", 
                 h4("Final Report (Estimated)"),
                 min = 0, 
                 max = 90, 
                 value = 75, 
                 round = -2, 
                 step = 0.25),
               
               sliderInput(
                 inputId = "final_presentation_slider", 
                 h4("Final Presentation (Estimated)"),
                 min = 0, 
                 max = 25, 
                 value = 20, 
                 round = -2, 
                 step = 0.25),
               
               sliderInput(
                 inputId = "data_memo_slider", 
                 h4("Data Memo"),
                 min = 0, 
                 max = 10, 
                 value = 10, 
                 round = -2, 
                 step = 0.25)
               
             ),
             
             # Low weight categories
             box(
               title = "Other Grading Categories", status = "info", solidHeader = TRUE,
               collapsible = TRUE, width = NULL, collapsed = TRUE,
               
               p("Find your percentages at the bottom of the Grade page of the course Canvas site."),
               
               sliderInput(
                 inputId = "lab_slider", 
                 h4("Labs"),
                 min = 0, 
                 max = 101, 
                 value = 90, 
                 round = -2, 
                 post = "%", 
                 step = 0.01),
               
               sliderInput(
                 inputId = "attend_slider", 
                 h4("Attendance"),
                 min = 0, 
                 max = 100, 
                 value = 95, 
                 round = -2, 
                 post = "%", 
                 step = 0.01),
               
               sliderInput(
                 inputId = "part_slider", 
                 h4("Participation"),
                 min = 0, 
                 max = 100, 
                 value = 100, 
                 round = -2, 
                 post = "%", 
                 step = 0.01)
              
             )
      ),
      
      column(width = 6,
             
             box(
               title = "Your Estimated Grade", status = "success", 
               solidHeader = TRUE, width = NULL,
               
               h4(strong(textOutput("grade"))),
               # p("This is an estimated course grade.", style = "font-size:12px"),
               
               hr(),
               p("We take the best of the two possible grading options. 
                 See side bar for more details."),
               br(),
               h5(strong("Course Grade Under Each Option:")),
               textOutput("grade_with_final"),
               textOutput("grade_no_final")
             ),
             
             box(
               title = "Important Note", status = "danger", 
               solidHeader = TRUE, width = NULL,
               
               p("This app is meant to help students determine if they want to take the online final exam.
               Please remember this app supplies an ESTIMATED course grade."),
               
               p("")
             ),
             
             box(
               title = "Grading Note From Syllabus", status = "warning", 
               solidHeader = TRUE, width = NULL,
               
               p("Final grades will be rounded to nearest tenth of a percent. We reserve the right to alter
                 the course grading scale. However, any alterations will be limited to those that would be 
                 beneficial to students (i.e. an upward grade curve).")
             )
             
      ),
      
      
    )
  ))


server <- function(input, output) {
  
  opt_no_final <- reactive({
    5 * input$final_presentation_slider/25 +
      2 * input$data_memo_slider/10 +
      78 * input$lab_slider/100 +
      5 * input$attend_slider/100 +
      10 * input$part_slider/100
  })
  
  opt_with_final <- reactive({
    18 * input$final_report_slider/90 +
      5 * input$final_presentation_slider/25 +
      2 * input$data_memo_slider/10 +
      60 * input$lab_slider/100 +
      5 * input$attend_slider/100 +
      10 * input$part_slider/100
  })
  
  best_grade <- reactive({
    if(opt_with_final() > opt_no_final()){
      grade <- opt_with_final() 
    }else{
      grade <- opt_no_final() 
    }
    
    grade
  })
  
  
  output$grade_no_final <- renderText({ 
    paste0("Opt not to submit a final report: ", 
           round(opt_no_final(), 2),
           "% (",
           letter_grade_std(opt_no_final()),
           ")")
  })
  
  output$grade_with_final <- renderText({ 
    paste0("Opt to submit a final report: ", 
           round(opt_with_final(), 2),
           "% (",
           letter_grade_std(opt_with_final()),
           ")")
  })
  
  output$grade <- renderText({ 
    paste0(round(best_grade(), 2),
           "% (",
           letter_grade_std(best_grade()),
           ")")
  })
  
}

shinyApp(ui, server)