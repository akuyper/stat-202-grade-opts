## app.R ##
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)

letter_grade_std <- function(score) {
  cut_pts <- c(-Inf, 60, 70, 73, 77, 80, 83, 87, 90, 93, Inf)
  labs <- c("F", "D", "C-", "C", "C+", "B-", "B", "B+", "A-", "A")

  as.character(cut(score, breaks = cut_pts, labels = labs, right = FALSE))
}

format_grade <- function(score) {
  paste0(round(score, 2), "% (", letter_grade_std(score), ")")
}

ui <- dashboardPage(
  skin = "purple",

  dashboardHeader(
    title = "STAT 202 Grading Options - Spring 2026",
    titleWidth = 550
  ),

  dashboardSidebar(
    width = 355,
    div(
      style = "margin-left: 16px",
      br(),
      h3("Optional Final Exam Policy"),
      hr(),
      h4("Without final exam"),
      p("This uses the standard course category weights from the syllabus."),
      "5% -- Small Assignments", br(),
      "5% -- Reading Tutorials", br(),
      "10% -- Learning Checks", br(),
      "20% -- Homework", br(),
      "30% -- Exam 1", br(),
      "30% -- Exam 2", br(),
      hr(),
      h4("With optional final exam"),
      p("The final exam can only improve your grade. If this option is better,
        Exam 1 and Exam 2 each count for 15%, and the final exam counts for 30%."),
      "5% -- Small Assignments", br(),
      "5% -- Reading Tutorials", br(),
      "10% -- Learning Checks", br(),
      "20% -- Homework", br(),
      strong("15% -- Exam 1"), br(),
      strong("15% -- Exam 2"), br(),
      strong("30% -- Final Exam")
    )
  ),

  dashboardBody(
    fluidRow(
      column(
        width = 6,

        box(
          title = "Grade Categories", status = "info", solidHeader = TRUE,
          width = NULL,

          p("Enter your current or estimated percentage for each grade category.
            You can find existing category scores in the Grades section of Canvas."),
          p("Use the final exam slider to estimate the score you might earn on the
            optional cumulative final exam.")
        ),

        box(
          title = "Coursework Scores", status = "info", solidHeader = TRUE,
          collapsible = TRUE, width = NULL,

          sliderInput(
            inputId = "small_assignments_slider",
            label = h4("Small Assignments"),
            min = 0,
            max = 100,
            value = 95,
            round = -2,
            post = "%",
            step = 0.01
          ),

          sliderInput(
            inputId = "reading_tutorials_slider",
            label = h4("Reading Tutorials"),
            min = 0,
            max = 100,
            value = 95,
            round = -2,
            post = "%",
            step = 0.01
          ),

          sliderInput(
            inputId = "learning_checks_slider",
            label = h4("Learning Checks"),
            min = 0,
            max = 100,
            value = 95,
            round = -2,
            post = "%",
            step = 0.01
          ),

          sliderInput(
            inputId = "homework_slider",
            label = h4("Homework"),
            min = 0,
            max = 100,
            value = 90,
            round = -2,
            post = "%",
            step = 0.01
          )
        ),

        box(
          title = "Exam Scores", status = "info", solidHeader = TRUE,
          collapsible = TRUE, width = NULL,

          sliderInput(
            inputId = "exam_1_slider",
            label = h4("Exam 1"),
            min = 0,
            max = 100,
            value = 85,
            round = -2,
            post = "%",
            step = 0.01
          ),

          sliderInput(
            inputId = "exam_2_slider",
            label = h4("Exam 2"),
            min = 0,
            max = 100,
            value = 85,
            round = -2,
            post = "%",
            step = 0.01
          ),

          sliderInput(
            inputId = "final_exam_slider",
            label = h4("Optional Final Exam (Estimated)"),
            min = 0,
            max = 100,
            value = 85,
            round = -2,
            post = "%",
            step = 0.01
          )
        )
      ),

      column(
        width = 6,

        box(
          title = "Your Estimated Grade", status = "success",
          solidHeader = TRUE, width = NULL,

          h4(strong(textOutput("grade"))),
          hr(),
          p("The reported estimate uses the better of the two grading options
            described in the syllabus."),
          br(),
          h5(strong("Course Grade Under Each Option:")),
          textOutput("grade_no_final"),
          textOutput("grade_with_final")
        ),

        box(
          title = "Important Note", status = "danger",
          solidHeader = TRUE, width = NULL,

          p("This app is meant to help students estimate whether the optional final
            exam could improve their course grade. It is not an official grade
            calculation.")
        ),

        box(
          title = "Grading Scale From Syllabus", status = "warning",
          solidHeader = TRUE, width = NULL,

          "93.0-100% -- A", br(),
          "90.0-92.9% -- A-", br(),
          "87.0-89.9% -- B+", br(),
          "83.0-86.9% -- B", br(),
          "80.0-82.9% -- B-", br(),
          "77.0-79.9% -- C+", br(),
          "73.0-76.9% -- C", br(),
          "70.0-72.9% -- C-", br(),
          "60.0-69.9% -- D", br(),
          "Below 60.0% -- F"
        )
      )
    )
  )
)

server <- function(input, output) {
  no_final <- reactive({
    5 * input$small_assignments_slider / 100 +
      5 * input$reading_tutorials_slider / 100 +
      10 * input$learning_checks_slider / 100 +
      20 * input$homework_slider / 100 +
      30 * input$exam_1_slider / 100 +
      30 * input$exam_2_slider / 100
  })

  with_final <- reactive({
    5 * input$small_assignments_slider / 100 +
      5 * input$reading_tutorials_slider / 100 +
      10 * input$learning_checks_slider / 100 +
      20 * input$homework_slider / 100 +
      15 * input$exam_1_slider / 100 +
      15 * input$exam_2_slider / 100 +
      30 * input$final_exam_slider / 100
  })

  best_grade <- reactive({
    max(no_final(), with_final())
  })

  output$grade_no_final <- renderText({
    paste0("Do not take optional final exam: ", format_grade(no_final()))
  })

  output$grade_with_final <- renderText({
    paste0("Take optional final exam: ", format_grade(with_final()))
  })

  output$grade <- renderText({
    format_grade(best_grade())
  })
}

shinyApp(ui, server)
