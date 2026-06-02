## app.R ##
library(shiny)
library(bslib)

accent_blue <- "#2563EB"
accent_teal <- "#0F766E"
ink_color <- "#1F2937"

letter_grade_std <- function(score) {
  cut_pts <- c(-Inf, 60, 70, 73, 77, 80, 83, 87, 90, 93, Inf)
  labs <- c("F", "D", "C-", "C", "C+", "B-", "B", "B+", "A-", "A")

  as.character(cut(score, breaks = cut_pts, labels = labs, right = FALSE))
}

format_grade <- function(score) {
  paste0(round(score, 2), "% (", letter_grade_std(score), ")")
}

format_points <- function(score) {
  paste0("+", round(score, 2), " pts")
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

clamp <- function(x, min_value, max_value) {
  pmax(min_value, pmin(max_value, x))
}

bounded_value <- function(x, default, min_value, max_value) {
  if (is.null(x) || is.na(x)) {
    return(default)
  }

  clamp(x, min_value, max_value)
}

grade_value <- function(x) {
  bounded_value(x, default = 80, min_value = 0, max_value = 100)
}

activity_count_value <- function(x) {
  round(bounded_value(x, default = 0, min_value = 0, max_value = 20))
}

grade_input <- function(input_id, label, value, min = 0, max = 100) {
  div(
    class = "grade-input",
    numericInput(
      inputId = input_id,
      label = paste0(label, " (%)"),
      min = min,
      max = max,
      value = value,
      step = 0.1,
      width = "100%",
      updateOn = "blur"
    )
  )
}

activity_input <- function(input_id, label, value) {
  div(
    class = "grade-input",
    numericInput(
      inputId = input_id,
      label = label,
      min = 0,
      max = 20,
      value = value,
      step = 1,
      width = "100%",
      updateOn = "blur"
    )
  )
}

policy_row <- function(category, standard, final) {
  div(
    class = "policy-row",
    span(class = "policy-category", category),
    span(standard),
    span(final)
  )
}

grade_badge <- function(range, grade) {
  div(class = "grade-badge", span(class = "grade-letter", grade), span(range))
}

ui <- page_sidebar(
  title = div(
    class = "app-title",
    span(class = "course-code", "STAT 202"),
    span("Optional Final Exam - Spring 2026"),
    div(
      class = "mode-toggle",
      input_dark_mode(id = "color_mode", mode = "light")
    )
  ),
  theme = bs_theme(
    version = 5,
    primary = accent_blue,
    secondary = "#64748B",
    success = accent_teal,
    danger = "#B42318",
    warning = "#B45309",
    bg = "#F8FAFC",
    fg = ink_color,
    base_font = font_google("Source Sans 3"),
    heading_font = font_google("Source Sans 3"),
    font_scale = 1.08
  ),
  tags$head(
    tags$style(HTML(paste0(
      ":root { --accent: ", accent_blue, "; --success: ", accent_teal,
      "; --surface: #FFFFFF; --surface-soft: #F1F5F9; --border-soft: #CBD5E1; --ink: #1F2937; --ink-soft: #475569; --accent-soft: #EFF6FF; }\n",
      "html[data-bs-theme='dark'] { --surface: #18202B; --surface-soft: #111827; --border-soft: #334155; --ink: #E5E7EB; --ink-soft: #CBD5E1; --accent-soft: #172554; --accent: #93C5FD; --success: #5EEAD4; }\n",
      "body { background: #F8FAFC; color: var(--ink); font-size: 17px; }\n",
      "html[data-bs-theme='dark'] body { background: #0F172A; color: var(--ink); }\n",
      ".bslib-page-title { background: var(--surface); border-bottom: 4px solid var(--accent); color: var(--ink); padding: .55rem .9rem; }\n",
      "html[data-bs-theme='dark'] .bslib-page-title { background: #111827; }\n",
      ".app-title { display: flex; gap: .55rem; align-items: baseline; flex-wrap: wrap; font-weight: 800; width: 100%;}\n",
      ".course-code { color: var(--accent); font-size: .9rem; font-weight: 900; letter-spacing: .06em; text-transform: uppercase; }\n",
      ".sidebar { background: var(--surface); border-right: 1px solid var(--border-soft); }\n",
      ".sidebar h2 { color: var(--ink); font-size: 1.14rem; margin: .1rem 0 .42rem; }\n",
      ".mode-toggle { align-items: center; display: flex; gap: .75rem; justify-content: flex-end; margin-bottom: 0; margin-left: auto; }\n",
      ".mode-toggle label { color: var(--ink); font-weight: 750; margin-bottom: 0; }\n",
      ".grade-help { color: var(--ink-soft); font-size: .9rem; font-weight: 700; margin: 0 0 .45rem; }\n",
      ".grade-grid, .grade-scale-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: .42rem .6rem; max-width: 560px; }\n",
      ".grade-input label { color: var(--ink); font-size: .86rem; font-weight: 800; margin-bottom: .08rem; }\n",
      ".grade-input .form-group { margin-bottom: 0; }\n",
      ".grade-input .form-control { background: var(--surface); border-color: var(--border-soft); color: var(--ink); font-size: 1rem; font-weight: 750; min-height: 1.95rem; padding: .18rem .48rem; }\n",
      ".grade-input .form-control:focus { border-color: var(--accent); box-shadow: 0 0 0 .15rem rgba(37, 99, 235, .18); }\n",
      ".policy-table { display: grid; font-size: .92rem; gap: .12rem; }\n",
      ".policy-row { align-items: center; border-bottom: 1px solid var(--border-soft); display: grid; gap: .38rem; grid-template-columns: minmax(0, 1.25fr) .72fr .72fr; padding: .2rem .05rem; }\n",
      ".policy-row.header { color: var(--accent); font-size: .76rem; font-weight: 900; text-transform: uppercase; }\n",
      ".policy-category { font-weight: 750; }\n",
      ".bonus-note { background: var(--accent-soft); border-left: 4px solid var(--accent); color: var(--ink); font-size: .86rem; font-weight: 750; line-height: 1.3; margin: .65rem 0 0; padding: .42rem .48rem; }\n",
      ".sidebar-footnote { border-top: 1px solid var(--border-soft); color: var(--ink-soft); font-size: .78rem; line-height: 1.25; margin-top: .65rem; padding-top: .48rem; }\n",
      ".bslib-grid { align-items: start; }\n",
      ".card { align-self: start; background: var(--surface); border: 1px solid var(--border-soft); box-shadow: none; }\n",
      ".card-body { padding: .58rem .66rem; }\n",
      ".card-header { background: var(--surface-soft); border-bottom: 1px solid var(--border-soft); color: var(--ink); font-size: 1rem; font-weight: 900; padding: .42rem .66rem; }\n",
      ".results-card { border-top: 4px solid var(--accent); }\n",
      ".estimated-grade { color: var(--accent); font-size: clamp(2rem, 4.2vw, 3.35rem); font-weight: 900; line-height: 1; margin: 0 0 .32rem; }\n",
      ".improvement { background: rgba(15, 118, 110, .1); border: 1px solid rgba(15, 118, 110, .28); border-radius: .45rem; margin-bottom: .34rem; padding: .42rem .55rem; }\n",
      ".improvement-label { color: var(--ink-soft); display: block; font-size: .78rem; font-weight: 850; text-transform: uppercase; }\n",
      ".improvement-value { color: var(--success); display: block; font-size: clamp(1.35rem, 3vw, 2.15rem); font-weight: 900; line-height: 1.05; }\n",
      ".policy-result { border-top: 1px solid var(--border-soft); display: flex; gap: .8rem; justify-content: space-between; padding: .3rem 0; }\n",
      ".policy-result span:first-child { color: var(--ink-soft); font-weight: 800; }\n",
      ".policy-result span:last-child { color: var(--ink); font-weight: 900; white-space: nowrap; }\n",
      ".grade-badge { align-items: center; background: var(--accent-soft); border: 1px solid var(--border-soft); border-left: 4px solid var(--accent); border-radius: .35rem; display: flex; font-size: .9rem; gap: .45rem; justify-content: space-between; padding: .28rem .42rem; }\n",
      ".grade-letter { color: var(--accent); font-weight: 900; }\n",
      ".accordion { max-width: 560px; --bs-accordion-btn-padding-y: .4rem; --bs-accordion-btn-padding-x: .66rem; --bs-accordion-body-padding-y: .55rem; --bs-accordion-body-padding-x: .6rem; }\n",
      ".accordion-button { background: var(--surface-soft); color: var(--ink); font-weight: 900; }\n",
      ".accordion-body { background: var(--surface); }\n",
      "@media (max-width: 720px) { .grade-grid, .grade-scale-grid { grid-template-columns: 1fr; max-width: none; } .policy-row { grid-template-columns: minmax(0, 1.1fr) .7fr .7fr; } }\n"
    )))
  ),
  sidebar = sidebar(
    width = 330,
    h2("Weighting Policies"),
    div(
      class = "policy-table",
      div(
        class = "policy-row header",
        span("Category"), span("Standard"), span("Final")
      ),
      policy_row("Small Assignments", "5%", "5%"),
      policy_row("Reading Tutorials", "5%", "5%"),
      policy_row("Learning Checks", "10%", "10%"),
      policy_row("Homework", "20%", "20%"),
      policy_row("Exam 1", "30%", "15%"),
      policy_row("Exam 2", "30%", "15%"),
      policy_row("Final Exam", "0%", "30%")
    ),
    p(
      class = "bonus-note",
      "Lecture activity bonus: earn up to +1 percentage point on your final course grade."
    ),
    div(
      class = "sidebar-footnote",
      "Disclaimer: this app is an estimate for planning around the optional final exam, not an official grade calculation."
    )
  ),
  layout_columns(
    col_widths = c(7, 5),
    gap = "0.7rem",
    fill = FALSE,
    fillable = FALSE,
    card(
      fill = FALSE,
      card_header("Grade Inputs"),
      card_body(
        p(class = "grade-help", "Enter category scores as percentages."),
        div(
          class = "grade-grid",
          grade_input("small_assignments", "Small Assignments", 90),
          grade_input("reading_tutorials", "Reading Tutorials", 90),
          grade_input("learning_checks", "Learning Checks", 85),
          grade_input("homework", "Homework", 85),
          activity_input("bonus_activities", "Completed Bonus Activities (0-20)", 0),
          grade_input("exam_1", "Exam 1", 80),
          grade_input("exam_2", "Exam 2", 80),
          uiOutput("final_exam_control")
        )
      )
    ),
    card(
      class = "results-card",
      fill = FALSE,
      card_header("Results"),
      card_body(
        div(class = "estimated-grade", textOutput("grade", inline = TRUE)),
        div(
          class = "improvement",
          span(class = "improvement-label", "Optional final improvement"),
          span(class = "improvement-value", textOutput("grade_difference", inline = TRUE))
        ),
        div(
          class = "improvement",
          span(class = "improvement-label", "Lecture activity bonus"),
          span(class = "improvement-value", textOutput("course_bonus", inline = TRUE))
        ),
        div(
          class = "policy-result",
          span("Standard policy"),
          span(textOutput("grade_no_final", inline = TRUE))
        ),
        div(
          class = "policy-result",
          span("Optional final policy"),
          span(textOutput("grade_with_final", inline = TRUE))
        )
      )
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        "Grading Scale",
        div(
          class = "grade-scale-grid",
          grade_badge("93.0-100%", "A"),
          grade_badge("90.0-92.9%", "A-"),
          grade_badge("87.0-89.9%", "B+"),
          grade_badge("83.0-86.9%", "B"),
          grade_badge("80.0-82.9%", "B-"),
          grade_badge("77.0-79.9%", "C+"),
          grade_badge("73.0-76.9%", "C"),
          grade_badge("70.0-72.9%", "C-"),
          grade_badge("60.0-69.9%", "D"),
          grade_badge("Below 60.0%", "F")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  keep_numeric_in_bounds <- function(input_id, min_value, max_value, default) {
    observeEvent(input[[input_id]], {
      current_value <- input[[input_id]]
      corrected_value <- bounded_value(current_value, default, min_value, max_value)

      if (is.null(current_value) || is.na(current_value) || !isTRUE(all.equal(current_value, corrected_value))) {
        updateNumericInput(
          session,
          inputId = input_id,
          min = min_value,
          max = max_value,
          value = corrected_value
        )
      }
    }, ignoreInit = FALSE)
  }

  grade_input_ids <- c(
    "small_assignments",
    "reading_tutorials",
    "learning_checks",
    "homework",
    "exam_1",
    "exam_2"
  )

  lapply(grade_input_ids, keep_numeric_in_bounds, min_value = 0, max_value = 100, default = 80)
  keep_numeric_in_bounds("bonus_activities", min_value = 0, max_value = 20, default = 0)

  observeEvent(input$bonus_activities, {
    current_value <- input$bonus_activities
    corrected_value <- activity_count_value(current_value)

    if (is.null(current_value) || is.na(current_value) || !isTRUE(all.equal(current_value, corrected_value))) {
      updateNumericInput(
        session,
        inputId = "bonus_activities",
        min = 0,
        max = 20,
        step = 1,
        value = corrected_value
      )
    }
  }, ignoreInit = FALSE)

  exam_average <- reactive({
    mean(c(grade_value(input$exam_1), grade_value(input$exam_2)))
  })

  final_exam_minimum <- reactive({
    round(exam_average(), 2)
  })

  course_bonus <- reactive({
    activity_count_value(input$bonus_activities) / 20
  })

  final_exam_score_value <- reactiveVal(NULL)

  output$final_exam_control <- renderUI({
    final_min <- final_exam_minimum()

    div(
      class = "grade-input",
      numericInput(
        inputId = "final_exam",
        label = "Optional Final Exam (%)",
        min = final_min,
        max = 100,
        value = final_min,
        step = 0.01,
        width = "100%",
        updateOn = "blur"
      )
    )
  })

  observeEvent(final_exam_minimum(), {
    final_min <- final_exam_minimum()

    final_exam_score_value(final_min)
    updateNumericInput(
      session,
      inputId = "final_exam",
      min = final_min,
      max = 100,
      value = final_min
    )
  }, ignoreInit = FALSE, priority = 100)

  observeEvent(input$final_exam, {
    final_min <- final_exam_minimum()
    current_value <- input$final_exam
    corrected_value <- bounded_value(current_value, default = final_min, min_value = final_min, max_value = 100)

    final_exam_score_value(corrected_value)

    if (is.null(current_value) || is.na(current_value) || !isTRUE(all.equal(current_value, corrected_value))) {
      updateNumericInput(
        session,
        inputId = "final_exam",
        min = final_min,
        max = 100,
        value = corrected_value
      )
    }
  }, ignoreInit = FALSE)

  final_exam_score <- reactive({
    bounded_value(final_exam_score_value(), default = final_exam_minimum(), min_value = final_exam_minimum(), max_value = 100)
  })

  no_final <- reactive({
      5 * grade_value(input$small_assignments) / 100 +
      5 * grade_value(input$reading_tutorials) / 100 +
      10 * grade_value(input$learning_checks) / 100 +
      20 * grade_value(input$homework) / 100 +
      30 * grade_value(input$exam_1) / 100 +
      30 * grade_value(input$exam_2) / 100 +
      course_bonus()
  })

  with_final <- reactive({
      5 * grade_value(input$small_assignments) / 100 +
      5 * grade_value(input$reading_tutorials) / 100 +
      10 * grade_value(input$learning_checks) / 100 +
      20 * grade_value(input$homework) / 100 +
      15 * grade_value(input$exam_1) / 100 +
      15 * grade_value(input$exam_2) / 100 +
      30 * final_exam_score() / 100 +
      course_bonus()
  })

  best_grade <- reactive({
    max(no_final(), with_final())
  })

  grade_difference <- reactive({
    with_final() - no_final()
  })

  output$course_bonus <- renderText({
    format_points(course_bonus())
  })

  output$grade_no_final <- renderText({
    format_grade(no_final())
  })

  output$grade_with_final <- renderText({
    format_grade(with_final())
  })

  output$grade_difference <- renderText({
    format_points(grade_difference())
  })

  output$grade <- renderText({
    format_grade(best_grade())
  })
}

shinyApp(ui, server)
