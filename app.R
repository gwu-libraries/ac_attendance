library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)
library(openxlsx)

# Load session choices from sessions.csv
sessions_raw <- read.csv("data/sessions.csv")
sessions <- sessions_raw |>
  mutate(
    start_date    = ymd(start_date),
    end_date      = ymd(end_date),
    session_label = paste0(session, " ", year, " (",
                           format(start_date, "%B %d, %Y"),
                           " - ",
                           format(end_date, "%B %d, %Y"),
                           ")"),
    session_code = paste0(year, ", ", session)
  )
# make a vector of labeled values
session_choices <- setNames(sessions$session_code, sessions$session_label)

# Choose the last session that has already started
# So, if we are between sessions, this will select the previous session
current_session <- sessions %>%
  filter(start_date <= today("America/New_York")) %>%
  arrange(start_date) %>%
  tail(1) %>%
  pull(session_code)

source('helper_functions.R')

ui <- page_sidebar(
  title = "GW Academic Commons Review Session Attendance Processor",
  theme = bs_theme(version = 5, preset = "shiny"),
  
  # tags$style(HTML("
  #   .bslib-page-sidebar .main {
  #     max-width: 800px;
  #     margin-left: auto;
  #     margin-right: auto;
  #   }
  # ")),

  tags$style(HTML("
  /* keep main content narrow */

  .bslib-page-sidebar .main {
    max-width: 800px;
    margin-left: auto;
    margin-right: auto;
  }
  
  /* shrink everything */
  body, .bslib-page-sidebar, .card, .form-control, .btn, label, input, select {
    font-size: 0.85rem !important;
  }

  .btn {
    padding: 0.25rem 0.5rem !important;
  }

  .form-control, .form-select {
    padding: 0.25rem 0.4rem !important;
    height: auto !important;
  }

  .narrow-card {
    max-width: 450px;
  }
  
  .card-header {
    padding: 0.4rem 0.75rem !important;
  }

  .card-body {
    padding: 0.75rem !important;
  }

  .sidebar {
    font-size: 0.85rem !important;
  }
")),
             
  sidebar = sidebar(
    width = 320,

    fileInput(
      "roster_file",
      "Roster (Excel .xlsx)",
      accept = c(".xlsx", ".xls"),
      placeholder = "No file selected"
    ),

    fileInput(
      "attendance_file",
      "Penji Attendance Log (CSV)",
      accept = ".csv",
      placeholder = "No file selected"
    ),

    selectInput(
      "session",
      "Session",
      choices  = session_choices,
      selected = current_session
    ),
    
    selectInput(
      "max_weekly_points",
      "Maximum Weekly Points",
      choices = c(1, 2),
      selected = NULL
    ),

    input_task_button(
      "process_btn",
      "Process",
      icon = bsicons::bs_icon("play-fill")
    )
  ),

  card(
    class = "narrow-card",
    card_header("Status"),
    uiOutput("status_ui")
  ),
  
  card(
    class = "narrow-card",
    card_header("Download"),
    uiOutput("download_ui")
  )

)


server <- function(input, output, session) {

  # Reactive to hold processed result
  result <- reactiveVal(NULL)

  observeEvent(input$process_btn, {
    # Validate inputs
    req(input$roster_file, input$attendance_file, input$session, input$max_weekly_points)

    result(NULL)  # reset

    tryCatch({
      df <- process_attendance(
        roster_path      = input$roster_file$datapath,
        attendance_path  = input$attendance_file$datapath,
        selected_session = input$session,
        max_weekly_points = input$max_weekly_points
      )
      result(df)
      
    }, error = function(e) {
      
      tb <- traceback(2, output = FALSE)
      
      result(list(
        error     = conditionMessage(e),
        call      = deparse(conditionCall(e)),
        traceback = tb
      ))
    })
  })

  output$status_ui <- renderUI({
    r <- result()

    if (is.null(r)) {
      p("Upload files, choose a session, and click Process.", class = "text-muted")

    } else if (is.list(r) && !is.data.frame(r) && !is.null(r$error)) {
      div(
        class = "alert alert-danger",
        bsicons::bs_icon("exclamation-triangle-fill"), " ",
        strong("Error:"), r$error
      )

    } else {
      div(
        class = "alert alert-success",
        bsicons::bs_icon("check-circle-fill"), " ",
        strong("Done."),
        sprintf(" %d row(s) ready to download.", nrow(r))
      )
    }
  })

  output$download_ui <- renderUI({
    r <- result()
    if (is.null(r) || (is.list(r) && !is.data.frame(r))) return(NULL)

    downloadButton("download_excel", "Download Zip",
                   icon = shiny::icon("download"),
                   class = "btn-primary")
  })

  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("attendance_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"), ".zip")
    },
    content = function(file) {
      # TODO: result() should return a list of files.
      # Need to loop through and write individual files (with respective names) as a zip,
      # then write the zip file, then clean up.
      create_zip(result(), file)
      #write.xlsx(result(), file, row.names = FALSE)#, na.string = '')
    }
  )
}

shinyApp(ui, server)
