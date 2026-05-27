library(dplyr)
library(readxl)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)
library(openxlsx)
library(zip)

process_attendance <- function(roster_path, attendance_path, selected_session, max_weekly_points) {
  #roster1_raw <- read_excel('data/SAMPLE ROSTER.xlsx')
  #roster2_raw <- read_excel('data/Sample Roster SP26 (BISC; CHEM; ECON; PHYS).xlsx')
  roster2_raw <- read_excel(roster_path)
  #attendance_raw <- read.csv('data/event_attendee_logs (9).csv')
  attendance_raw <- read.csv(attendance_path)
  sessions_raw <- read.csv('data/sessions.csv')
  # session <- 'Spring 2026 (January 12, 2026 - May 08, 2026)'
  
  attendance <- attendance_raw %>%
    select(Student.Email, Student.Name,
           session_date = Start.At.Date,
           course = Courses,
           attended = Student.Attendance) %>%
    mutate(session_date = ymd(session_date),
           student_id = tolower(str_replace(Student.Email, '@.*', '')),
           course = str_sub(course, 1, 8),
           week_num = epiweek(session_date - days(1)),
           day_of_week = wday(session_date)) %>%
    select(-Student.Email)
  
  # ADD YEAR OR ELSE WEEKNUM is not unique
  
  attendance <- attendance %>%
    filter(attended == 'Present') %>%
    select(-attended)
  
  attendance_stats <- attendance %>%
    group_by(course, student_id, week_num) %>%
    summarize(n_sessions = n()) %>%
    ungroup()
  
  roster2 <- roster2_raw %>%
    filter(!is.na(`GW E-Mail Address`)) %>%
    select(student_email = `GW E-Mail Address`,
           student_firstname = `First Name`,
           student_lastname = `Last Name`,
           subject = `Subject Code`,
           course_number = `Course Number`,
           section_number = `Section Number`) %>%
    mutate(student_id = tolower(str_replace(student_email, '@.*', '')),
           course = paste0(subject, course_number)) %>%
    select(-subject, -course_number, -student_email) %>%
    filter(section_number %in% as.character(10:19))
  
  attendance_stats <- left_join(roster2, attendance_stats)
  
  sessions <- sessions_raw %>%
    mutate(start_date = ymd(start_date),
           end_date = ymd(end_date),
           start_week = epiweek(start_date- days(1)),
           end_week = epiweek(end_date - days(1)),
           session_label = paste0(session, ' ', year, ' (', 
                                  format(start_date, '%B %d, %Y'),
                                  ' - ',
                                  format(end_date, '%B %d, %Y'),
                                  ')'))
  
  session_weeks <- sessions %>%
    mutate(week_num = map2(start_week, end_week, seq)) %>%
    unnest(week_num)  
  
  attendance_stats <- attendance_stats %>%
    left_join(sessions %>% select(year, start_week, end_week, session_label),
              join_by(week_num >= start_week, week_num <= end_week)) %>%
    filter(session_label == selected_session) %>%
    mutate(session_week = week_num - start_week + 1) %>%
    select(student_lastname, student_firstname, student_id, course, section_number, session_label, session_week, n_sessions) %>%
    # using pmin in the next line, otherwise, the next line will always result in 1 since it's min of n_sessions over all rows
    mutate(weekly_attendance_points = pmin(n_sessions, as.numeric(max_weekly_points)))
  
  return(attendance_stats)
}

create_zip <- function(attendance_stats, zip_filename) {
  # Create a temp directory to hold the Excel file
  tmp_path <- tempfile()
  dir.create(tmp_path)
  
  # The file INSIDE the ZIP (relative)
  # xlsx_basename <- sub("\\.zip$", ".xlsx", basename(zip_filename))
  xlsx_basename <- paste0("attendance_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
  
  # The file ON DISK (absolute)
  xlsx_fullpath <- file.path(tmp_path, xlsx_basename)
  
  # Write the Excel file
  write.xlsx(attendance_stats, xlsx_fullpath, rowNames = FALSE)
  
  # TODO:  CREATE THE INDIVIDUAL COURSE REPORTS
  file_paths <- xlsx_basename
  
  for (course_i in unique(attendance_stats$course)) {
    course_df <- attendance_stats %>% filter(course == course_i)
    xlsx_i_basename <- xlsx_basename <- paste0("attendance_", course_i, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    xlsx_i_fullpath <- file.path(tmp_path, xlsx_i_basename)

    write.xlsx(course_df, xlsx_i_fullpath, rowNames = FALSE)
    
    file_paths <- c(file_paths, xlsx_i_basename)
  }
  
  # ZIP using the relative name, with tmp_path as the root
  zip::zip(
    zipfile = zip_filename,
    files = file_paths,   # relative path
    root = tmp_path          # base directory
  )
}
