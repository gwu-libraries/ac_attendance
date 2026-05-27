# Academic Commons review session attendance reporting app

<img width="592" height="373" alt="ac_attendance" src="https://github.com/user-attachments/assets/273a1514-6a57-43ed-ac0a-020991e3dabf" />

This app takes as inputs:

-   An Excel workbook containing student rosters for a set of courses and sections.
-   A CSV-formatted Penji export containing information on student visits to Academic Commons for review sessions, for specific courses.
-   The semester to report on
-   The maximum number of review sessions that can be credited per week:  1 or 2

The app returns a zip file containing multiple Excel documents:  

- An Excel workboook containing the students in the roster, which weeks they attended review sessions for their courses, and how many sessions they attended each week.
- [TODO] Filtered versions of the above, one for each course.

Weeks are Monday through Sunday.

The app is run as an R Shiny app.

## Installation

```         
git clone https://github.com/gwu-libraries/ac_attendance.git
```

In RStudio, run `app.R` as a Shiny App using the Run App <img width="65" height="18" alt="clipboard-70856000" src="https://github.com/user-attachments/assets/b389b452-4b97-45c2-8267-96b23a809c2e" />
 button. Select the relevant files, the correct sheet within the faculty feedback Excel file, and the start date. Press "Generate Notifications" and download the result file.
