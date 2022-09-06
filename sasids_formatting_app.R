# The following is a Shiny webapp that takes an SPSS input and creates a new file
# (with choice of file type) with the correct formatting for upload to SASIDs.

# Note that you should export to xlsx, not csv, because sex needs to have a leading zero

load_packages <- function() {
  # loading packages
  library(shiny)
  library(tidyverse)
  library(lubridate)
  library(haven)
  library(openxlsx)
  library(shinythemes)
  library(dplyr)
}

main_function <- function() {
  # define UI 
  ui <- fluidPage(
    titlePanel("SASIDs Formatting App"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file_to_upload", "Choose SAV file to be formatted: ", accept = ".sav"),
        h5(strong("Click button to format your data:")),
        actionButton("format_button", "Format"),
        div(style = "margin-bottom:20px"),
        radioButtons("filetype", "Choose file type for download:", choices = c(".xlsx", ".csv", ".txt")),
        downloadButton("downloadData", "Download")
      ),
      mainPanel(
        tableOutput("table")
      )
    )
  )
  
  # define server 
  server <- function(input, output) {
    
    raw_data <- eventReactive(input$file_to_upload, {
      read_sav(input$file_to_upload$datapath)
    })
    
    formatted_data <- eventReactive(input$format_button, {
      formatted_upload_file <- raw_data() %>%
        mutate(
          dob = format(make_date(month = bmonth, day = bday, year = byear), "%m%d%Y"),
          sex_code = case_when(
            as.vector(sex) == "1" ~ "02",
            as.vector(sex) == "2" ~ "01",
            as.vector(sex) == "" ~ ""
          ),
          name = str_replace(name, "- ", "-"),
          name = str_replace(name, " -", "-")) %>%
        separate(name, into = c("last_name", "first_middle"), ", ") %>%
        separate(first_middle, into = c("first_name", "middle_name"), sep = "\\s", extra = "merge") %>%
        mutate(
          # this removes all punctuation except for hyphens, converts diacritics to standard letters, and then
          # finally replaces hyphens with a space
          middle_name = replace_na(middle_name, "NMN"),
          first_name = str_replace(iconv(gsub("([-])[[:punct:]]", "", first_name), to = 'ASCII//TRANSLIT'), "-", " "),
          middle_name = str_replace(iconv(gsub("([-])[[:punct:]]", "", middle_name), to = 'ASCII//TRANSLIT'), "-", " "),
          last_name = str_replace(iconv(gsub("([-])[[:punct:]]", "", last_name), to = 'ASCII//TRANSLIT'), "-", " "),
        )
      
      trimmed <- select(formatted_upload_file, c('id',
                                                 'last_name',
                                                 'first_name',
                                                 'middle_name',
                                                 'dob',
                                                 'sex_code'))
      
      sasid = rep("          ", times = nrow(trimmed))
      sd_boces_code = rep("7030", times = nrow(trimmed))
      school_code = rep("0000", times = nrow(trimmed))
      suffix = rep("", times = nrow(trimmed))
      grade_lvl = rep(120, times = nrow(trimmed))
      ai_ind = rep("0", times = nrow(trimmed))
      
      header = c("SASID",
                 "School District/BOCES Code",
                 "School Code",
                 "Local ID (LASID)",
                 "Student's Last Name",
                 "Student's Suffix",
                 "Student's First Name",
                 "Student's Middle Name",
                 "Student's Date of Birth",
                 "Grade Level",
                 "Student's Gender",
                 "Active/Inactive Indicator")
      
      final_data <- cbind(sasid,
                          sd_boces_code,
                          school_code,
                          trimmed$id,
                          trimmed$last_name,
                          suffix,
                          trimmed$first_name,
                          trimmed$middle_name,
                          trimmed$dob,
                          grade_lvl,
                          trimmed$sex_code,
                          ai_ind)
      colnames(final_data) <- header
      
      final_data
      
    })
    
    output$table <- renderTable({
      formatted_data()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if(input$filetype == ".csv") {
          paste("sasids_upload_data_", format(Sys.Date(), "%m%d%y"), ".csv", sep = "")
        }
        else if(input$filetype == ".xlsx") {
          paste("sasids_upload_data_", format(Sys.Date(), "%m%d%y"), ".xlsx", sep = "")
        }
        else if(input$filetype == ".txt") {
          paste("sasids_upload_data_", format(Sys.Date(), "%m%d%y"), ".txt", sep = "")
        }
      },
      content = function(file) {
        if(input$filetype == ".csv") {
          write.csv(formatted_data(), file, row.names = F)
        }
        else if(input$filetype == ".xlsx") {
          my_workbook <- createWorkbook()
          addWorksheet(
            wb = my_workbook,
            sheetName = "sasids_data"
          )
          writeData(
            my_workbook,
            sheet = 1,
            formatted_data(),
            startRow = 1,
            startCol = 1
          )
          saveWorkbook(my_workbook, file)
        } 
        else if(input$filetype == ".txt") {
          write.table(formatted_data(), file, sep = "\t", row.names = F, quote = F)
        }
      }
    )
  }
  
  # run the application
  shinyApp(ui = ui, server = server)
}

load_packages()

main_function()
