source("www/packages.R")
source("www/functions.R")
source("www/styling.R")

# Set options for data tables:
options(DT.options = list(pageLength = 5, lengthMenu = c(5, 25, 50, 100,250)))

# Ask for backup path from user
# rstudioapi::showDialog("Backup Path", message = "The next step asks you to select
#                        a folder for backups. Consider using an external Device
#                        for this. A backup is created for every change and named
#                        by date and time.")
# backup_path <- rstudioapi::selectDirectory(caption = "Backup Folder")
# 
# while(is.null(backup_path)){
#   rstudioapi::showDialog("Backup Path", 
#                          message = "Seriously: select a backup folder!")
#   backup_path <- rstudioapi::selectDirectory(caption = "Backup Folder")
# }

backup_path <- getwd()

# Create log folder if not existent
dir.create("backup", showWarnings = F)

con <- dbPool(drv = RSQLite::SQLite(), dbname = "db/students_db") 

# Connect to database:
onStop(function() {
  poolClose(con)
})

################################################################################
###################################  UI  #######################################
################################################################################

ui <- dashboardPage(skin = "green",
                    
                    dashboardHeader(title = "Students ID Check", disable = FALSE),
                    
                    dashboardSidebar(disable = T),
                    
                    dashboardBody( # Main Panel
                      
                      # Refocus search bar after action
                      tags$head(includeScript("www/refocus_search.js")),
                      # Include Github corner
                      includeHTML("www/github.html"),
                      # Box with various tabs that show subsets of students dataframe
                      
                      fluidRow(
                      column(9, offset = 0,
                      tabBox(
                        width = NULL, title = "Overview",side = "right", selected = "Checked In",
                        tabPanel("Checked In", DT::dataTableOutput("studtable_accept")),
                        tabPanel("Open", DT::dataTableOutput("studtable_open")),
                        tabPanel("With Note", DT::dataTableOutput("studtable_note")),
                        tabPanel("Declined", DT::dataTableOutput("studtable_decline"))
                      )),
                      column(3, offset = 0,
                      # Students checked in box
                      valueBoxOutput("progressBox", width = NULL),
                      # Students with a note box
                      valueBoxOutput("progressBox2", width = NULL),
                      # Shift Options Box
                      box(
                        width = "NULL", collapsible = T, collapsed = T, title = "Shift options",background = "maroon",
                        radioGroupButtons(
                          inputId = "shiftnumber",
                          label = "Shift", 
                          choices = c("1", "2", "3", "4"),
                          status = "success",
                          width = "100%"
                        ),
                        tags$style(slider_maxnumshift),
                        sliderInput("maxnumshift", "Notify me at:", 25, 220, 65, 1, 
                                    width = "100%", post = " Students"),
                        actionButton("finish_shift", # Row for accept decline buttons
                                     "Close Shift",
                                     style = button_finish_shift)
                      ),
                      # Take Note or Decline Box
                      box(width = "NULL", collapsible = T, collapsed = T, title = "Take Note or Decline", background = "red",
                          searchInput(
                            inputId = "note", 
                            label = NULL, 
                            value = NULL,
                            placeholder = "Press Enter to save.", 
                            btnSearch = icon("save"), 
                            btnReset = icon("remove"), 
                            width = "100%"),
                          actionButton("decline", # Row for accept decline buttons
                                       "",
                                       style = button_decline,
                                       icon = icon("user-times"))))),
                      fluidRow(
                      # Box with search result: 
                      box(#title = "Search Result:",
                          collapsible = FALSE, width = NULL,
                          h2(htmlOutput("results"), align = "center")
                      )),
                      fluidRow(align = "center",
                      column(4, offset = 0,
                             searchInput(
                               inputId = "search",
                               label = h3("Search by Name or Number"), 
                               placeholder = "Press Enter to search.", 
                               btnSearch = icon("search"), 
                               btnReset = icon("remove"), 
                               width = "84%")),
                      column(4,
                             actionButton("accept", "Accept",
                                          style = button_accept,
                                          icon = icon("user-check"))),
                      column(4,
                      # Pre defined actions:
                      # Accept without ID Card
                      actionButton("accept_wo_id", "Accept without ID",
                                   style = button_accept_wo_id,
                                   icon = icon("exclamation-triangle")))),
                      #fluidRow(align = "center", style = "padding-top:20px"),
                      # Info box for sum of accepted students
                      fluidRow(align = "center",
                               column(10, offset = 1)),
                      
                      # Info box for sum of students with a note
                      fluidRow(align = "center",style = "position:fixed, bottom:0",
                               column(10, offset = 1)),
                      
                      # Shift options
                      fluidRow(align = "center", style = "position:fixed, bottom:0",
                               column(10, offset = 1))#,
                      # Backup path
                      # fluidRow(style = "padding-bottom:20px",
                      #         htmlOutput("backup")),
                      # Include footer
                      # includeCSS("www/footer.css"), includeHTML("www/footer.html")
                      # ,
                      # fluidRow(
                      #   column(
                      #     width = 8,
                      #     shinyviewr_UI("my_camera", height = '400px')
                      #   ),
                      #   column(
                      #     width = 3,
                      #     offset = 1,
                      #     h2("Taken Photo"),
                      #     imageOutput("snapshot")
                      #   )
                      # )
                    )
)

################################################################################
#################################  Server  #####################################
################################################################################

server <- function(input, output, session) {
  
  #server side call of the drawr module
  # camera_snapshot <- callModule(
  #   shinyviewr,
  #   'my_camera',
  #   output_width = 200,
  #   output_height = 200
  # )
  
  # output$snapshot <- renderPlot({
  #   req(camera_snapshot())
  #   par(mar = c(0,0,0,0))
  #   plot(camera_snapshot(), main = 'My Photo!')
  #   snap <- camera_snapshot()
  #   png(file="tmp",
  #       width=1000, height=1000)
  #   par(mar = c(0,0,0,0))
  #   plot(camera_snapshot(), main = 'My Photo!')
  #   dev.off()
  #   numbers <- tesseract(options = list(tessedit_char_whitelist = "0123456789"))
  #   updateSearchInput(session, "search", value = ocr("tmp", engine = numbers), trigger = TRUE)
  # })
  
  students  <- function(){}
  stats     <- function(){}
  shiftnum     <- function(){}
  
  students <- reactivePoll(intervalMillis = 500, session = session, 
                           checkFunc = function() {
                             if(all_equal(as.data.frame(students()),dbReadTable(con, "students")) %>% isTRUE()){0}else{1}},
                           valueFunc = function(){dbReadTable(con, "students")})
  
  stats <- reactivePoll(intervalMillis = 500, session = session, 
                           checkFunc = function() {
                             if(all_equal(as.data.frame(stats()),dbReadTable(con, "stats")) %>% isTRUE()){0}else{1}},
                           valueFunc = function(){dbReadTable(con, "stats")})
  
  shiftnum <- reactivePoll(intervalMillis = 1000, session = session, 
                        checkFunc = function() {
                          if(all_equal(as.data.frame(shiftnum()),dbReadTable(con, "shift")) %>% isTRUE()){0}else{1}},
                        valueFunc = function(){dbReadTable(con, "shift")})
  
  # Render the search results:
  
  output$results <- renderUI({
    forename <- students() %>% dplyr::filter(
      str_detect(input$search, as.character(students()$matrnumber)) |
        name == input$search) %>% dplyr::select(forename) %>% unlist()
    name <- students() %>% dplyr::filter(
      str_detect(input$search, as.character(students()$matrnumber)) |
        name == input$search) %>% dplyr::select(name) %>% unlist()
    shift <- students() %>% dplyr::filter(
      str_detect(input$search, as.character(students()$matrnumber)) |
        name == input$search) %>% dplyr::select(shift) %>% unlist()
    overbooked <- students() %>% dplyr::filter(
      str_detect(input$search, as.character(students()$matrnumber)) |
        name == input$search) %>% dplyr::select(overbooked) %>% unlist()
    if(length(name) ==1){
      if(overbooked == TRUE){
        HTML(paste(forename, name, "<br/>", "Shift:", shift, "<br/>", '<strong style="color: red;">Overbooked</strong>' ))
      } else {
        HTML(paste(forename, name, "<br/>", "Shift:", shift ))  
      }} else {
        if(length(name) > 1) {HTML("Selection not unambiguous!")
          } else {HTML("No student selected.")}
        }
  })
  
  # Render sum of accepted students
  
  output$progressBox <- renderValueBox({
    valueBox(paste0(sum(students() %>% dplyr::filter(accepted == TRUE) %>%
                          dplyr::count() - sum(stats()[,"sumstudents"]))), "students checked in.",
             icon = icon("user-check"), color = "green")})
  
  # Render sum of students with note
  
  output$progressBox2 <- renderValueBox({valueBox(paste0(sum(students() %>%
                                                               dplyr::filter(!is.na(note)) %>%
                                                               dplyr::count())), "students with note.",
                                                  icon = icon("user-edit"), color = "yellow")})
  
  # Handle the current shift
  
  observeEvent(input$shiftnumber, {
    con %>% dbExecute(paste("UPDATE shift ",
                            "SET shift = '", input$shiftnumber,"'", sep = "" ,collapse = ""))
    })
  
  observeEvent(shiftnum(), {
    updateRadioGroupButtons(session, inputId = "shiftnumber",
                            label = "Shift",
                            choices = c("1", "2", "3", "4"),
                            status = "success",
                            selected = shiftnum()$shift)
  })
  
  # Render the backup path
  
  output$backup <- renderUI({
    HTML(paste("Saving Backup to:<br/>", backup_path, "/", sep= ""))})
  
  # Render all data tables
  
  output$studtable_accept <- 
    DT::renderDataTable(students() %>%
                          dplyr::filter(accepted == TRUE) %>%
                          dplyr::arrange(desc(modified)) %>%
                          dplyr::select(-modified), rownames = FALSE,
                        options = list(columnDefs = list(list(
                          className = 'dt-center', 
                          targets = 0:6))))
  output$studtable_decline <- 
    DT::renderDataTable(students() %>%
                          dplyr::filter(accepted == FALSE) %>%
                          dplyr::arrange(desc(modified)) %>%
                          dplyr::select(-modified), rownames = FALSE,
                        options = list(columnDefs = list(list(
                          className = 'dt-center', 
                          targets = 0:6))))
  output$studtable_open <- 
    DT::renderDataTable(students() %>% 
                          dplyr::filter(is.na(accepted)) %>% 
                          dplyr::arrange(desc(modified), name) %>%
                          dplyr::select(-modified), rownames = FALSE,
                        options = list(
                          columnDefs = list(list(
                            className = 'dt-center', 
                            targets = 0:6))))
  output$studtable_note <- 
    DT::renderDataTable(students() %>%
                          dplyr::filter(!is.na(note)) %>%
                          arrange(desc(modified)) %>%
                          dplyr::select(-modified),rownames = FALSE,
                        options = list(
                          columnDefs = list(list(
                            className = 'dt-center',
                            targets = 0:6))))
  
  # Accept Event
  
  observeEvent({
    input$accept
    }, {
    
    sid_a <- which(str_detect(input$search,
                              as.character(students()$matrnumber)) |
                     students()$name == input$search)
    
    # Check if (only) one student is selected
    if(length(sid_a) == 1){
      
      # Check if student belongs to the current shift
      
      if(students() %>% select(shift) %>% dplyr::slice(sid_a) %>% unlist() == shiftnum()$shift){
        
        # Check if student is already accepted in database
        if(dbReadTable(con, "students")[sid_a, "accepted"] == FALSE | is.na(dbReadTable(con, "students")[sid_a, "accepted"])){
          
          # Check if limit is reached
          if((sum(dbReadTable(con, "students") %>% dplyr::filter(accepted == TRUE) %>%
                  nrow() - sum(stats()[,"sumstudents"]))) >= (input$maxnumshift-1)){
            confirmSweetAlert(
              session = session,
              inputId = "limit_confirm",
              type = "warning",
              title = "Limit Reached!",
              text = "You've reached your specified limit. Do you still want to accept this student?",
              danger_mode = TRUE)
          } else {
            # Accept the student, write log and write modification time
            con %>% dbExecute(paste("UPDATE students ",
                                    "SET accepted = '1', log = '", paste(na.omit(c(students()[sid_a, "log"],as.character(Sys.time()), "[A]")), collapse = " "),"', modified = '", Sys.time(), "' ",
                                    "WHERE matrnumber = ",students()[sid_a, "matrnumber"], sep = ""))
            
            # Save a log, backup data, reset- and refocus search field
            log_backup_reset(sid = sid_a, 
                             event = "[A]", 
                             data = dbReadTable(con, "students"),
                             stats = dbReadTable(con, "stats"),
                             session = session,
                             backup_path = backup_path)
            
            # Check if student is overbooked:
            if(students()[sid_a, "overbooked"] == TRUE){
              sendSweetAlert(session, title = "Overbooked",
                             text = "This student is part of the overbooking. Seperate him/her from the crowd.")
            }
          }
        } else {
          sendSweetAlert(session, title = "Already Accepted",
                         text = "This student is already checked in! Consider taking a note!")
        }
      } else {
        sendSweetAlert(session, title = "Shift",
                       text = paste("The current selected shift is shift",
                                    input$shiftnumber, "but this student belongs to shift",
                                    students() %>% select(shift) %>% 
                                      dplyr::slice(sid_a) %>% unlist()))
      }
    } else {
      sendSweetAlert(session, title = "Selection not unambiguous",
                     text = "Please select one student.")
    }
  })

  #############################  accept ithout ID #################
  
  observeEvent({
    input$accept_wo_id
  }, {
    
    sid_a <- which(str_detect(input$search,
                              as.character(students()$matrnumber)) |
                     students()$name == input$search)
    
    # Check if (only) one student is selected
    if(length(sid_a) == 1){
      
      # Check if student belongs to the current shift
      
      if(students() %>% select(shift) %>% dplyr::slice(sid_a) %>% unlist() == shiftnum()$shift){
        
        # Check if student is already accepted in database
        if(dbReadTable(con, "students")[sid_a, "accepted"] == FALSE | is.na(dbReadTable(con, "students")[sid_a, "accepted"])){
          
          # Check if limit is reached
          if((sum(dbReadTable(con, "students") %>% dplyr::filter(accepted == TRUE) %>%
                  nrow() - sum(stats()[,"sumstudents"]))) >= (input$maxnumshift-1)){
            confirmSweetAlert(
              session = session,
              inputId = "limit_confirm",
              type = "warning",
              title = "Limit Reached!",
              text = "You've reached your specified limit. Do you still want to accept this student?",
              danger_mode = TRUE)
          } else {
            # Accept the student, write log and write modification time
            con %>% dbExecute(paste("UPDATE students ",
                                    "SET accepted = '1', log = '", paste(na.omit(c(students()[sid_a, "log"],as.character(Sys.time()), "[A]")), collapse = " "),"', modified = '", Sys.time(), "' ",
                                    "WHERE matrnumber = ",students()[sid_a, "matrnumber"], sep = ""))
            
            # Save a log, backup data, reset- and refocus search field
            log_backup_reset(sid = sid_a, 
                             event = "[A]", 
                             data = dbReadTable(con, "students"),
                             stats = dbReadTable(con, "stats"),
                             session = session,
                             backup_path = backup_path)
            
            # Check if student is overbooked:
            if(students()[sid_a, "overbooked"] == TRUE){
              sendSweetAlert(session, title = "Overbooked",
                             text = "This student is part of the overbooking. Seperate him/her from the crowd.")
            }
          }
        } else {
          sendSweetAlert(session, title = "Already Accepted",
                         text = "This student is already checked in! Consider taking a note!")
        }
      } else {
        sendSweetAlert(session, title = "Shift",
                       text = paste("The current selected shift is shift",
                                    input$shiftnumber, "but this student belongs to shift",
                                    students() %>% select(shift) %>% 
                                      dplyr::slice(sid_a) %>% unlist()))
      }
    } else {
      sendSweetAlert(session, title = "Selection not unambiguous",
                     text = "Please select one student.")
    }
  })
  
  
  
  
    
  # Accept if Limit is reached but user has confirmed the checkin
  observeEvent(input$limit_confirm, {
    if(isTRUE(input$limit_confirm)) {
      
      sid_a <- which(str_detect(input$search,
                                as.character(students()$matrnumber)) |
                       students()$name == input$search)
      
      con %>% dbExecute(paste("UPDATE students ",
                              "SET accepted = '1', log = '", paste(na.omit(c(students()[sid_a, "log"],as.character(Sys.time()), "[A]")), collapse = " "),"', modified = '", Sys.time(), "' ",
                              "WHERE matrnumber = ",students()[sid_a, "matrnumber"], sep = ""))
      
      # Save a log, backup data, reset- and refocus search field
      log_backup_reset(sid = sid_a, 
                       event = "[A]", 
                       data = dbReadTable(con, "students"),
                       stats = dbReadTable(con, "stats"),
                       session = session,
                       backup_path = backup_path)
      
      if(students()[sid_a, "overbooked"] == TRUE){
        sendSweetAlert(session, title = "Overbooked",
                       text = "This student is part of the overbooking. Seperate him/her from the crowd.")
      }
    }})
  
  # Add note if accepted without ID
  
  observeEvent(input$accept_wo_id, {
    
    sid_n <- which(str_detect(input$search,
                              as.character(students()$matrnumber)) |
                     students()$name == input$search)
    
    # Take Note if single student is selected else notify
    if(length(sid_n) == 1){
      
    #  if(dbReadTable(con, "students")[sid_n, "accepted"] == FALSE | is.na(dbReadTable(con, "students")[sid_n, "accepted"])){
        con %>% dbExecute(paste("UPDATE students ",
                                "SET note = '", paste(na.omit(c(students()[sid_n, "note"], "Accepted without ID")), collapse = " "),"', log = '", paste(na.omit(c(students()[sid_n, "log"],as.character(Sys.time()), "[N]")), collapse = " "),"', modified = '", Sys.time(), "' ",
                                "WHERE matrnumber = ",students()[sid_n, "matrnumber"], sep = ""))
        
        # Save a log, backup data, reset- and refocus search field
        log_backup_reset(sid = sid_n, 
                         event = "[N]", 
                         data = dbReadTable(con, "students"),
                         note = input$note,
                         stats = dbReadTable(con, "stats"),
                         session = session,
                         backup_path = backup_path)
        # Clear Note field
        updateSearchInput(session, "note", value = "", trigger = FALSE)
      #  }
    } else {
      if(input$note != ""){
        sendSweetAlert(session, title = "Selection",
                       text = "Please select one student.")
      }
    }
  })
  
  # Decline Event
  # Ask user to confirm or cancel
  observeEvent(input$decline, {
    
    sid_d <- which(str_detect(input$search,
                              as.character(students()$matrnumber)) |
                     students()$name == input$search)
    
    if(length(sid_d) == 1){
      if(is.na(students()[sid_d, "accepted"]) | students()[sid_d, "accepted"] == TRUE){
        confirmSweetAlert(
          session = session,
          inputId = "decline_confirm",
          type = "warning",
          title = "Want to check out?",
          text = "Do you really want to check out? This should rarely be the case.",
          danger_mode = TRUE)
      } else {
        sendSweetAlert(session, title = "Can't Check Out!",
                       text = "Can't check out! This student is still marked as checked out. Consider taking a note!")
      }}})
  
  
  observeEvent(input$decline_confirm, {
    if(isTRUE(input$decline_confirm)) {
      
      sid_d <- which(str_detect(input$search,
                                as.character(students()$matrnumber)) |
                       students()$name == input$search)
      
      con %>% dbExecute(paste("UPDATE students ",
                              "SET accepted = '0', log = '", paste(na.omit(c(students()[sid_d, "log"],as.character(Sys.time()), "[D]")), collapse = " "),"', modified = '", Sys.time(), "' ",
                              "WHERE matrnumber = ",students()[sid_d, "matrnumber"], sep = ""))
      
      # Save a log, backup data, reset- and refocus search field
      log_backup_reset(sid = sid_d, 
                       event = "[D]", 
                       data = dbReadTable(con, "students"),
                       stats = dbReadTable(con, "stats"),
                       session = session,
                       backup_path = backup_path)
    }})
  
  # Note event
  observeEvent(c(input$note, input$note_search), {
    
    sid_n <- which(str_detect(input$search,
                              as.character(students()$matrnumber)) |
                     students()$name == input$search)
    
    # Take Note if single student is selected else notify
    if(length(sid_n) == 1){
      
      con %>% dbExecute(paste("UPDATE students ",
                              "SET note = '", paste(na.omit(c(students()[sid_n, "note"], input$note)), collapse = " "),"', log = '", paste(na.omit(c(students()[sid_n, "log"],as.character(Sys.time()), "[N]")), collapse = " "),"', modified = '", Sys.time(), "' ",
                              "WHERE matrnumber = ",students()[sid_n, "matrnumber"], sep = ""))
      
      # Save a log, backup data, reset- and refocus search field
      log_backup_reset(sid = sid_n, 
                       event = "[N]", 
                       data = dbReadTable(con, "students"),
                       note = input$note,
                       stats = dbReadTable(con, "stats"),
                       session = session,
                       backup_path = backup_path)
      # Clear Note field
      updateSearchInput(session, "note", value = "", trigger = FALSE)
    } else {
      if(input$note != ""){
        sendSweetAlert(session, title = "Selection",
                       text = "Please select one student.")
      }
    }
  })
  
  # Close shift event:
  
  observeEvent(input$finish_shift, {
    confirmSweetAlert(
      session = session,
      inputId = "finish_confirm",
      type = "warning",
      title = "Want to finish this shift?",
      text = "This action will reset the sum of students. It can't be reverted.",
      danger_mode = TRUE)
  })
  
  observeEvent(input$finish_confirm, {
    
    con %>% dbExecute(paste("UPDATE stats ",
                            "SET sumstudents = ", (sum(students() %>% dplyr::filter(accepted == TRUE) %>% dplyr::count()) - sum(dbReadTable(con, "stats")[,"sumstudents"])),
                            " WHERE shift = ", min(which(dbReadTable(con, "stats")[,"sumstudents"] == 0)) , sep = ""))
    
  })
}

# options(shiny.host = '192.168.0.2')
# options(shiny.port = 8888)
shinyApp(ui, server)
