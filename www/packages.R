if (!require("pacman")) install.packages("pacman")
pacman::p_load(shinydashboard, 
               shiny, 
               DT,
               dplyr,
               stringr,
               shinyWidgets,
               RSQLite,
               DBI,
               pool,
               shinysense,
               png,
               tesseract,
               shinysense)
