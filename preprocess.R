
library(readxl)
library(dplyr)
library(tibble)
library(RSQLite)
library(pool)


# Data input
oek_b_nor <- read_excel(file.choose(),skip = 4)
oek_b_math <-read_excel(file.choose(),skip = 4)

dataset <- bind_rows(oek_b_nor, oek_b_math)

students <- dataset %>% as_tibble() %>% 
  mutate_at(vars("Matrikelnummer"), .funs = as.numeric) %>%
  dplyr::select("Vorname", "Nachname", "Matrikelnummer") %>%
  rename("name" = "Nachname", "forename" = "Vorname", "matrnumber" = "Matrikelnummer") %>%
  add_column("accepted" = NA, "note" = NA, "log" = NA, "modified" = NA, "shift" = NA, "overbooked" = NA) %>%
  filter_all(any_vars(!is.na(.)))


################ Slebsteingabe
#Parameters

people <- nrow(students) #Anzahl der Leute
room_size <- 170 # Raumgröße
max_shifts <- 2  # Maximale Anzahl der Schichten
overload <- 0 #Prozent der Überbüchung 



# creating shift variable 
shifts <-  rep(1 , (room_size + room_size * overload))
 
for (i in 2:max_shifts) {
  
  shifts <- c(shifts,  rep((i) , (room_size + room_size * overload)))
  
}

ifelse(length(shifts) < people , 'NOT enought seats, you need a bigger room or more shifts', paste( 'you have enough seats and you need ',
                                                             shifts[people], ' shift(s)' , sep = '' ))

students$shift <-  shifts[1:people]

# creating  overbooked  variable 

overbook <-  rep(c(rep(0 , room_size) , rep(1 , room_size * overload) ), shifts[people])   

students$overbooked <- overbook[1:people]

# create summary table:

stats <- data.frame(shift = c(1,2,3,4), 
                    sumstudents = c(0,0,0,0))
shift <- data.frame(shift = c(1))
dir.create("db", showWarnings = F)
con <- dbPool(drv = RSQLite::SQLite(), dbname = "db/students_db")
dbWriteTable(con, "students", students)
dbWriteTable(con, "stats", stats, overwrite = T)
dbWriteTable(con, "shift", shift, overwrite = T)

# Use the example:
# 
# load(file = "students_example.Rda")
# stats <- data.frame(shift = c(1,2,3,4), sumstudents = c(0,0,0,0))
# dir.create("db", showWarnings = F)
# con <- dbPool(drv = RSQLite::SQLite(), dbname = "db/students_db")
# dbWriteTable(con, "students", students)
# dbWriteTable(con, "stats", stats)
