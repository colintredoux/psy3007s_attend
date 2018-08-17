# Initialise
library(pacman)
p_load(tidyverse, readxl, janitor, lubridate, magrittr)

# Def function
is.letter <- function(x) grepl("[[:alpha:]]", x)

# Read data
access_data <- read_excel("156-L1-08 Snape LT1 LT Safe.xlsx", skip = 5) %>% 
                  clean_names()
stud_mark_data <- read_csv("gradebook-b6bae547-9d1c-4b4e-ab1c-a6f845f74527-2018%2F08%2F17.csv") 
stud_mark_data %<>%  
  mutate(`Lecture attendance [1]` = as.numeric(`Lecture attendance [1]`)) %>% 
  replace_na(`Lecture attendance [1]` = 0) 
  
outfile <- paste("psy3007s_attendance_", today(),".csv", 
                 sep="")
access_data %>% 
  filter(wday(date) %in% c(1:5)) %>% 
  filter(hour(hms(time)) %in% c(9:12)) %>% 
  select(first_name, last_name, cn) %>% 
  mutate(cn = tolower(cn)) %>% 
  filter(is.letter(cn)) %>% 
  group_by(cn) %>% 
  count() %>% 
  rename(`Student ID` = cn) -> attendance

stud_mark_data %<>% left_join(attendance) %>% 
  select(-`Lecture attendance [1]`) %>% 
  rename(`Lecture attendance [1]` = n) 

stud_mark_data[is.na(stud_mark_data)] <- ""

write_csv(attendance, outfile)
write_csv(stud_mark_data, "gradebook-b6bae547-9d1c-4b4e-ab1c-a6f845f74527-2018%2F08%2F17.csv")



         