library(tidyverse)

# load_student data
db.stud <- read_csv("G:\\My Drive\\_DATA\\csv\\inep\\prova_brasil\\prova_brasil_students_ses_2017.csv")

# prepare internet questions and number of students with only one dorm
db.stud <- db.stud %>% 
  mutate(mark = substr(answers, 13, 13),
         computer = mark %in% LETTERS[2:5] )  %>% 
  group_by(id_school) %>% 
  summarise(perc_computer = mean(computer, na.rm = T))

write_csv(db.stud, './data/stud_data_prova_brasil_2017.csv')  
  