## code to prepare `DATASET` dataset goes here
students <- read.csv("C:/Users/LASS/Desktop/students.csv")

usethis::use_data(students, overwrite = TRUE)
