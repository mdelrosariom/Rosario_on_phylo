library("dplyr")
first_column <- c("value_1", "value_2", "value_3")
second_column <- c("value_A", "value_B", "value_C")
third_column <- c("aa", "bb", "cc")

df <- data.frame(first_column, second_column, third_column)



select(filter(df, first_column == "value_1"), second_column, third_column)
