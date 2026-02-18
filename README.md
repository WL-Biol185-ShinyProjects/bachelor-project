# bachelor-project

# Will you accept this rose?

View(githubbachelorette)
library(shiny)

contestants <- read_csv("contestants.csv")
seasons <- read_csv("seasons.csv")

library(tidyverse)
seasons <- seasons %>% select(-`Timeslot (ET)`)
seasons <- seasons %>% select(-`#`)
seasons <- seasons %>% select(-`TV Season`)
seasons <- seasons %>% select(-`Original run`)
contestants <- contestants %>% select(-`Season`)

install.packages("showtext")
library(showtext)
font_add_google("Lobster")

View(contestants)
View(seasons)

