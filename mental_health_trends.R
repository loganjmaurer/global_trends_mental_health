library(tidyverse)
trends <- read.csv("Mental health Depression disorder Data.csv")
summary(trends)
trends <- na.omit(trends)
trends <- trends %>%
  dplyr::select(Entity, Year, Schizophrenia...., Bipolar.disorder...., Eating.disorders...., Anxiety.disorders...., Drug.use.disorders...., Depression...., Alcohol.use.disorders....) %>%
  rename(sch = Schizophrenia....,
         bipol = Bipolar.disorder....,
         eat = Eating.disorders....,
         anx = Anxiety.disorders....,
         drug = Drug.use.disorders....,
         dep = Depression....,
         alc = Alcohol.use.disorders....)
