library(readr)
library(dplyr)
library(shiny)
library(shinysurveys)
# Make sure to install from a PR
# remotes::install_github("jdtrat/shinysurveys#43")
# See SO: https://stackoverflow.com/questions/71512952/is-there-any-other-way-to-create-dependent-survey-questionnaire-using-r-shiny
library(googlesheets4)
library(bslib)
library(stringr)
library(blastula)

df <- read_csv("questions.csv")

source("email_logic.R")


# Extend types of inputs to checkbox
extendInputType(input_type = "checkbox", {
  shiny::checkboxGroupInput(
    inputId = surveyID(),
    label = surveyLabel(),
    choices = surveyOptions()
  ) 
})