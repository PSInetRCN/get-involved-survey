
fluidPage(
  theme = bs_theme(version = 3),
  surveyOutput(df = df,
               survey_title = "Register",
               survey_description = h4(HTML("Thank you for your interest in joining to PSInet. Please complete the following form, and we look forward to meeting you at a future PSInet event!")),
               theme = "#348DBC")
)