function(input, output, session) {
  
  renderSurvey()
  
  observeEvent(input$submit, {
    
    gs4_auth(
      cache = gargle::gargle_oauth_cache(),
      email = gargle::gargle_oauth_email()
    )
    
    # Retrieve existing datasheet
    sheet_url <- "https://docs.google.com/spreadsheets/d/1F7DB2WZDnbTZ48SZxyZDKohc_U1ZimIiEMO_5wX9Qcw/edit#gid=0"
    previous <- read_sheet(sheet_url)
    
    # Obtain and and append submitted results
    response <- getSurveyData(custom_id = input$email,
                              include_dependencies = FALSE)
    updated <- bind_rows(previous, response)
    
    # Write back to Google sheet
    write_sheet(updated, ss = sheet_url, sheet = 'Sheet1')
    
    # Show submission message
    showModal(modalDialog(
      title = "Thank you for your interest in PSInet.",
      "Please reach out to Kim Novick (knovick@indiana.edu) or Jessica Guo (jessicaguo@arizona.edu) with any questions. "
    ))
  })
  
}