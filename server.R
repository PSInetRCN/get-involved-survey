function(input, output, session) {
  renderSurvey()
  
  observeEvent(input$submit, {
    gs4_auth(
      cache = gargle::gargle_oauth_cache(),
      email = gargle::gargle_oauth_email()
    )
    
    # Retrieve existing datasheet
    # Change this URL to the real one before merging upstream.
    sheet_url <-
      "https://docs.google.com/spreadsheets/d/1X697V-KqTA0v26dyFbFxx8mPeKTVsBAyYJO0MkDKbkA/edit#gid=0"
    previous <- read_sheet(sheet_url)
    
    
    # Obtain and and append submitted results
    response <- getSurveyData(custom_id = input$email,
                              include_dependencies = FALSE)
    
    timestamp <- data.frame(
      subject_id = input$email,
      question_id = "timestamp",
      question_type = "time",
      response = as.character(Sys.time())
    )
    
    response <- bind_rows(response, timestamp)
    

    updated <- bind_rows(previous, response)
    
    # Write back to Google sheet
    write_sheet(updated, ss = sheet_url, sheet = 'Sheet1')
    
    # Show submission message
    showModal(
      modalDialog(
        title = "Your answers have been recorded. Thank you!",
        "Please reach out to Kim Novick (knovick@indiana.edu) or Jessica Guo (jessicaguo@arizona.edu) with any questions. "
      )
    )
    
    
    # Email
    
    library(blastula)
    body_text0 <-
      paste0("Hi ", input$name_first, " ", input$name_last, ",")
    
    body_text1 <- paste("Thank you for filling out our survey!")
    
    
    body_text2 <- ""
    if (grepl("Slack", input$platform)) {
      body_text2 <-
        "You can join our Slack channel by following this invite... "
    }
    
    body_text2p5 <- ""
    
    if (grepl("listserv", input$platform)) {
      body_text2p5 <-
        "You will shortly receive an email invitation to join our listserv."
    }
    
    body_text3 <- paste("We look forward to working with you!")
    
    body_text4 <- "Sincerely,"
    
    body_text5 <- "The PSInet team"
    
    survey_message <- compose_email(
      body = blocks(
        body_text0,
        block_spacer(),
        body_text1,
        block_spacer(),
        body_text2,
        body_text2p5,
        block_spacer(),
        body_text3,
        block_spacer(),
        body_text4,
        block_spacer(),
        body_text5
      )
    )
    
    # find a way to validate if email field is a valid email? otherwise might crash app.
    # 
    smtp_send(
      survey_message,
      from = "renatadiaz.sci@gmail.com",
      #change this
      to = "renatadiaz.sci@gmail.com",
      #eventually replace this with input$email
      subject = "New survey response",
      credentials = creds_envvar(
        user = "renatadiaz.sci@gmail.com",
        pass_envvar = "SMTP_PASSWORD",
        provider = NULL,
        host = "smtp.gmail.com",
        port = 465,
        use_ssl = FALSE
      )
    )
    
    # Send email to IU listserv inviting person
    # listserv_message <- compose_email(
    #   body = paste("invite psinet-l ", input$email)
    # )
    #
    # smtp_send(survey_message,
    #           from = "renatadiaz.sci@gmail.com", #change this
    #           to = "renatadiaz.sci@gmail.com", #eventually replace this with IU listserv email
    #           subject = "Add to listserv",
    #           credentials = creds_file(".secrets/gmail_creds")
    # )
    
    
    
  })
  
}