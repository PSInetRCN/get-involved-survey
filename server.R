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
    
    email_ok <- check_email(input$email)

    if(email_ok) {

    # Show submission message
    showModal(
      modalDialog(
        title = "Your answers have been recorded. Thank you!",
        "Please reach out to Kim Novick (knovick@indiana.edu) or Jessica Guo (jessicaguo@arizona.edu) with any questions. "
      )
    )
    
    # Email

    body_text0 <-
      paste0("Hi ", input$name_first, " ", input$name_last, ",")

    body_text1 <- paste("Thank you for expressing interest in PSInet!")


    body_text2 <- ""
    if (grepl("Slack", input$platform)) {
      body_text2 <- paste0("Please use this invitation link to join our Slack channel: ", Sys.getenv("SLACK_LINK"), ".")
    }

    body_text2p5 <- ""

    if (grepl("listserv", input$platform)) {
      body_text2p5 <-
        "This email address has been automatically added to the PSInet listserv."
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
        block_spacer(),
        body_text2p5,
        block_spacer(),
        body_text3,
        block_spacer(),
        body_text4,
        block_spacer(),
        body_text5
      ),
      footer = "Please note that this email inbox is not monitored. If you have questions or concerns, please reach out to Jessica Guo (jessicaguo@arizona.edu) or Kim Novick (knovick@indiana.edu)."
    )


    smtp_send(
      survey_message,
      from = "psinetrcn@gmail.com",
      #change this
      to = input$email,
      subject = "Welcome to PSInet!",
      credentials = creds_envvar(
        user = "psinetrcn@gmail.com",
        pass_envvar = "SMTP_PASSWORD",
        provider = NULL,
        host = "smtp.gmail.com",
        port = 465,
        use_ssl = FALSE
      )
    )
# 
#     Send email to IU listserv inviting person
#     listserv_message <- compose_email(
#       body = paste("invite psinet-l ", input$email)
#     )
# 
#     smtp_send(survey_message,
#               from = "psinetrcn@gmail.com", #change this
#               to = "renatadiaz.sci@gmail.com", #eventually replace this with IU listserv email
#               subject = "Add to listserv",
#               credentials = creds_file(".secrets/gmail_creds")
#     )
#     
    } else {
      # Show submission message
      showModal(
        modalDialog(
          title = "Please check the email provided; it does not appear to be valid!",
          "Please reach out to Kim Novick (knovick@indiana.edu) or Jessica Guo (jessicaguo@arizona.edu) with any questions. "
        )
      )
      
    }
    
    
  })
  
}