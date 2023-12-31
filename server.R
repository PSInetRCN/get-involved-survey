function(input, output, session) {
  renderSurvey()
  
  observeEvent(input$submit, {
    posit_board <- board_connect()
    
    previous <- posit_board |>
      pin_read("renatadiaz/getinvolved_responses")
    
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
    
    response <- response |>
      mutate(response_id = max(previous$response_id) + 1)
    
    updated <- bind_rows(previous, response)
    
    # Write back to pin
    posit_board |>
      pin_write(updated, "renatadiaz/getinvolved_responses")
    
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
      if (grepl("listserv", input$platform)) {

        #  Send email to IU listserv inviting person
        listserv_message <- compose_email(
          body = paste("ADD psinet-l ", input$email)
        )

        smtp_send(listserv_message,
                  from = "psinetrcn@gmail.com", #change this
                  to = "list@list.indiana.edu",
                  subject = paste("ADD psinet-l ", input$email),
                  credentials = creds_envvar(
                    user = "psinetrcn@gmail.com",
                    pass_envvar = "SMTP_PASSWORD",
                    provider = NULL,
                    host = "smtp.gmail.com",
                    port = 465,
                    use_ssl = FALSE
                  )
        )
      }

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