library(googlesheets4)
library(dplyr)
library(tidyr)
library(blastula)
library(stringr)
library(pins)

source(here::here("email_logic.R"))

gs4_auth(cache = gargle::gargle_oauth_cache(),
         email = gargle::gargle_oauth_email())

# Retrieve existing datasheet
sheet_url <-
  "https://docs.google.com/spreadsheets/d/1TZh0P8gsjvq572LzI0rWTikrvplyvMYVH8rpOJ7fz30/edit#gid=0"
previous <- read_sheet(sheet_url)


# Check for duplicated responses
dup_responses <- previous |>
  filter(
    !grepl("jessicaguo", subject_id),
    !grepl("kimberly.novick", subject_id),
    !grepl("jsg2139", subject_id)
  ) |>
  group_by(subject_id, question_id) |>
  summarize(n = dplyr::n(),
            n_unique = length(unique(response)),
            .groups = "drop") |>
  filter(n > 1)

# From dup_responses we see that we have 2 instances where the subject_id is duplicated.
# Means someone has filled out the survey twice with the same email address.
# For both these instances, both entries are perfect duplicates, so we can just drop one of them.

# Pivot responses to a wide format
responses <- previous |>
  filter(
    !grepl("jessicaguo", subject_id),
    !grepl("kimberly.novick", subject_id),
    !grepl("jsg2139", subject_id)
  ) |>
  select(-question_type) |>
  pivot_wider(
    id_cols = subject_id,
    names_from = question_id,
    values_from = response,
    values_fill = NA,
    values_fn = function(x)
      na.omit(x)[1]
  )

# Add contact management columns

responses_contacts <- responses |>
  mutate(
    welcome_email_sent = !is.na(timestamp),
    # anyone who entered the survey after timestamp was added got auto-emailed
    slack_invite_requested = grepl("Slack", platform),
    listserv_requested = grepl("listserv", platform),
    slack_invite_sent = grepl("Slack", platform) & !is.na(timestamp),
    # anyone who entered the survey after timestamp was added got auto-emailed
    listserv_added = grepl("listserv", platform) & !is.na(timestamp)
  ) # anyone who entered the survey after timestamp was added got auto-emailed

# One-off:
# Identify folks who have not gotten emails

need_emails <- responses_contacts |>
  filter(is.na(timestamp)) |>
  select(subject_id,
         name_first,
         name_last,
         slack_invite_requested,
         listserv_requested)

compose_and_send_emails <-
  function(subject_id,
           name_first,
           name_last,
           slack_invite_requested,
           listserv_requested) {
    
    email_ok <- check_email(subject_id)
    
    if (email_ok) {
      # Email
      
      body_text0 <-
        paste0("Hi ", name_first, " ", name_last, ",")
      
      body_text1 <-
        paste("Thank you for expressing interest in PSInet!")
      
      
      body_text2 <- ""
      if (slack_invite_requested) {
        body_text2 <-
          paste0(
            "Please use this invitation link to join our Slack channel: ",
            Sys.getenv("SLACK_LINK"),
            "."
          )
      }
      
      body_text2p5 <- ""
      
      if (listserv_requested) {
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
        to = subject_id,
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
      if (listserv_requested) {
        #  Send email to IU listserv inviting person
        listserv_message <- compose_email(body = paste("ADD psinet-l ", subject_id))
        
        smtp_send(
          listserv_message,
          from = "psinetrcn@gmail.com",
          #change this
          to = "list@list.indiana.edu",
          subject = paste("ADD psinet-l ", subject_id),
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
      
      return(data.frame(
        subject_id = subject_id,
        slack_invite_sent_now = slack_invite_requested,
        listserv_added_now = listserv_requested
      ))
      
    } else {
      return(paste("weird email: ", subject_id))
    }
  }

contacts_update <- purrr::pmap(need_emails, compose_and_send_emails)

contacts_update <- bind_rows(contacts_update)

responses_contacts_updated <- responses_contacts |>
  left_join(contacts_update) |>
  group_by(subject_id) |> 
  mutate(slack_invite_sent = any(slack_invite_sent, slack_invite_sent_now, na.rm = T),
         listserv_added = any(listserv_added, listserv_added_now, na.rm = T)) |>
  ungroup() |>
  select(!contains("now"))

posit_board <- board_connect()

posit_board <- posit_board |>
  pin_write(responses_contacts_updated, "renatadiaz/psinet_contacts")
