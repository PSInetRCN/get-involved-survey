check_email <- function(email_provided) {
  
  # Check that email contains @ exactly 1 time
  
  if(length(str_extract_all(pattern = "@", email_provided)[[1]]) != 1) {
    return(FALSE)
  }
  
  # Split email into pre and post @ sections
  
  at_split <- str_split(email_provided, pattern = "@", simplify = T)
  
  # Check that first @ section:
  # begins and end with a letter or a number  [:alnum:]: letters and numbers.
  # is the right number of characters (1:64)
  
  user_name <- at_split[1]
  endletters <- c(substr(user_name, 1,1), substr(user_name, nchar(user_name), nchar(user_name)))
  
  if(any(!str_detect(pattern = "[[:alnum:]]", endletters))) {
    return(FALSE)
  }
  
  if(nchar(user_name) > 64) {
    return(FALSE)
  }

  # Check that the second @ section:
  # Contains at least one .
  # Does not begin or end with .
  
  domain_section <- at_split[2]
  
  if(!str_detect(domain_section, "\\.")) {
    return(FALSE)
  }
  
  domain_endletters <- c(substr(domain_section, 1,1), substr(domain_section, nchar(domain_section), nchar(domain_section)))
  
  if(any(str_detect(pattern = "\\.", domain_endletters))) {
    return(FALSE)
  }
  
  return(TRUE)
  
}



