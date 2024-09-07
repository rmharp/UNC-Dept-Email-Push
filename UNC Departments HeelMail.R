#' ---
#' title: "UNC Departments HeelMail"
#' output: html_document
#' date: "`r Sys.Date()`"
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------
### If first time using some of the packages then required packages will be installed before being loaded via library, otherwise installed packages will be loaded via require

if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)}
if(!require(rvest)) {install.packages("rvest"); library(rvest)}
if(!require(wdman)) {install.packages("wdman"); library(wdman)}
if(!require(netstat)) {install.packages("netstat"); library(netstat)}
if(!require(xml2)) {install.packages("xml2"); library(xml2)}
if(!require(purrr)) {install.packages("purrr"); library(purrr)}
if (!require(RSelenium)) {install.packages("RSelenium"); library(RSelenium)}
if (!require(shiny)) {install.packages("shiny"); library(shiny)}
if (!require(remotes)) {install.packages("remotes"); library(remotes)}
if (!require(SummeRnote)) {install_github("LukasK13/SummeRnote"); library(SummeRnote)}

#' 
## -------------------------------------------------------------------------------------
### Run code chunk to collect latest Director of Undergraduate Studies and Student Services Manager Contact Information in a dataframe

page <- read_html("https://curricula.unc.edu/departmental-contacts/?wpv_aux_current_post_id=191&wpv_aux_parent_post_id=191&wpv_view_count=645")
rows <- page %>% html_nodes("table tr")
extracted_data <- lapply(rows, function(row) {
  # Initialize placeholders for department, role
  department <- NA
  role <- NA
  
  # Extract text from all cells (assuming the first cell is department, the second is role)
  cells <- row %>% html_nodes("td") %>% html_text(trim = TRUE)
  if (length(cells) >= 1) {
    department <- cells[1]
  }
  if (length(cells) >= 2) {
    role <- cells[2]
  }
  
  # Extract and split email addresses
  email_links <- row %>% html_nodes("a[href^='mailto:']") %>% html_attr("href")
  emails <- if (length(email_links) > 0) {
    unlist(lapply(email_links, function(link) {
      # Remove 'mailto:' and then split by ';'
      split_emails <- strsplit(gsub("mailto:", "", link), ";\\s*")
      unlist(split_emails) # Ensure split_emails is flattened into a single vector
    }))
  } else {
    character(0) # Ensure this is an empty character vector if no emails
  }
  
  list(Department = department, Role = role, Emails = emails)
})

final_table <- data.frame(Department = character(), Role = character(), Email = character(), stringsAsFactors = FALSE)

# Populate final_table, ensuring multiple emails result in duplicated rows
for (row in extracted_data) {
  if (length(row$Emails) > 0) {
    for (email in row$Emails) {
      # Create a new row for each email
      new_row <- data.frame(Department = row$Department, Role = row$Role, Email = email, stringsAsFactors = FALSE)
      final_table <- rbind(final_table, new_row)
    }
  } else {
    # If no emails, add the row with NA for Email
    new_row <- data.frame(Department = row$Department, Role = row$Role, Email = NA, stringsAsFactors = FALSE)
    final_table <- rbind(final_table, new_row)
  }
}
filtered_table <- final_table %>%
  filter(str_detect(Role, "DUS") | str_detect(Role, "SSM"))

table_grouped <- filtered_table %>% 
  filter(!is.na(Email)) %>%
  group_by(Department) %>% 
  summarise(Email = paste(Email, collapse = ", "))

#' 
## -------------------------------------------------------------------------------------
### Enter your email login and password for the email you would like to send from as well as the email you'd like to send below and then run the code chunk chunk to send the email to all of the UNC departments
username <- ""
password <- ""

# If you would like to test sending your email to yourself you can do so here, otherwise leave this as an empty string
testEmail <- ""

# If you would like to cc anyone please add them below with a , between each email (eg xyz@unc.edu, admin@gmail.com), otherwise leave this as an empty string
ccEmailAddresses <- ""

# If you would like to send emails with high importance turned on set true, otherwise set false
highImportance <- FALSE

# Start the Selenium server with a specified port (e.g., 4567)
rD <- rsDriver(browser = "firefox", chromever = NULL, port = netstat::free_port())
remDr <- rD$client

# Navigate to a website
remDr$navigate("http://heelmail.unc.edu/")
Sys.sleep(1.5)
login <- remDr$findElement(using = 'css selector', value = 'input[type="email"]')
login$sendKeysToElement(list(username))
next_button <- remDr$findElement(using = 'css selector', value = 'input[type="submit"]')
next_button$clickElement()
Sys.sleep(1.5)
pass <- remDr$findElement(using = 'css selector', value = 'input[type="password"]')
pass$sendKeysToElement(list(password))
signin_button <- remDr$findElement(using = 'css selector', value = 'input[type="submit"]')
signin_button$clickElement()
Sys.sleep(1.5)
textbutton <- remDr$findElement(using = 'xpath', value = "//div[contains(text(), 'Text')]")
textbutton$clickElement()
Sys.sleep(1.5)
code <- readline(prompt = "Enter the code: ")
codeloc <- remDr$findElement(using = 'css selector', value = 'input[type="tel"]')
codeloc$sendKeysToElement(list(code))
verify_button <- remDr$findElement(using = 'css selector', value = 'input[type="submit"]')
verify_button$clickElement()
Sys.sleep(1.5)
check_button <- remDr$findElement(using = 'css selector', value = 'input[type="checkbox"]')
check_button$clickElement()
yes_button <- remDr$findElement(using = 'css selector', value = 'input[type="submit"]')
yes_button$clickElement()
Sys.sleep(1.5)
if (testEmail != "") {
  numEmailsToSend <- 1
} else {
  numEmailsToSend <- nrow(table_grouped)
}
for (i in 1:numEmailsToSend) {
  newMailButton <- remDr$findElement(using = 'css selector', value = 'button[aria-label="New mail"]')
  newMailButton$clickElement()
  Sys.sleep(1.5)
  if (highImportance == TRUE) {
    messageButton <- remDr$findElement(using = 'xpath', value = "//button[.//span[text()='Message']]")
    messageButton$highlightElement()
    messageButton$clickElement()
    Sys.sleep(0.5)
    success <- FALSE
    while (!success) {
      tryCatch({
        # Try to find and click the second "More options" button
        moreOptionsButtons <- remDr$findElements(using = 'css selector', value = 'button[aria-label="More options"]')
        moreOptionsButton <- moreOptionsButtons[[2]]
        moreOptionsButton$highlightElement()
        moreOptionsButton$clickElement()
        Sys.sleep(1.5)
        # Try to find and click the "High importance" button
        highImportanceButton <- remDr$findElement(using = 'css selector', value = 'button[aria-label="High importance"]')
        highImportanceButton$highlightElement()
        highImportanceButton$clickElement()
        # If no error occurs, set success to TRUE to exit the loop
        success <- TRUE
      }, error = function(e) {
        # If an error occurs, print the error message and try again
        cat("An error occurred: ", e$message, "\nRetrying...\n")
        Sys.sleep(1)  # Wait before retrying
      })
    }
  }
  emailAddresses <- table_grouped[[i, 2]]
    if (testEmail != "") {
      emailAddresses <- testEmail
  }
  department <- table_grouped[i, 1]
  emailAddresses <- strsplit(emailAddresses, ", ")[[1]]
  emailInputDiv <- remDr$findElement(using = 'css selector', value = 'div[aria-label="To"]')
  for(email in emailAddresses) {
    emailInputDiv$sendKeysToElement(list(email))
    Sys.sleep(2.75)
    emailInputDiv$sendKeysToElement(list(key = "enter"))
  }
  if (ccEmailAddresses != "") {
    ccEmailAddresses <- strsplit(ccEmailAddresses, ", ")[[1]]
    ccEmailInputDiv <- remDr$findElement(using = 'css selector', value = 'div[aria-label="Cc"]')
    for(email in ccEmailAddresses) {
      ccEmailInputDiv$sendKeysToElement(list(email))
      Sys.sleep(2.5)
      ccEmailInputDiv$sendKeysToElement(list(key = "enter"))
    }
  }
  subject <- "[Call for Editors] Office of Undergraduate Research Journal"
  subjectElem <- remDr$findElement(using = 'css selector', value = 'input[aria-label="Add a subject"]')
  subjectElem$sendKeysToElement(list(subject))
  body <- paste0("Good Evening,
  
My name is ..., and I'm a ... for the ... . <strong>We are reaching out to invite students from the ", department," department to apply for ... .</strong> Students interested in joining the can submit an application by completing the form available <a href='enter link here'>here.</a> The deadline for applications is ... .</strong>

Please let me know if you have any questions or need additional information. Thank you for your support in helping us spread the word about this opportunity.")
  body_escaped <- gsub("'", "\\\\'", body)  # Escape single quotes
  body_escaped <- gsub("\n", "<br>", body_escaped)  # Replace newlines with <br> tags for HTML if you want to preserve them in some way)
  # JavaScript to find the first elementToProof div and insert the bold text
  script <- sprintf("var editor = document.querySelector('div[contenteditable=\\'true\\'][aria-label=\\'Message body, press Alt+F10 to exit\\']');
  if (editor) {
      var newElement = document.createElement('div');
      newElement.className = 'elementToProof';
      newElement.style.fontFamily = 'Times New Roman, Times, serif';
      newElement.style.fontSize = '12pt';
      newElement.style.color = 'rgb(0, 0, 0)';
      newElement.innerHTML = '%s<br>';
  
      if (editor.firstChild) {
          editor.insertBefore(newElement, editor.firstChild);
      } else {
          editor.appendChild(newElement);
      }
  }", body_escaped)
  remDr$executeScript(script, args=list(list()))
  
  ## If you plan to attach files this can be done by dragging them into the email and then typing anything into the command line if the line below is uncommented
  #readline(prompt = "Are you finished attaching files?\n\n")
  
  Sys.sleep(1.5)
  sendButton <- remDr$findElement(using = 'css selector', value = 'button[aria-label="Send"]')
  sendButton$clickElement()
  Sys.sleep(1.5)
  # Attachment Reminder
  tryCatch({
    ARsendButton <- remDr$findElement(using = 'css selector', value = 'button#ok-1')
    ARsendButton$highlightElement()
    ARsendButton$clickElement()
    Sys.sleep(1.5)
  }, error = function(e) {
    cat("Attachment Reminder send button not found: ", e$message, "\n")
  })
  Sys.sleep(1.5)
}

