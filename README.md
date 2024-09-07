# UNC Dept Email Push

UNC Dept Email Push is an R script designed to automate the collection of departmental contact information (e.g., Directors of Undergraduate Studies and Student Services Managers) from the UNC website. The script then sends emails to these contacts using Gmail with the intention of disseminating event-related resources to students.

## Features
- Automatically scrapes the latest contact information from the UNC Departmental Contacts webpage.
- Uses Gmail to send personalized emails to the extracted contacts.
- Uses HeelMail to send personalized emails to the extracted contacts.

## Getting Started

### Prerequisites

Before running the script, ensure you have the following installed (instructions are provided in the rmd file for setting up the Gmail API):

- R (version 4.0 or higher)
- The following R packages: `dplyr`, `tidyverse`, `rvest`, `wdman`, `netstat`, `xml2`, `purrr`, and `gmailr`
- A Gmail account with an app password configured for third-party apps (as direct login using your regular password may not be supported) or a HeelMail account.

### Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/rmharp/UNC-Dept-Email-Push.git
   cd UNC-Dept-Email-Push
   ```
