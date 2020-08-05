# Assign all the packages you want to install to the object "packages" and install themm all at once
packages <- c("googledrive", "shiny", "miniUI", "shinyFiles", "devtools", "googleLanguageR", "cld2", "splashr", "RSelenium", "xml2", "robotstxt", "rvest", "DT","patchwork", "rowr","berryFunctions","plotrix","CGPfunctions","tidyr","ggthemes","likert","extrafont","scales","dlookr","outliers", "stringi", "tibble", "rlist", "dplyr", "rJava", "jsonlite", "NPS", "httr", "Hmisc", "ggpubr", "RColorBrewer", "stringr", "xlsx", "plotrix","tools","AMR","tidyverse","scales","tidyr","BBmisc","ggplot2","grid","networkD3","likert","openxlsx","DT",'formattable',"stringi", "tibble", "rlist", "dplyr", "rJava", "jsonlite", "NPS", "httr", "Hmisc", "ggpubr", "RColorBrewer", "stringr", "xlsx", "plotrix")
# Load all the packages at once
lapply(packages, require, character.only=TRUE)
#Address of the login webpage
login<-"https://www.impactpool.org/signin"
# Set-up RSelenium
cprof <- getChromeProfile(
  "~/Library/Application Support/Google/Chrome",
  "Default")
chrome_versions <- binman::list_versions("chromedriver")
rD <- rsDriver(browser = "chrome", port = 4444L, geckover = NULL, 
               chromever =  chrome_versions[[1]][6], iedrver = NULL, 
               phantomver = NULL, extraCapabilities = cprof)
remDr <- rD[["client"]] 
search <- c("&q=data%20analyst")
url <- paste0("https://www.impactpool.org/search?", search)
remDr$open()
# Make sure you are allowed to scrap a web page
paths_allowed("https://www.impactpool.org")
remDr$navigate(url[1])
remDr$navigate(login)
username_btn <- remDr$findElement(using ="id" , "signin_email")
username_btn$sendKeysToElement(list("gchourd@hotmail.com"))
pass_btn <- remDr$findElement(using = "id", "signin_password")
pass_btn$sendKeysToElement(list("Londonaki123", "\uE007"))

all_hyperlinks <- c()
html_data <- c()
while (remDr$findElement(using = "xpath", '//*[@id="search"]/div/div/div[2]/div[6]/div[1]/div[3]/div[2]/a')$isElementDisplayed()[[1]]){
  for(i in 1:length(search)){
   remDr$navigate(url[i])
   load_btn <- remDr$findElement(using = "xpath", '//*[@id="search"]/div/div/div[2]/div[6]/div[1]/div[3]/div[2]/a')
   Sys.sleep(2)
   load_btn$clickElement()
   Sys.sleep(2)
   html_data[i] <- remDr$getPageSource()[[1]]
   all_hyperlinks[[i]] <- html_data[i] %>%
    read_html() %>%
    html_nodes('.job-description-link') %>%
    html_attr("href")
  }
}
all_hyperlinks <- unlist(all_hyperlinks)
job_scraper <- data.frame()
job_summary <- c()
job_description <- c()
job_title <- c()
job_deadline <- c()
job_location <- c()
organisation_name <- c()
apply_links <- c()
url2 <- c()
page <- c()
for(i in 1:length(all_hyperlinks)){
  url2[i] <- paste0("https://www.impactpool.org", all_hyperlinks[i])
  page[[i]] <- read_html(url2[i])}
for(i in 1:length(all_hyperlinks)){
      tryCatch(job_summary[i] <- page[[i]] %>% 
                 html_nodes("div") %>%
                 html_node(xpath = '//*[@id="vacancy"]/main/div/div/div[1]/div[2]/div[2]/ul') %>%
                 html_text() %>%
                 .[1] %>%
                 str_replace_all(c("\n\n\n"=".", "\n\n"=" ", "\n"=" ")) %>%
                 str_trim("both"), 
               error = function(e){NA})}
for(i in 1:length(all_hyperlinks)){
      job_title[i] <- page[[i]] %>% 
                 html_nodes("div") %>%
                 html_nodes(xpath = '//*[@id="vacancy"]/main/div/div/div[1]/div[1]/h1') %>%
                 html_text() %>%
                 str_trim("both")}
for(i in 1:length(all_hyperlinks)){
      job_deadline[i] <- page[[i]] %>% 
                 html_nodes("div") %>% 
                 html_nodes(xpath = '//*[@id="vacancy"]/main/div/div/div[1]/div[2]/div[2]/ul/li[5]')%>% 
                 html_text() %>%
                 str_replace_all(c("Closing Date:\n"="", "\n"="")) %>%
                 str_trim("both")}
for(i in 1:length(all_hyperlinks)){
      job_description[i] <- page[[i]] %>%
                 html_nodes("span")  %>% 
                 html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "job-description", " " ))]') %>% 
                 html_text() %>%
                 str_replace_all(c("We do our best to provide you the most accurate info, but closing dates may be wrong on our site. Please check on the recruiting organization's page for the exact info. Candidates are responsible for complying with deadlines and are encouraged to submit applications well ahead."="")) %>%
                 str_trim("both") %>%
                 str_squish()}
for(i in 1:length(all_hyperlinks)){
      job_location[i] <- page[[i]] %>% 
                 html_nodes("ul") %>% 
                 html_nodes(xpath = '//*[@id="vacancy"]/main/div/div/div[1]/div[2]/div[2]/ul/li[2]')%>% 
                 html_text() %>%
                 str_replace_all(c("\nLocation:"="", "\n"="")) %>%
                 str_trim("both")}
for(i in 1:length(all_hyperlinks)){
      organisation_name[i] <- page[[i]] %>% 
                 html_nodes("ul")  %>% 
                 html_nodes(xpath = '//*[@id="vacancy"]/main/div/div/div[1]/div[2]/div[2]/ul/li[1]')  %>% 
                 html_text() %>%
                 str_replace_all(c("Organization:"="", "\n"="")) %>%
                 str_trim("both")}
for(i in 1:length(all_hyperlinks)){
  tryCatch(apply_links[i] <- page[[i]] %>% 
                html_nodes("div") %>%
                html_nodes(xpath ='//*[@id="vacancy"]/main/div/div/div[1]/div[4]/div/a') %>%
                html_attr("href"), 
               error = function(e){NA})}
job_scraper <- data.frame(job_title, organisation_name, job_deadline , job_location, job_summary, job_description, apply_links)
names(job_scraper) <- c("Job title", "Organisation name", "Deadline", "Location", "Summary", "Description", "Application link")
write.xlsx(job_scraper, "j-ad.xlsx")
remDr$close()
rD[["server"]]$stop()
.rs.restartR()







# Web scraper for Indeed jobs
listings <- c() 
search2 <- c("data%20analyst")
url3 <- paste0("https://www.indeed.co.uk/jobs?q=", search2)
url4 <- c()

for(i in 1:length(search2)){
  for (j in seq(0, 70, 10)){
    url4 <- paste0(url3[i], "&start=", j)
    page2 <- read_html(url4)
    
    job_title2 <-  page2 %>% 
      html_nodes('#resultsCol .jobtitle') %>%
      html_text() %>%
      str_extract("(\\w+.+)+")
    
    organisation_name2 <- page2 %>% 
      html_nodes('#resultsCol .company') %>%
      html_text() %>%
      str_extract("(\\w+).+")
    
    job_location2 <- page2 %>%
      html_nodes('#resultsCol .location') %>%
      html_text() %>%
      str_extract("(\\w+.)+,.[A-Z]{2}")
    
    job_summary2 <- page2 %>%
      html_nodes('#resultsCol .summary') %>%
      html_text() %>%
      str_extract(".+")
    
    link2 <- page2 %>%
      html_nodes('#resultsCol .jobtitle .turnstileLink, #resultsCol a.jobtitle') %>%
      html_attr('href') 
    link2 <- paste0("https://www.indeed.com",link2)
    
    job_deadline2 <- NA
    job_description2 <- NA
    listings <- as.data.frame(cbind(job_title2, organisation_name2, job_deadline2, job_location2, job_summary2, job_description2, link2))
    names(listings) <- c("Job title", "Organisation name", "Deadline", "Location", "Summary", "Description", "Application link")
    listings <- rbind(job_scraper, listings)
    
    }
}

#obtain full description for all job postings
for (i in (nrow(job_scraper)+1:length(listings$`Application link`))){
  desciption <- as.character(tryCatch(read_html(as.character(listings$`Application link`[i])) %>%
                            html_node(xpath = '//*[@id="jobDescriptionText"]') %>%
                            html_text(),
                          error=function(e){NA}))
  #desciption <- tryCatch(html_text(html_node(read_html(as.character(listings$link2[i])),'.jobsearch-JobComponent-description')), 
  #                       error=function(e){NA})
  if (is.null(desciption)){
    desc <- NA
  }
  listings$Description[i] <- desciption
}


