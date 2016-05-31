
cnt <- 1
for(job in job_urls[1:12]){
  job_page_html <- read_html(paste("job", cnt, ".html", sep = ""))
  #GET(job, user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64; rv:42.0) Gecko/20100101 Firefox/42.0")) %>% read_html
  selector_name_basic <- '//*[@id="summary-detail"]//div[@class="rich-text"]'
  selector_name_ul <- paste(selector_name_basic, '//ul', sep = "")
  job_description_title <- html_nodes(job_page_html, xpath = selector_name_ul) %>% html_text()
  if(length(job_description_title) > 0){
    for (li in 1:length(job_description_title)){
      selector_name_li <- paste(selector_name_ul, '[', li, ']//li', sep ="") 
      job_description <- html_nodes(job_page_html, xpath = selector_name_li) %>% html_text()
      #print(job_urls_li)
    }
  } else {
    job_description <- html_nodes(job_page_html, xpath = selector_name_basic) %>% extract2(1) %>% html_text()
  }
  cnt <- cnt + 1
}


library(RSelenium)
shell.exec("batchfile.bat")
Sys.sleep(5)
RSelenium::checkForServer()
RSelenium::startServer(args = c("-port 5000"))
require(RSelenium)
remDr <- remoteDriver(browserName = "chrome")
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 5000
                      , browserName = "chrome"
)
remDr$open() # open web browser



lnkdn_page_html <- read_html("linkedin.html")
selector_name <- "a[class='title main-headline']"
job_urls <- html_nodes(lnkdn_page_html, selector_name) %>% html_attr('href')