# Author: Jack Royal

#Load scraping library
library("rvest")

#Helper function which ensures doesn't timeout while scraping
#Author: Krzysztof Przygodzki
html_NE = function(x) {
  require(rvest)
  
  Page.src = try(html(x), silent = T)
  
  # test if Page.src is erroneous
  if (class(Page.src)[1] == "try-error") {
    
    error.cond = attr(Page.src, "condition")
    
    # check if error condition contains "Timed out" phrase.  If regexpr cannot find a
    # match it returns -1
    timed.out = regexpr("Timed out", error.cond, ignore.case = T) != -1
    
    # we want to continue only on "timed out" error
    if (timed.out == TRUE) {
      
      # print information in the console
      print(paste(x, ": Timed out. Trying to reconnect in 30s. Please wait..."))
      Sys.sleep(30)
      
      return(html_NE(x))
    }
  }
  
  return(Page.src)
}
#Empty List
rugbytable <- list()
#Iterates through each page grabbing the data from table of players up until page 383(Last Page)
for(i in 1:383){
  url <- paste0("http://stats.espnscrum.com/statsguru/rugby/stats/index.html?class=1;page=",i,";template=results;type=player")
  rugby <- url %>% html_NE() %>% html_nodes(xpath = '//*[@id="scrumArticlesBoxContent"]/table[2]') %>% html_table(header = TRUE)
  rugbytable[i] <- rugby
}

rugbytable
#Binds each item in list to create DF
rugbyPlayer <- do.call("rbind", rugbytable)
View(rugbyPlayer)

#Omits last column which is just full of NAs
rugbyPlayer <- rugbyPlayer[1:(length(rugbyPlayer)-1)]

#Export to CSV file for easy portability
write.csv(rugbyPlayer, "C:\\Users\\Jackr\\Desktop\\RugbyPlayerDataESPN.csv")
``