## This script will do webscraping and can be adapted to fit multiple contexts

## Version 0.2: This method may take longer because it will build a tibble in memory as it goes.
# In general that is not great coding practice (better to build all at the end), but
# the various layers of nesting on the page make it annoying to deal with the full list and also
# make it useful to debug based on where exactly the process runs into trouble.

# load required libraries

library(robotstxt)
library(rvest)
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(forcats)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)
library(readr)

# check that we can do web scraping
# paths_allowed(
#   paths = c("https://research.bowdoin.edu/dante-today/")
# )

# returns true for bowdoin research site; ok to scrape


# ================================================================
# The first part of code figures out the correct urls for scraping. This is, at present
# done with visual inspection of the site rather than automation, as there are not 
# a huge number of pages.
# =================================================================

# list of categories for the site
category_list <- c("consumer-goods", "dining-leisure", "music", "odds-ends", "performing-arts", "places", "visual-art-architecture", "written-word")

# get urls for each category page

category_urls <- paste0("https://research.bowdoin.edu/dante-today/category/",category_list)
pages_per_category <- c(9, 7, 11, 8, 22, 7, 26, 47) # this info is taken by visual inspection. A more robust algorithm would draw it from the page to make sure it is up to date with the site
page_urls <- list() # initialize a list that will be used to store the urls

# generate the urls for all the pages that need to be scraped.
for (i in seq_along(pages_per_category)) {
  page_numbers <- 1:pages_per_category[[i]]
  page_urls[[i]] <- paste0(category_urls[[i]],"/page/",page_numbers)
}

# to simplify the iteration through the pages, each one needs a unique index

urls_to_scrape <- unlist(page_urls)


# =========================================================================
# The following code does the scraping. Make sure that the interval is set to
# something sensible so you don't get locked out of the site.
# =======================================================================

# the loop below goes through each url to scrape and then extracts the post information 
# from each page. typically there are 10 posts per page. 
# It stores the urls from the entry itself as a list item but does not preserve which parts
# of the original text linked to those urls.

# initialize tibble for storing results
dante_today_tbl <- tibble()


# initialize a list for storing raw results
dante_raw <- list()

# List of nodes needed
nodes_wanted <- c(".entry-title", ".date", ".entry-content", ".author", "span.categories", ".tags")

for (k in seq_along(urls_to_scrape)) {
    
    cat("Scraping... ", urls_to_scrape[[k]], "\n")
    # grab the page
  
    dante_data <- read_html(urls_to_scrape[[k]])
    dante_raw[[k]] <- dante_data
    
    # separate out by post
    posts <- dante_data %>% html_nodes(".post")
    
    
    # iterate over each post to extract relevant data
    for (l in seq_along(posts)) {
      
      # get first level of html_nodes and check if empty
      nodes_firstlevel <- map2(.x = posts[l], .y = nodes_wanted, .f = html_node)
      
      if (is.na(nodes_firstlevel[[1]])) {
        title <- NA
      } else {
        posts[l] %>% html_node(".entry-title") %>% html_text() -> title
      }
      
      if (is.na(nodes_firstlevel[[2]])) {
        date <- NA
        date_utc <- NA
      } else {
        posts[l] %>% html_node(".date") %>% html_text() -> date
        posts[l] %>% html_node(".date") %>% html_attr("title") -> date_utc
      }
      
      if (is.na(nodes_firstlevel[[3]])) {
        post_img <- NA
        post_text <- NA
        post_text_raw <- NA
        post_links <- NA
      } else {
        posts[l] %>% html_node(".entry-content") %>% html_node("img") %>% html_attr("src") -> post_img
        posts[l] %>% html_node(".entry-content") %>% html_text() -> post_text
        posts[l] %>% html_node(".entry-content") -> post_text_raw
        posts[l] %>% html_node(".entry-content") %>% html_nodes("a") %>% html_attr("href") -> post_links
      }
      
      if (is.na(nodes_firstlevel[[4]])) {
        author <- NA
      } else {
        posts[l] %>% html_node(".author") %>% html_text() -> author
      }
     
      if (is.na(nodes_firstlevel[[5]])) {
        categories <- NA
      } else {
        posts[l] %>% html_node("span.categories") %>% html_nodes("a") %>% html_text() -> categories
      }
      
      if (is.na(nodes_firstlevel[[6]])) {
        tags <- NA
      } else {
        posts[l] %>% html_node(".tags") %>% html_nodes("a") %>% html_text() -> tags
      }
     
      # store that as a tibble. 
      dante_today_tbl <- rbind(dante_today_tbl, tibble(title = title, 
                                date = date, 
                                date_utc = date_utc, 
                                post_img = post_img, 
                                post_text = post_text, 
                                post_text_raw = post_text_raw,
                                post_links = paste0(post_links, collapse = ", "), 
                                post_author = author, 
                                post_categories = paste0(categories, collapse = ", "),
                                post_tags = paste0(tags, collapse = ", ")
                                )
      )
    }
    
 
     # insert a time lag so as to not get banned
    # the sample function provides a quick and easy way to generate a random number for a random interval
    sleepytime <- sample(10:20, 1)
    Sys.sleep(sleepytime)
}
  
  
# =============================================================
# The result of that scraping will be a tibble with each of the extracted fields
# The tibble can be saved as a csv and can also be manipulated within R. 
# The functions below are a quick sample of the kind of thing that can be done with this data
# 
# ==========================================================

# write the output
write_csv(dante_today_tbl, path = "~/DEMOS/DEMOS_PROJECTS/dantescrapeR/dante_today.csv" )
write_lines(dante_raw, path = "~/DEMOS/DEMOS_PROJECTS/dantescrapeR/dante_raw.txt")
