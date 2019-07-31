## This script will do webscraping and can be adapted to fit multiple contexts

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
pages_per_category <- c(9, 7, 11, 8, 22, 7, 26, 46) # this info is taken by visual inspection. A more robust algorithm would draw it from the page to make sure it is up to date with the site
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

# initialize a tibble to store results of scraping
dante_today_all <- list()
dante_raw <- list()

# the loop below goes through each url to scrape and then extracts the post information 
# from each page. typically there are 10 posts per page. 
# It stores the urls from the entry itself as a list item but does not preserve which parts
# of the original text linked to those urls.

for (k in seq_along(urls_to_scrape)) {
    # grab the page
  
    dante_data <- read_html(urls_to_scrape[[k]])
    dante_raw[[k]] <- dante_data
    
    # seprate out by post
    posts <- dante_data %>% html_nodes(".post")
    
    # initialize a list to store the page of posts as a list of lists
    page <- list()
    
    # iterate over each post to extract relevant data
    for (l in seq_along(posts)) {
      posts[l] %>% html_node(".entry-title") %>% html_text() -> title
      posts[l] %>% html_node(".date") %>% html_text() -> date
      posts[l] %>% html_node(".date") %>% html_attr("title") -> date_utc
      posts[l] %>% html_node(".entry-content") %>% html_node("img") %>% html_attr("src") -> post_img
      posts[l] %>% html_node(".entry-content") %>% html_text() -> post_text
      posts[l] %>% html_node(".entry-content") %>% html_nodes("a") %>% html_attr("href") -> post_links
      posts[l] %>% html_node(".author") %>% html_text() -> author
      posts[l] %>% html_node("span.categories") %>% html_nodes("a") %>% html_text() -> categories
      posts[l] %>% html_node(".tags") %>% html_nodes("a") %>% html_text() -> tags
      
      # store that as a list
      page[[l]] <- list(title, date, date_utc, post_img, post_text, post_links, author, categories, tags)
    }
    
    dante_today_all[[k]] <- page
    
     # insert a time lag so as to not get banned
    # the sample function provides a quick and easy way to generate a random number for a random interval
    sleepytime <- sample(60:360, 1)
    Sys.sleep(sleepytime)
}
  
  
# =============================================================
# The result of that scraping will be a list with 3 levels. the first is the url level, 
# where each page is a list item. The second is a the indivdual post on that page, so the
# range of these is usually 1 to 10 entries (usually 10 entries per page). 
# The third list depth is for multiple links and tags, and categories and can vary in its length.
#
#
# 
# =============================================================

# we undo the process of building the list by smushing everything together with rbind. 

dante_all_flat <- unlist(dante_today_all, recursive = FALSE) # This function reshapes the list so that we can apply the do.call function

dante_df <- do.call("rbind",dante_all_flat)

dante_tbl <- dante_df %>% as_tibble()

# the object produced here would likely need to combine the sublists in order to be useable as a tibble