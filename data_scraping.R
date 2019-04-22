# package
library(rvest)
library(tidyverse)

# URLs to scrape data from
doshisha_url_list = paste0("https://unilife.co.jp/search/range/lat:35.03017044/lon:135.76075745/dis:1.5/page:", 1:7, "/l:20/?#schoollink")
kyoto_uni_url_list = paste0("https://unilife.co.jp/search/range/lat:35.04909134/lon:135.77854919/dis:1.5/page:", 1:6, "/l:20/?#schoollink")
url_list = c(doshisha_url_list, kyoto_uni_url_list)

apartment_url_list = list()
len = length(url_list)

for (i in 1:len) {
  page = read_html(url_list[[i]])
  apartment_url_list[[i]] = page %>%
    html_nodes("p.resultTit") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  print(i) # showing progress
}

doshisha_num = 133
kyo_uni_num = 109


vec <- Reduce(c,apartment_url_list)
vec

data.frame(paste0("https://unilife.co.jp", vec))

num = 0
for (i in 1:7){
  len = length(apartment_url_list[[i]])
  num = num + len
  print(num)
}

# 104 apartments


