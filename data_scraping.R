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


vec <- Reduce(c,apartment_url_list)
apartment_urls = data.frame(paste0("https://unilife.co.jp", vec))

df_list = list()

for (i in c(1:nrow(apartment_urls))) {
  print(i)
  url = apartment_urls[i,]
  page = read_html(as.character(url))
  title = page %>% 
    html_nodes("h1.title") %>% 
    html_text() %>% 
    str_remove_all("\t")
  
  main_table = page %>% 
    html_nodes("table.tbl02")
  
  price_info = page %>% 
    html_nodes("div.twoColBlock") %>% 
    html_text() %>% 
    str_remove_all("\t") %>% 
    str_remove("※複数の間取りがある場合は数字をクリックして下さい。\n\n\n\n\n\n\n\n\n\n") %>% 
    str_remove("\n\n\n\n\n\n\n賃料情報\n")
  
  details = main_table %>% 
    html_nodes("tbody") %>% 
    html_text() %>% 
    magrittr::extract2(2) %>% 
    str_remove_all("\t")
  
  points = page %>% 
    html_nodes("ol.pointList") %>% 
    html_text() %>% 
    str_remove_all("\t")
  
  price_info2 = main_table %>% 
    html_nodes("tbody") %>% 
    html_text() %>% 
    magrittr::extract2(1) %>% 
    str_remove_all("\t")
  
  df_list[[i]] = data.frame(title, details, points, price_info, price_info2)
}

main_df = do.call(rbind, df_list)
# write.csv(main_df, "Kyoto_rent.csv")
