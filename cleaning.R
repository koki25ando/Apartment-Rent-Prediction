rent = read.csv("Kyoto_rent.csv")
rent = select(rent, -X)

## Title

rent$title = rent$title %>% 
  str_remove("\n\n\n")
rent$title = gsub(".*\n","",rent$title)

## Details

rent$details = str_remove(rent$details, "所在地\n〒")
rent = separate(rent, details, into = c("zip_code", "details"), sep = "　")


# price information

rent = select(rent, -price_info2)
rent$price_info = rent$price_info %>% 
  str_remove("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n") %>% 
  str_remove_all("\n\n\n\n\n\n\n\n\n\n") %>% 
  # str_remove("\n\n\n\n\n\n\n\n\n\n") %>% 
  str_remove("\n\n\n\n\n\n\n\n")

# Price

rent = separate(rent, price_info, into = c("price", "info"), sep = "\n\n")
rent$info = str_replace(rent$info, "\n", "ABC")
rent = separate(rent, info, into = c("price", "info"), sep = "ABC")


## address & station

rent$details = str_replace(rent$details, "\n最寄り駅\n\n", "NStation")
rent = separate(rent, details, into = c("address", "details"), sep = "NStation")
rent$details = str_replace(rent$details, "分", "分ABCD")
rent = separate(rent, details, into = c("station", "details"), sep = "ABCD")

## distance
rent$station = str_replace(rent$station, "京都市営バス「河原町今出川」駅\n京阪本線「出町柳」駅 ", "京都市営バス「河原町今出川」駅")
rent = separate(rent, station, into = c("station", "distance"), sep = "」")

View(rent)
