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


## station details

rent = separate(rent, station, into = c("station_type", "station"), sep = "「")
rent$distance = rent$distance %>% 
  str_remove("分") %>% 
  str_remove("駅 ") %>% 
  str_remove("駅") %>% 
  str_remove("停")

rent = rent %>% 
  mutate(methods = gsub('\\d','', distance),
         dis = gsub('\\D','', distance))
rent = select(rent, -distance)

# Price information

rent$info = rent$info %>% 
  str_remove("備考") %>% 
  str_replace_all("間取り\n", "\n") %>% 
  str_replace("間取図\n", "\n")

rent = separate(rent, info, into = c("info", "maintainance"), sep = "設備維持費\n")
rent = separate(rent, info, into = c("info", "key"), sep = "礼金\n")
rent = separate(rent, info, into = c("info", "deposits"), sep = "敷金\n")
names(rent)[9] = "admin"


## Details
separate(rent, details, into = c("details", "floor"), sep = "階数・全部屋数\n\n") %>% View
rent = separate(rent, details, into = c("details", "floor"), sep = "階数・全部屋数\n\n")
rent = separate(rent, details, into = c("details", "direction"), sep = "\n向き\n")
rent = separate(rent, details, into = c("details", "date"), sep = "\n完成年月\n")
rent = separate(rent, details, into = c("details", "renewal"), sep = "\n\n更新料\n")
rent = separate(rent, details, into = c("details", "contract"), sep = "\n契約年数\n")
rent = separate(rent, details, into = c("details", "area"), sep = "\n間取り／面積\n\n")

rent = select(rent, -details)


## cleansing

rent$contract = substr(rent$contract, 0, 1)
rent$floor = rent$floor %>% 
  str_remove_all("\n") %>% 
  str_remove("地上") %>% 
  str_remove_all("階建") %>% 
  str_remove("階/地下1") %>% 
  str_remove("階/地下33")
rent$admin = rent$admin %>%
  str_remove_all("\n") %>%
  str_remove("管理費") %>%
  str_remove("共益費") %>%
  str_remove("ユニットバス（一部セパレート）") %>%
  str_remove("～3,000円") %>% 
  str_remove("～3,500円") %>% 
  str_remove("食費：朝350円　夕650円") %>% 
  str_remove("4,500円～") %>% 
  str_replace("5,000円～6,000円", "5,000円") %>% 
  str_remove("0円～") %>% 
  str_remove("円") %>% 
  str_remove("\\,")

## Deposits
rent$deposits = rent$deposits %>% 
  str_remove_all("\n") %>% 
  str_remove("年間管理費26.4万円保証金5万円")

rent$deposits = gsub("入館金.*","", rent$deposits)
rent$deposits = gsub("～.*","", rent$deposits)

# Key
rent$key = rent$key %>% 
  str_remove_all("\n") %>% 
  str_remove("間取り図")
rent$key = gsub("衛生費.*", "", rent$key)
rent$key = gsub("～.*", "", rent$key)
rent$key = gsub("保証金.*", "", rent$key)
rent$key = rent$key %>% 
  str_remove("303号室") %>% 
  str_remove("2017/10/2")

# Price
rent$price = gsub("～.*", "", rent$price)
rent$price = str_remove(rent$price, "万円")


# Deposits
rent$deposits = str_remove(rent$deposits, "万円")
rent$deposits = str_replace(rent$deposits, "なし", "0")


# Renewal
rent$renewal = rent$renewal %>% 
  str_remove("万円") %>% 
  str_replace("なし", "0")

# Key
rent$key = rent$key %>% 
  str_remove("万円") %>% 
  str_replace("なし", "0")

# area
rent$area = gsub("m2／.*", "", rent$area)
rent$area = gsub(".*／", "", rent$area)
rent$area = gsub("～.*", "", rent$area)
rent$area = str_remove(rent$area, "m2")

# maintain

rent$maintainance = rent$maintainance %>% 
  str_remove("\n") %>% 
  str_remove("0円～") %>% 
  str_remove("円") %>% 
  str_remove("\\,")








View(rent)
