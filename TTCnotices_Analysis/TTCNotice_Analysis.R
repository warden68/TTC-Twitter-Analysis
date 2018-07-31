# Install and /or load the gsubfn package
library(gsubfn)
library(stringr)
library(tm)
library(ggplot2)
library(plyr)
library(dplyr)

# Load the necessary table into R
first <- read.csv2("C:/Users/Stephen/Desktop/CKME 136/TTC_Notices_Analysis/TTCNotice_Analysis_test5.csv")
TTC_station <- read.csv("C:/Users/Stephen/Desktop/CKME 136/TTC_Notices_Analysis/Data/CSV - TTC Station List.csv")

# Normalize the tweet data (Change to lower text and remove some stopwords)
first$text <- tolower(first$text)
TTC_station$name <- tolower(TTC_station$name)
first$text <- gsub(" a | an | the "," ",first$text)
first$date.of.tweet <- as.Date(first$date.of.tweet,format='%d/%m/%Y')

# Normalize some names of subway station for easier analysis
first$text <- gsub("st clair west","stclairwest",first$text)
first$text <- gsub("dundas west","dundaswest",first$text)
first$text <- gsub("eglinton west","eglintonwest",first$text)
first$text <- gsub("lawrence east","lawrenceeast",first$text)
first$text <- gsub("lawrence west","lawrencewest",first$text)
first$text <- gsub("sheppard west","sheppardwest",first$text)
first$text <- gsub("finch west", "finchwest", first$text)

TTC_station$name <- gsub("st clair west","stclairwest",TTC_station$name)
TTC_station$name <- gsub("dundas west","dundaswest",TTC_station$name)
TTC_station$name <- gsub("eglinton west","eglintonwest",TTC_station$name)
TTC_station$name <- gsub("lawrence east","lawrenceeast",TTC_station$name)
TTC_station$name <- gsub("lawrence west","lawrencewest",TTC_station$name)
TTC_station$name <- gsub("sheppard west","sheppardwest",TTC_station$name)
TTC_station$name <- gsub("finch west", "finchwest", TTC_station$name)

# Remove numbers for non bus route for easier extraction of route numbers
first$text <- gsub("highway 27|highway 401|highway 7|highway 400","highway",first$text)
first$text <- gsub("10k","Ten KM",first$text)
first$text <- gsub("\\d+ am|\\d+am","am",first$text)
first$text <- gsub("\\d+ pm|\\d+pm","pm",first$text)
first$text <- gsub("\\d+ minute|\\d+ minutes","minute",first$text)
first$text <- gsub("\\d+th|\\d+nd|\\d+rd","road name",first$text)

# Remove hashtags
first$text <- gsub("# ttc","", first$text)

# Remove the web link (image/URL)
first$text <- gsub("info.pic.twitter.\\S+","picture", first$text)
first$text <- gsub("pic.twitter.\\S+","picture", first$text)
first$text <- gsub("http\\S+","URL", first$text)
first$text <- gsub("https\\S+","URL", first$text)

# Remove puncutation
first$text <- gsub(",","", first$text)
first$text <- gsub(":","", first$text)

# Create 3 columns for month/day/hour of tweets
first$month.of.tweet <- as.integer(format(first$date.of.tweet,"%m"))
first$day.of.tweet <- as.integer(format(first$date.of.tweet,"%d"))
first$hour.of.tweet <- as.integer(substr(first$time.of.tweet,1,2))

# Determine the period of time disruption happened
# Weekday: 0:00-5:59: "Late night", 6:00-9:59 "Morning Rush", 10:00-15:59 "Midday", 16:00-19:59 "Evening Rush", 20:00-23:59 "Evening"
# Weekend and holiday: 0:00-5:59: "Late night", 6:00-11:59 "Morning", 12:00-18:59 "Midday", 18-23:59 "Evening"
first$period.of.tweet = NA

for (i in 1:nrow(first))
{
  if (first$day.of.week[i] == "Saturday" | first$day.of.week[i] == "Sun&Holiday")
    {
      if (first$hour.of.tweet[i] >= 0 & first$hour.of.tweet[i] <= 5)
      {first$period.of.tweet[i] = "Late night"}
    
      if (first$hour.of.tweet[i] >= 6 & first$hour.of.tweet[i] <= 11)
      {first$period.of.tweet[i] = "Morning"}
    
      if (first$hour.of.tweet[i] >= 12 & first$hour.of.tweet[i] <= 18)
      {first$period.of.tweet[i] = "Midday"}
    
      if (first$hour.of.tweet[i] >= 19 & first$hour.of.tweet[i] <= 23)
      {first$period.of.tweet[i] = "Evening"}
    }
  
  else
  {
    if (first$hour.of.tweet[i] >= 0 & first$hour.of.tweet[i] <= 5)
    {first$period.of.tweet[i] = "Late night"}
    
    if (first$hour.of.tweet[i] >= 6 & first$hour.of.tweet[i] <= 9)
    {first$period.of.tweet[i] = "Morning Rush"}
    
    if (first$hour.of.tweet[i] >= 10 & first$hour.of.tweet[i] <= 15)
    {first$period.of.tweet[i] = "Midday"}
    
    if (first$hour.of.tweet[i] >= 16 & first$hour.of.tweet[i] <= 19)
    {first$period.of.tweet[i] = "Evening Rush"}
    
    if (first$hour.of.tweet[i] >= 20 & first$hour.of.tweet[i] <= 23)
    {first$period.of.tweet[i] = "Evening"}
  }
}

# Identifying the bus/streetcar/subway route that got service disrupted, or where elevator broke down
first$route <- NA

for (i in  1:nrow(first))
{
  if (grepl("elevator", first$text[i]))
  { first$route[i] <- str_extract(first$text[i], paste(TTC_station$name, collapse="|"))}
  
  else if (grepl("line 1|yu", first$text[i]))
  { first$route[i] <- "line 1"}

  else if (grepl("line 2|bd", first$text[i]))
  { first$route[i] <- "line 2"}
  
  else if (grepl("line 3|srt", first$text[i]))
  { first$route[i] <- "line 3"}
  
  else if (grepl("line 4|shp", first$text[i]))
  { first$route[i] <- "line 4"}
  
  else if (grepl("[0-9]+", first$text[i]))
  { first$route[i] <-  str_extract_all(first$text[i], "[0-9]+\\d*")}
  
  else if (grepl("southbound|northbound|sb|nb", first$text[i]))
  { first$route[i] <- "line 1"}
  
  else if (grepl("westbound|eastbound|eb|wb", first$text[i]))
  { first$route[i] <- "line 2"}
}

# Since some textes involve multiple routes we want to unlist all of them for individual counting
# Therefore new data frame / table is created with all routes unlisted

second <- data.frame("month.of.tweet" = as.integer(first$month.of.tweet[1]), "day.of.tweet" = as.integer(first$day.of.tweet[1]),
                     "day.of.week" = first$day.of.week[1], "hour.of.tweet" = as.integer(first$hour.of.tweet[1]), 
                     "period.of.tweet" = first$period.of.tweet[1],"text" = first$text[1],
                     "route" = as.character(unlist(first$route[1][[1]])), stringsAsFactors = FALSE)

for (i in 2:nrow(first))
{
      for (j in 1:length(first$route[i][[1]]))
      {second[nrow(second)+1,] <- list (first$month.of.tweet[i], first$day.of.tweet[i], first$day.of.week[i],
                                  first$hour.of.tweet[i], first$period.of.tweet[i], first$text[i],
                                  as.character(first$route[i][[1]][j]))}
}

# Identifying whether the extracted word is a bus, subway or streetcar route or a subway station
second$routetype <- NA

for (i in  1:nrow(second))
{
  if (grepl("line 1|line 2|line 3|line 4", second$route[i]))
  { second$routetype[i] <- "subway"}
  
  else if (grepl("501|502|503|504|505|506|509|510|511|512|514", second$route[i]))
  { second$routetype[i] <- "streetcar"}
  
  else if (grepl("3[0-9]{2,}", second$route[i]))
  { second$routetype[i] <- "bus"}
  
  else if (grepl("185|186|188|190|191|192|193|195|198|199", second$route[i]))
  { second$routetype[i] <- "bus"}
  
  else if (grepl("141|142|143|144|145", second$route[i]))
  { second$routetype[i] <- "bus"}
  
  else if (grepl("[a-z]+", second$route[i]))
  { second$routetype[i] <- "elevator"}
  
  else
  { second$routetype[i] <- "bus"}
}


# Extract the type of surface disruption

second$disrupt <- NA

for (i in  1:nrow(second))
{
  if (grepl("elevator", second$text[i]))
  {second$disrupt[i] <- "elevator down"}
  
  else if (grepl(".*detour.*|.*diver.*", second$text[i]))
  {second$disrupt[i] <- "detour"}
  
  else if (grepl(".*delay.*|.*longer than normal|slower than normal.*|.*hold.*", second$text[i]))
  {second$disrupt[i] <- "delay"}
  
  else if (grepl(".*bypass.*|.*not stop.*", second$text[i]))
  {second$disrupt[i] <- "bypass"}
  
  else if (grepl(".*close.*", second$text[i]))
  {second$disrupt[i] <- "no service"}
  
  else if (grepl(".*suspen.*",second$text[i]))
  {second$disrupt[i] <- "no service"}
  
  else if (grepl(".*no.*service.*|.*turn.*", second$text[i]))
  {second$disrupt[i] <- "no service"}
}

# Extract reasons of surface disruption
second$disrupt_reason <- NA

for (i in  1:nrow(second))
{
  if (grepl("due", second$text[i]))
  {second$disrupt_reason[i] <- strapplyc(second$text[i], "due ([^.]+)|due ([$]+)", simplify = TRUE)}
  
  else if (grepl(".* with .*", second$text[i]))
  {second$disrupt_reason[i] <- strapplyc(second$text[i], "with ([^.]+)|with ([$]+)", simplify = TRUE)}
  
  else if (grepl("while we fix", second$text[i]))
  {second$disrupt_reason[i] <- strapplyc(second$text[i], "while we fix ([^.]+)|while we fix ([$]+)", simplify = TRUE)}
  
  else if (grepl("as result of", second$text[i]))
  {second$disrupt_reason[i] <- strapplyc(second$text[i], "as result of ([^.^,]+)|as result of ([$]+)", simplify = TRUE)}
  
  else if (grepl(".* for .*", second$text[i]))
  {second$disrupt_reason[i] <- strapplyc(second$text[i], "for ([^.]+)|for ([$]+)", simplify = TRUE)}
  
  else if (grepl("while we respond to", second$text[i]))
  {second$disrupt_reason[i] <- strapplyc(second$text[i], "while we respond to ([^.]+)|while we respond to ([$]+)", simplify = TRUE)}
}

# Catergorize reasons of surface disruption into 1 of 18 groups
second$reasongroup <- NA
for (i in  1:nrow(second))
{
  if (grepl("collision|collide", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "collision on the road"}
  
  if (grepl("stalled streetcar", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "stalled streetcar"}
  
  else if (grepl("construction|crane|late clearing|speed zone|generator lift|gas line relocation|planned roadway closure", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "construction"}
  
  else if (grepl("watermain|water main|sewer|sink hole|water repairs|road repair|roadway repairs|drain installation|pot hole|pole down|road sinking|emergency repairs|hydro|maintenance trucks", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "watermain/sewer/road repairs"}
  
  else if (grepl("block.* |falling debris| rail|disabled| downed|stalled| tree.* |congestion.*|fallen| auto |stuck|spill|go transit equipment issues|poles down|heavy traffic|road clearance issues|traffic lights|debris on roadway", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "blockage on the road/rail"}
  
  else if (grepl("fire|tfs", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "fire"}
  
  else if (grepl("police|tps|security|investigation|disorderly|disruptive customer", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "police/security"}
  
  else if (grepl("mechanical|door problem|brake problem|frozen switch|equipment issues|frozen track switch", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "mechanical"}
  
  else if (grepl(".*medical.*|ill customer|injury on platform|emergency situation", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "medical"}
  
  else if (grepl("emergency alarm|emergency .* alarm", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "emergency alarm"}
  
  else if (grepl("trespasser|injury at track|injury on track|unauthorized person|unauthorized customer", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "person on track"}
  
  else if (grepl(" overhead| wire| power|power off", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "power problem"}
  
  else if (grepl("track.*", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "track problem/maintainance"}
  
  else if (grepl("signal|delay leaving yard", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "signal problem"}
  
  else if (grepl("earlier delay.*|early delay.*", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "earlier delay"}
  
  else if (grepl(" blue jays|game|games|exhibition| event.*|parade|vigil| marathon| rogers|labor|labour|protest| race| cne| strike| festival| run|movie shoot|film shoot|estate sale|crowds|picketers", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "activities/protest"}
  
  else if (grepl("road closure.*", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "road closure"}
  
  else if (grepl("poor road|icy|slippery|snow|flooding|road conditions|roadway conditions|inclement weather|gas leak", second$disrupt_reason[i]))
  {second$reasongroup[i] <- "poor road conditions"}
}

# Plot counts of service disruption counts from 2018.01-2018.06 (by hour)
hour_subset <- subset(second, second$routetype != "elevator")
hour_count_table <- count(hour_subset, c("hour.of.tweet","routetype"))
p_hour <- ggplot(hour_count_table, aes(x = hour.of.tweet, y = freq, fill = routetype)) + geom_bar(stat = "identity") 
p_hour <- p_hour + ggtitle("Total service disruption counts of TTC from 2018.01-2018.06") + xlab("Hour") + ylab("Count")
p_hour <- p_hour + theme(axis.title.x = element_text(size=16)) + theme(axis.title.y = element_text(size=16)) + theme(text = element_text(size=20))
p_hour

# Plot elevator maintainance count for TTC stations (2018.01-2018.06)
subway_subset <- subset(second, second$routetype == "elevator")
elevator_table <- table(subway_subset$route)
elevator_top20 <- head(sort(elevator_table, decreasing=TRUE), n=20)
elevator_top20_plot <- data.frame(Station = names(elevator_top20), Count = as.vector(elevator_top20))

p_elevator <- ggplot(elevator_top20_plot, aes(x = reorder(Station, -Count), y = Count)) + geom_bar(stat = "identity")
p_elevator <- p_elevator + ggtitle("Elevator maintainance Counts in TTC stations from 2018.01-2018.06") + xlab("Station Name") + ylab("Maintainace Count")
p_elevator <- p_elevator + theme(axis.title.x = element_text(size=16))
p_elevator <- p_elevator + theme(axis.title.x = element_text(size=16))
p_elevator <- p_elevator + theme(text = element_text(size=25),axis.text.x = element_text(angle=90, hjust=1))
p_elevator

# Count service disruption by bus route (2018.01-2018.06)
bus_subset <- subset(second, second$routetype == "bus")
bus_table <- table(bus_subset$route)
bus_top15 <- head(sort(bus_table, decreasing=TRUE), n=15)
bus_top15_plot <- data.frame(Routeno = names(bus_top15), Count = as.vector(bus_top15))

p_bus <- ggplot(bus_top15_plot, aes(x = reorder(Routeno, -Count), y = Count)) + geom_bar(stat = "identity")
p_bus <- p_bus + ggtitle("Service disruption counts for TTC bus routes from 2018.01-2018.06") + xlab("Route Number") + ylab("Frequency")
p_bus <- p_bus + theme(axis.title.x = element_text(size=16))
p_bus <- p_bus + theme(axis.title.x = element_text(size=16))
p_bus <- p_bus + theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1))
p_bus

# Plot a pie chart to see the distribution of reasons of bus service disruption
bus_disrupt_table <- table(bus_subset$reasongroup)
bus_pie_label <- names(bus_disrupt_table)
bus_pie_count <- as.vector(bus_disrupt_table)
bus_pie_count_percentage <- round(bus_pie_count/sum(bus_pie_count)*100)
bus_pie_label  <- paste(bus_pie_label, bus_pie_count_percentage) # add percents to labels 
bus_pie_label  <- paste(bus_pie_label ,"%",sep="") # ad % to labels 
pie(bus_pie_count,labels = bus_pie_label, col=rainbow(length(bus_pie_label)),
    main="Pie Chart for reasons of service disruption for bus 2018.01 - 2018.06")

# Count service disruption by streetcar route (2018.01-2018.06)
streetcar_subset <- subset(second, second$routetype == "streetcar")
streetcar_table <- table(streetcar_subset$route)
streetcar_table_plot <- data.frame(Routeno = names(streetcar_table), Count = as.vector(streetcar_table))

p_streetcar <- ggplot(streetcar_table_plot, aes(x = reorder(Routeno, -Count), y = Count, fill = Routeno)) + geom_bar(stat = "identity")
p_streetcar <- p_streetcar + ggtitle("Service disruption counts for TTC streetcar routes from 2018.01-2018.06") + xlab("Route Number") + ylab("Frequency")
p_streetcar <- p_streetcar + theme(axis.title.x = element_text(size=16))
p_streetcar <- p_streetcar + theme(axis.title.x = element_text(size=16))
p_streetcar <- p_streetcar + theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1))
p_streetcar

# Plot a pie chart to see the distribution of reasons of street service disruption
streetcar_disrupt_table <- table(streetcar_subset$reasongroup)
streetcar_pie_label <- names(streetcar_disrupt_table)
streetcar_pie_count <- as.vector(streetcar_disrupt_table)
streetcar_pie_count_percentage <- round(streetcar_pie_count/sum(streetcar_pie_count)*100)
streetcar_pie_label  <- paste(streetcar_pie_label, streetcar_pie_count_percentage) # add percents to labels 
streetcar_pie_label  <- paste(streetcar_pie_label ,"%",sep="") # ad % to labels 
pie(streetcar_pie_count,labels = streetcar_pie_label, col=rainbow(length(streetcar_pie_label)),
    main="Pie Chart for reasons of service disruption for streetcar 2018.01 - 2018.06")

# Count service disruption by subway route (2018.01-2018.06)
subway_subset <- subset(second, second$routetype == "subway")
subway_table <- table(subway_subset$route)
subway_table_plot <- data.frame(Routeno = names(subway_table), Count = as.vector(subway_table))

p_subway <- ggplot(subway_table_plot, aes(x = reorder(Routeno, -Count), y = Count, fill = Routeno)) + geom_bar(stat = "identity")
p_subway <- p_subway + ggtitle("Service disruption counts for TTC subway routes from 2018.01-2018.06") + xlab("Subway Line") + ylab("Frequency")
p_subway <- p_subway + theme(axis.title.x = element_text(size=16))
p_subway <- p_subway + theme(axis.title.x = element_text(size=16))
p_subway <- p_subway + theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1))
p_subway

# Plot a pie chart to see the distribution of reasons of subway service disruption
subway_disrupt_table <- table(subway_subset$reasongroup)
subway_pie_label <- names(subway_disrupt_table)
subway_pie_count <- as.vector(subway_disrupt_table)
subway_pie_count_percentage <- round(subway_pie_count/sum(subway_pie_count)*100)
subway_pie_label  <- paste(subway_pie_label, subway_pie_count_percentage) # add percents to labels 
subway_pie_label  <- paste(subway_pie_label ,"%",sep="") # ad % to labels 
pie(subway_pie_count,labels = subway_pie_label, col=rainbow(length(subway_pie_label)),
    main="Pie Chart for reasons of service disruption for subway 2018.01 - 2018.06")

# Plot the data of 501, 504 and 514, both 2017 (Jan-Jun) and 2018 (Jan-Jun) for each route
# King's pilot started from Nov 2017, by comparing the data
# We can visualize if the project is a success or not
king_queen_compare <- data.frame(Year = "2017 (Jan-Jun)", Route = "501", Count = 185,stringsAsFactors = FALSE)
king_queen_compare <- rbind(king_queen_compare, c("2017 (Jan-Jun)", "504", 220))
king_queen_compare <- rbind(king_queen_compare, c("2017 (Jan-Jun)", "514", 69))
king_queen_compare <- rbind(king_queen_compare, c("2018 (Jan-Jun)", "501", 314))
king_queen_compare <- rbind(king_queen_compare, c("2018 (Jan-Jun)", "504", 205))
king_queen_compare <- rbind(king_queen_compare, c("2018 (Jan-Jun)", "514", 86))
king_queen_compare <-transform(king_queen_compare, Count = as.numeric(Count))
str(king_queen_compare)

p_kq <- ggplot(king_queen_compare, aes(x = Route, y = Count)) + geom_bar(stat = "identity", aes(fill = Year), position = "dodge")
p_kq <- p_kq + scale_fill_manual(values=c("#E69F00", "#56B4E9")) + ggtitle("Service disruption on King Street route before and after the King's Pilot Project")
p_kq <- p_kq + theme(axis.title.x = element_text(size=16)) + theme(axis.title.y = element_text(size=16)) + theme(text = element_text(size=20))
p_kq

# Output the result
output <- as.matrix(second)
write.csv2(output, file = "C:/Users/Stephen/Desktop/TTCNotice_Analysis_Result.csv")