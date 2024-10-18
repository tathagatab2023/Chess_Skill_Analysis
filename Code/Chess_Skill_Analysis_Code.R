#Loading libraries
library(readxl)
library(rvest)

#Creating database from raw data
data_base = matrix(0,nrow=1e4,ncol=10)
data_base = as.data.frame(data_base)

colnames(data_base) = c("Tournament","White_Name","White_Age","White_Rating","White_Title","Black_Name","Black_Age","Black_Rating","Black_Title","Result_White")


d = read_excel("Tata_Masters_22.xlsx")

d = d[-(1:4),-c(1,2,10)]
d = na.omit(d)

data_base[365:455,1] = "TSM_22"
data_base[365:455,c(2,4,5,6,8,9)] = d[,c(3,1,2,6,7,5)]

foo = character(length=nrow(d))
for(i in 1:nrow(d))
{
  foo[i] = as.numeric(strsplit(unlist(d[,4]),"-")[[i]][1])
}
foo = as.numeric(ifelse(is.na(foo),"0.5",foo))

data_base[365:455,10] = foo

d1 = data_base[1:91,]
d2 = data_base[92:182,]
d3 = data_base[183:273,] 
d4 = data_base[274:364,]
d5 = data_base[365:455,]

players = c(unique(d1$White_Name),unique(d2$White_Name),unique(d3$White_Name),unique(d4$White_Name),unique(d5$White_Name))
year = c(rep(2024,28),rep(2023,28),rep(2022,14))

bday = c(1999,2000,1991,1994,1998,2006,
         2005,2003,2004,1999,1992,1994,
         1990,2000,2003,1992,1993,2005,
         2006,2007,2005,2002,1991,2006,
         1996,1985,1996,1985,1999,1996,
         1992,2006,2000,1990,2005,1993,
         1982,2004,1992,1994,2004,2003,
         1992,2009,1985,2000,2001,1998,
         1998,2002,2001,1992,2001,1996,
         2006,2005,1994,1992,1996,2002,
         1994,1998,1999,1991,1996,2005,
         1990,1985,1990,1993)

age = year - bday
age_data = cbind(players,bday)

for(i in 57:70)
{
  d5[which(d5$White_Name == players[i]),3] = age[i]
  d5[which(d5$Black_Name == players[i]),7] = age[i]
}


data_base[1:455,] = rbind(d1,d2,d3,d4,d5)


url <- "https://worldcup2023.fide.com/en/players"

webpage <- read_html(url)

first_table <- webpage %>% html_nodes("table") %>% html_table()

d1 = first_table[,1:3]
d1_filtered <- d1[!d1[[2]] == "", ]

d1_filtered = na.omit(d1_filtered)

data = d1_filtered[,1]
data = c(data)


rating <- c()
name <- c()
for (element in data) {
  # Separate the first 6 characters in each element
  rating <- c(rating, substr(element, start = 2, stop = 5))
  name = c(name,substr(element,start=9,stop=nchar(element)))
}

new = cbind(name,rating,d1_filtered[,2:3])

new1 = matrix(0,nrow=(nrow(new)/2),ncol = 5) 
new1 = as.data.frame(new1)


for(i in seq(1,(nrow(new) - 1),2))
{
  new1[(i+1),] = unlist(c(new[i,1:2],new[(i+1),1:2],new[i,3]))
  new1[i,] = unlist(c(new[(i+1),1:2],new[i,1:2],new[(i+1),4]))
}

fwc[405:408,c(2,4,6,8,10)] = new1
fwc[409,c(2,4,6,8,10)] = c("Carlsen Magnus",2835,"Praggnanandhaa R",2690,0.5)
fwc[410,c(2,4,6,8,10)] = c("Praggnanandhaa R",2690,"Carlsen Magnus",2835,0.5)
fwc[411,c(2,4,6,8,10)] = c("Abasov Nijat",2632,"Caruana Fabiano",2782,1)
fwc[412,c(2,4,6,8,10)] = c("Caruana Fabiano",2782,"Abasov Nijat",2632,1)


data_base = data_base[,-1]

fwc23 = matrix(0,nrow=length(unique(fwc$White_Name)),ncol=3)
fwc23[,1] = unique(fwc$White_Name)

links = paste0("https://en.wikipedia.org/wiki/",fwc23[,1])
links = gsub(" ", "_", links)

webpage = read_html(links[29])
first_table <- html_nodes(webpage, "table")[[1]] %>% html_table()

h6_elements <- html_nodes(webpage, "h6")

# Extract the text from the <h6> elements
h6_text <- html_text(h6_elements)[1:206]

text <- webpage %>%
  html_nodes("span") %>%
  html_text()
text = text[-(1:8)]
text = text[1:824]

byear = text[seq(1,824,4)]
title = text[seq(4,824,4)]
age = 2023 - as.numeric(byear)

for(i in 1:206)
{
  fwc[which(fwc$White_Name == h6_text[i]),3] = age[i]
  fwc[which(fwc$White_Name == h6_text[i]),5] = title[i]
  fwc[which(fwc$Black_Name == h6_text[i]),7] = age[i]
  fwc[which(fwc$Black_Name == h6_text[i]),9] = title[i]
}
data_base[456:867,] = fwc

d = readLines("Fi_2021.txt")
d = d[which(d != " ")]
d = d[-(1:13)]

elements = paste0("#",1)

index = numeric(length=length(elements))
for(i in 1:length(elements))
{
  index[i] = which(d == elements[i])
}

d1 = c()
for(i in index)
{
  d1 = c(d1,d[(i+1):(i+4)])
}

d1 = matrix(d1,ncol=4,byrow=T)
d2 = matrix(0,nrow=2*nrow(d1),ncol=4)

for(i in 1:nrow(d1))
{
  d2[(2*i - 1),] = d1[i,]
  d2[(2*i),] = d1[i,]
}

new = as.vector(rbind(d2[,3],d2[,4]))

vec = unlist(strsplit(new,":"))

d3 = matrix(0,nrow=nrow(d2),ncol=3)


for(i in 1:nrow(d2))
{
  if(i %% 2 == 1)
  {
    d3[i,1:2] = d2[i,1:2]
  }else
  {
    d3[i,1:2] =c(d2[i,2],d2[i,1])
  }
}

vec <- gsub("\\s+", "", vec)
vec = matrix(vec,ncol=4,byrow=T)


interim = cbind(head(d3,20)[,-3],vec[1:20,])

for(i in 1:nrow(vec))
{
  if(i %% 4 == 1)
  {
    d3[i,3] = vec[i,3]
  }else if(i %% 4 == 2)
  {
    d3[i,3] = vec[i,2]
  }else if(i %% 4 == 3)
  {
    d3[i,3] = vec[i,1]
  }else if(i %% 4 == 0)
  {
    d3[i,3] = vec[i,4]
  }
}

fwc21[401:408,c(2,6,10)] = d3

fwc21[which(fwc21[,10] == "½"),10] = 0.5
fwc21[409,c(2,6,10)] = c("Duda, Jan-Krzysztof","Karjakin, Sergey",1)
fwc21[410,c(6,2,10)] = c("Duda, Jan-Krzysztof","Karjakin, Sergey",0.5)
fwc21[411,c(2,6,10)] = c("Fedoseev, Vladimir","Carlsen, Magnus",0)
fwc21[412,c(6,2,10)] = c("Carlsen, Magnus","Fedoseev, Vladimir",1)

data_base = read.csv("Chess_Database_Classical.csv")[,-1]

temp = readLines("ranking_2021.txt")[15:632]

players = temp[seq(1,618,3)]
ratings = temp[seq(2,617,3)]

for(i in 1:length(players))
{
  fwc21[which(fwc21$Black_Name == players[i]),8] = ratings[i]
  fwc21[which(fwc21$White_Name == players[i]),4] = ratings[i]
}

fwc21 = fwc21[1:412,]
data_base[868:1279, ] = fwc21
colnames(fwc21) = colnames(data_base)

d = data_base

temp = read.csv("temp.csv")
age21 = 2021-temp$byear
fwc21 = d[868:1279,-1]
temp$players <- gsub("\\s+", "", temp$players)

for(i in 1:length(temp$players))
{
  fwc21[which(fwc21$Black_Name == temp$players[i]),9] = temp$title[i]
  fwc21[which(fwc21$Black_Name == temp$players[i]),7] = age21[i]
  fwc21[which(fwc21$White_Name == temp$players[i]),3] = age21[i]
  fwc21[which(fwc21$White_Name == temp$players[i]),5] = temp$title[i]
}

data_base= d[,-1]
data_base[868:1279,]=fwc21

data_base = read.csv("Chess_Database_Classical.csv")
d = data_base[1:1274,-1]

#Transforming data for easier interpretation
d[,3] = d[,3]/80
d[,7] = d[,7]/80
d[,4] = d[,4]/3000
d[,8] = d[,8]/3000

White_Age_Squared = d[,3]^2
Black_Age_Squared = d[,7]^2
White_Rating_Squared = d[,4]^2
Black_Rating_Squared = d[,8]^2
White_Age_Rating_CP = d[,3]*d[,4]
Black_Age_Rating_CP = d[,7]*d[,8]

new = cbind(d,White_Age_Squared,White_Rating_Squared,White_Age_Rating_CP,Black_Age_Squared,Black_Rating_Squared,Black_Age_Rating_CP)

d = read.csv("Chess_Database_Classical.csv")[,-1]
levels(d$Result_White) = c("1","0.5","0")
library(nnet)
test <- multinom(Result_White ~ White_Age +White_Rating + Black_Age + Black_Rating, data = d)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
t = summary(test)$coefficients

alpha = t[,1]
wa = t[,2]
wr = t[,3]
ba = t[,4]
br = t[,5]

age_w = 25
age_b = 25
rating_w = 2700
rating_b = 2000

probab = function(age_w,age_b,rating_w,rating_b)
{
  age_w = age_w/6.5
  age_b = age_b/6.5
  rating_w = (rating_w-2000)/100
  rating_b = (rating_b - 2000)/100
  
  den = 1 + exp(alpha[1] + wa[1]*age_w + wr[1]*rating_w + ba[1]*age_b + br[1]*rating_b) + exp(alpha[2] + wa[2]*age_w + wr[2]*rating_w + ba[2]*age_b + br[2]*rating_b)
  num = numeric(3)
  num[3] = 1
  num[2] = exp(alpha[1] + wa[1]*age_w + wr[1]*rating_w + ba[1]*age_b + br[1]*rating_b)
  num[1] = exp(alpha[2] + wa[2]*age_w + wr[2]*rating_w + ba[2]*age_b + br[2]*rating_b)
  prob = num/den
  return(prob)
}

#Finding fitted probabilities
check = matrix(0,nrow=64,ncol=6)
check[,1] = c(rep(20,32),rep(35,32))
check[,2] = c(rep(20,16),rep(35,16),rep(20,16),rep(35,16))
check[,3] = rep(c(rep(2500,4),rep(2600,4),rep(2700,4),rep(2800,4)),4)
check[,4] = rep(c(2500,2600,2700,2800),16)

for(i in 1:64)
{
  check[i,5:6] = probab(check[i,1],check[i,2],check[i,3],check[i,4])[1:2]
}
age_w = d[,3]*6.5
age_b = d[,7]*6.5
rating_w = d[,4]*100 + 2000
rating_b = d[,8]*100 + 2000
new1 = matrix(0,nrow=nrow(d),ncol=3)
for(i in 1:nrow(d))
{
  new1[i,] = probab(age_w[i],age_b[i],rating_w[i],rating_b[i])-new[i,]
}


new = cbind(fitted(test)[,c(3,2,1)])

levels(d$Result_White) = c("1","0.5","0")
library(VGAM)
fitnew <- vglm(Result_White~White_Age + White_Rating + Black_Age + Black_Rating,data=d,family=multinomial)

check = cbind(fitted(test),d$White_Age,d$White_Rating,d$Black_Age,d$Black_Rating)

#Finding class differences
c = 0
c1 = 0
for(i in 1:1274)
{
  if(d$Result_White[i] == 1 && d$White_Age[i] < d$Black_Age[i] && d$White_Rating[i] > d$Black_Rating[i])
  {
    c = c + 1
  }else if(d$Result_White[i] == 0 && d$White_Age[i] < d$Black_Age[i] && d$White_Rating[i] > d$Black_Rating[i])
  {
    c1 = c1 + 1
  }else  if(d$Result_White[i] == 0.5 && d$White_Age[i] < d$Black_Age[i] && d$White_Rating[i] > d$Black_Rating[i])
  {
    c = c+0.5
    c1 = c1 + 0.5
  }
}
rating_diff = d$White_Rating - d$Black_Rating
age_diff = d$White_Age - d$Black_Age
dn = 0
for(i in 1:1274)
{
  if(rating_diff[i] > 0 && age_diff[i] < 0)
    dn = dn + 1
}




d = read.csv("Chess_Database_Classical.csv")[,-1]

d1 = d[,2:4]

library(ggplot2)
#Plotting for EDA
scatterplot <- ggplot(d1, aes(x = White_Age, y = White_Rating)) +
  geom_point(color = "black", size = 2) + # Customize points
  labs(title = "Scatterplot of Age v/s Rating",      # Title
       x = "Age",                  # X-axis label
       y = "Rating") +                # Y-axis label
  theme_minimal()                           # Minimal theme
print(scatterplot)

histogram1 <- ggplot(d1, aes(x = White_Age)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) + # Customize histogram
  labs(title = "Histogram of Age",      # Title
       x = "Age",                      # X-axis label
       y = "Frequency") +                # Y-axis label
  theme_minimal()                        # Minimal theme

print(histogram1)

histogram2 <- ggplot(d1, aes(x = White_Rating)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) + # Customize histogram
  labs(title = "Histogram of Rating",      # Title
       x = "Rating",                      # X-axis label
       y = "Frequency") +                # Y-axis label
  theme_minimal()                        # Minimal theme

print(histogram2)


#Fitting the final parsimonious prediction models
levels(d$Result_White) = c("1","0.5","0")
library(nnet)
test <- multinom(Result_White ~ White_Age +White_Rating + Black_Age + Black_Rating, data = d)
summary(test)
test1 <- multinom(1-Result_White ~ Black_Age + Black_Rating + White_Age + White_Rating, data=d)
summary(test1)


z <- summary(test)$coefficients/summary(test)$standard.errors
summary(test)$coefficients


levels(d$Result_White) = c("1","0.5","0")
library(VGAM)
fitnew <- vglm(Result_White~White_Age + White_Rating + Black_Age + Black_Rating,data=d,family=multinomial)
summary(fitnew)
summary(test)


