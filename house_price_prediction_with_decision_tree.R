#library
library(dplyr)
library(rpart)
library(rpart.plot)

#Pemanggilan Data
rawdata <- read.csv("Housepricepred.csv")
head(rawdata)

str(rawdata)
rawdata %>% summarise_all(n_distinct)

# Date: Tanggal Terjual
# Price: Harga Rumah
# Bedrooms: Jumlah Kamar Tidur
# Bathrooms: Jumlah Kamar Mandi
# Sqft_Living: square footage of the home
# Sqft_Lot: square footage of the lot
# Floors: Total lantai (tingkat) di rumah
# Waterfront: Rumah dengan view menghadap pantai
# View: Telah dilihat
# Condition: Seberapa baik kondisinya (Secara keseluruhan)
# Sqft_Above: square footage of house apart from basement
# Sqft_Basement: square footage of the basement
# Yr_Built: Tahun Dibangun
# Yr_Renovated: Tahun ketika rumah direnovasi
# Zipcode: Kode Pos
# street : Jalan
# City : Kota
# Country : Negara



#DATA PREPROCESSING
#1. Data Cleaning
#a. menghilangkan outlier
sum(is.na(rawdata))

#menambahkan kolom n
rawdata["no"] <- c(1:4600)
str(rawdata)
head(rawdata)
#kolom price
options(scipen=999)
dev.new(width=500, height=500)
plot(rawdata$no, rawdata$price)
#menghilangkan outlier diatas 5.000.000
rawdata <- subset(rawdata, price <= 5000000)

#kolom sqft_lot
plot(rawdata$no, rawdata$sqft_lot)
#menghilangkan outlier diatas 600.000
rawdata <- subset(rawdata, sqft_lot <= 600000)

#kolom sqft_living
plot(rawdata$no, rawdata$sqft_living)
#menghilangkan outlier diatas 5.000.000
rawdata <- subset(rawdata, sqft_living <= 12000)


#b. mengatasi missing value kolom price (harga 0)
rawdata <- subset(rawdata, price > 0)

#c. Data Reduction
names(rawdata)
rawdata <- rawdata[-c(1,14,15,16,17,18,19)]
head(rawdata)

#d. Transformation
str(rawdata)

rawdata$floors <- as.factor(rawdata$floors)
rawdata$waterfront <- as.factor(rawdata$waterfront)
rawdata$condition <- as.factor(rawdata$condition)
rawdata$view <- as.factor(rawdata$view)

# Data Discretization
rawdata$price[rawdata$price < mean(rawdata$price)] <- 0
rawdata$price[rawdata$price >= mean(rawdata$price)] <- 1
rawdata$price <- as.factor(rawdata$price)

# PROCESSING
#membagi data
house <- rawdata
head(house)
str(house)
set.seed(1234)
ind <- sample(2, nrow(house), replace=TRUE, prob=c(0.7, 0.3))
house.train <- house[ind==1,]
house.test <- house[ind==2,]
str(house.train)

# train a decision tree
names(house)
house_rpart <- rpart(price~., data = house.train)

attributes(house_rpart)
print(house_rpart)
plot(house_rpart)
text(house_rpart)
rpart.plot(house_rpart)

opt <- which.min(house_rpart$cptable[,"xerror"])
cp <- house_rpart$cptable[opt, "CP"]
print(house_rpart$cptable)
plotcp(house_rpart)
house_prune <- prune(house_rpart, cp = cp)
print(house_prune)
plot(house_prune)
text(house_prune, use.n=T)
rpart.plot(house_prune)

# prediction
house_pred <- predict(house_prune, newdata=house.test, type="class")

#confussion Matrix
table <- table(house_pred, house.test$price)
table
accuracy <- (table[1,1]+table[2,2])/(table[1,1]+table[2,2]+table[2,1]+table[1,2])  
accuracy

precision <- table[1,1]/(table[1,1]+table[2,1])
precision

recall <- table[1,1]/(table[1,1]+table[1,2])
recall

fmeasures <- (2*(precision)*(recall))/(precision+recall)
fmeasures

