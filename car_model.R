install.packages('MASS')
install.packages('car')
install.packages('Amelia')

library(ggplot2)
library(swirl)
library(titanic)
library(tidyr)
library(dplyr)
library(sqldf)
library(stringr)
library(stats)
library(scales)
library(gridExtra)
library(Amelia)
library(devtools)
library(lasagnar)  
library(tabplot)
library(DataExplorer)
library(plyr)
library(dplyr)
library(car)
library(MASS)



#reading car information
carinfo<-read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)

#checking the details
str(carinfo)

#plotting for missing values and found 0% are missing
missmap(carinfo, main = "Missing values in DF vs observed")

#no blanks
sapply(carinfo, function(x) length(which(x == "")))

#checking for duplicate rows and no rows are duplicated
sum(duplicated(carinfo))


#separating carname into company and model
carinfo <- separate(carinfo, CarName, into = c("Company","Model"),sep = " ", extra = "merge", fill = "right")

#from the dataframe view there are some company having wrong name

carinfo$Company <- factor(gsub("maxda", "mazda", carinfo$Company))
carinfo$Company <- factor(gsub("vw", "volkswagen", carinfo$Company))
carinfo$Company <- factor(gsub("vokswagen", "volkswagen", carinfo$Company))
carinfo$Company <- factor(gsub("porcshce", "porsche", carinfo$Company))
carinfo$Company <- factor(gsub("toyouta", "toyota", carinfo$Company))
carinfo$Company<-factor(gsub("Nissan", "nissan", carinfo$Company))

str(carinfo)
summary(carinfo)

carinfo$symboling <- as.factor(carinfo$symboling)
carinfo$fueltype <- as.factor(carinfo$fueltype)
carinfo$aspiration <- as.factor(carinfo$aspiration)
carinfo$doornumber <- as.factor(carinfo$doornumber)
carinfo$carbody <- as.factor(carinfo$carbody)
carinfo$drivewheel <- as.factor(carinfo$drivewheel)
carinfo$enginelocation <- as.factor(carinfo$enginelocation)
carinfo$enginetype <- as.factor(carinfo$enginetype)
carinfo$cylindernumber <- as.factor(carinfo$cylindernumber)
carinfo$fuelsystem <- as.factor(carinfo$fuelsystem)
carinfo$Company <- as.factor(carinfo$Company)

str(carinfo)

#boxplot of wheelbase to check any outlier and found there are  outliers
ggplot(carinfo, mapping = aes(y = wheelbase, x ='constant'))+geom_boxplot()+labs(x="dummy", y="wheelbase")
quantile(carinfo$wheelbase,seq(0,1,.05))

#removing outlier and replacing the outlier with the min and max value
carinfo$wheelbase[which(carinfo$wheelbase >110)]<- 110

# re-plotting and found no outlier
ggplot(carinfo, mapping = aes(y = wheelbase, x ='constant'))+geom_boxplot()+labs(x="dummy", y="wheelbase")

#boxplot for car length and found some outlier
ggplot(carinfo, mapping = aes(y = carinfo$carlength, x ='constant'))+geom_boxplot()+labs(x="dummy", y="carlength")
quantile(carinfo$carlength,seq(0,1,.01))

#removing outlier and replacing the outlier with the min value
carinfo$carlength[which(carinfo$carlength <155)]<- 155.900
carinfo$carlength[which(carinfo$carlength >202.480)]<- 202.480


#replotting boxplot for car length and not outlier found
ggplot(carinfo, mapping = aes(y = carinfo$carlength, x ='constant'))+geom_boxplot()+labs(x="dummy", y="carlength")

# boxplot for car width
ggplot(carinfo, mapping = aes(y = carinfo$carwidth, x ='constant'))+geom_boxplot()+labs(x="dummy", y="carlength")
quantile(carinfo$carwidth,seq(0,1,.01))
carinfo$carwidth[which(carinfo$carwidth >70.852)]<- 70.852

# replotingg boxplot for car width and found no outliers
ggplot(carinfo, mapping = aes(y = carinfo$carwidth, x ='constant'))+geom_boxplot()+labs(x="dummy", y="carlength")

#boxplot for carheight and found no outliers 
ggplot(carinfo, mapping = aes(y = carinfo$carheight, x ='constant'))+geom_boxplot()+labs(x="dummy", y="carheight")
quantile(carinfo$carheight,seq(0,1,.01))

#boxplot for curbweight and found no outliers 
ggplot(carinfo, mapping = aes(y = carinfo$curbweight, x ='constant'))+geom_boxplot()+labs(x="dummy", y="curbweight")
quantile(carinfo$curbweight,seq(0,1,.01))

#boxplot for enginesize and found no outliers 
ggplot(carinfo, mapping = aes(y = carinfo$enginesize, x ='constant'))+geom_boxplot()+labs(x="dummy", y="enginesize")
quantile(carinfo$enginesize,seq(0,1,.01))
carinfo$enginesize[which(carinfo$enginesize >194.00)]<- 194.00


#boxplot for boreratio and found no outliers 
ggplot(carinfo, mapping = aes(y = carinfo$boreratio, x ='constant'))+geom_boxplot()+labs(x="dummy", y="boreratio")

#boxplot for stroke and found no outliers 
ggplot(carinfo, mapping = aes(y = carinfo$stroke, x ='constant'))+geom_boxplot()+labs(x="dummy", y="stroke")
quantile(carinfo$stroke,seq(0,1,.01))

carinfo$stroke[which(carinfo$stroke >3.6400)]<- 3.6400
carinfo$stroke[which(carinfo$stroke <2.7056)]<- 2.7056

#replotting stroke boxplot and found no outliers


#boxplot for compressionratio and found  outliers 
ggplot(carinfo, mapping = aes(y = carinfo$compressionratio, x ='constant'))+geom_boxplot()+labs(x="dummy", y="compressionratio")
quantile(carinfo$compressionratio,seq(0,1,.01))
carinfo$compressionratio[which(carinfo$compressionratio >=10)]<- 10
carinfo$compressionratio[which(carinfo$compressionratio <=7.5000)]<- 7.5000

#replotting boxplot
ggplot(carinfo, mapping = aes(y = carinfo$compressionratio, x ='constant'))+geom_boxplot()+labs(x="dummy", y="compressionratio")


#boxplot for horsepower and removing outliers
ggplot(carinfo, mapping = aes(y = carinfo$horsepower, x ='constant'))+geom_boxplot()+labs(x="dummy", y="horsepower")
quantile(carinfo$horsepower,seq(0,1,.01))
carinfo$horsepower[which(carinfo$horsepower >=184)]<- 184

#replotting boxplot 
gplot(carinfo, mapping = aes(y = carinfo$horsepower, x ='constant'))+geom_boxplot()+labs(x="dummy", y="horsepower")

#boxplot for peakrpm and removing outliers
ggplot(carinfo, mapping = aes(y = carinfo$peakrpm, x ='constant'))+geom_boxplot()+labs(x="dummy", y="peakrpm")
quantile(carinfo$peakrpm,seq(0,1,.01))
carinfo$peakrpm[which(carinfo$peakrpm >=5980)]<- 5980  

#replotting boxplot for peakrpm 
ggplot(carinfo, mapping = aes(y = carinfo$peakrpm, x ='constant'))+geom_boxplot()+labs(x="dummy", y="peakrpm")


#boxplot for peakrpm and removing outliers
ggplot(carinfo, mapping = aes(y = carinfo$citympg, x ='constant'))+geom_boxplot()+labs(x="dummy", y="citympg")
quantile(carinfo$citympg,seq(0,1,.01))
carinfo$citympg[which(carinfo$citympg >=44.72)]<- 44.72  

#boxplot for citympg and found no outliers
ggplot(carinfo, mapping = aes(y = carinfo$citympg, x ='constant'))+geom_boxplot()+labs(x="dummy", y="citympg")


#boxplot for peakrpm and removing outliers
ggplot(carinfo, mapping = aes(y = carinfo$highwaympg, x ='constant'))+geom_boxplot()+labs(x="dummy", y="highwaympg")
quantile(carinfo$highwaympg,seq(0,1,.01))
carinfo$highwaympg[which(carinfo$highwaympg >=46.92)]<- 46.92


#converting factor with 2 variables into numeric with 2 numbers
levels(carinfo$fueltype)<-c(1,0)
carinfo$fueltype <- as.numeric(carinfo$fueltype)

levels(carinfo$aspiration) <- c(1,0)
carinfo$aspiration <- as.numeric(carinfo$aspiration)

levels(carinfo$doornumber) <- c(4,2)
carinfo$doornumber <- as.numeric(carinfo$doornumber)

levels(carinfo$enginelocation) <- c(1,0)
carinfo$enginelocation <- as.numeric(carinfo$enginelocation)


#converting factors with more then 2 variables into dummy variables
summary(carinfo$symboling)

#Converting "symboling" into dummies . 
dummy_1 <- data.frame(model.matrix( ~symboling, data = carinfo))

#check the dummy_1 data frame.
View(dummy_1)

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "symboling". 
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
carinfo_1 <- cbind(carinfo[,-2], dummy_1)

str(carinfo_1)


#dummy variable for carbody
dummy_1 <- model.matrix(~carbody,data = carinfo_1)
dummy_1 <- dummy_1[,-1]
carinfo_1 <- cbind(carinfo_1[,-7],dummy_1)

#dummy variable for drivewheel
dummy_1 <- model.matrix(~drivewheel,data = carinfo_1)
dummy_1 <- dummy_1[,-1]
carinfo_1 <- cbind(carinfo_1[,-7],dummy_1)

#dummy variable for enginetype , 
dummy_1 <- model.matrix(~enginetype,data = carinfo_1)
dummy_1 <- dummy_1[,-1]
carinfo_1 <- cbind(carinfo_1[,-13],dummy_1)

# dummy variable for cylindernumber 
dummy_1 <- model.matrix(~cylindernumber,data = carinfo_1)
dummy_1 <- dummy_1[,-1]
carinfo_1 <- cbind(carinfo_1[,-13],dummy_1)


# dummy variable for fuelsystem
dummy_1 <- model.matrix(~fuelsystem,data = carinfo_1)
dummy_1 <- dummy_1[,-1]
carinfo_1 <- cbind(carinfo_1[,-14],dummy_1)

#deleting unnecessary columns
carinfo_1<-carinfo_1[,-3]
carinfo_1<-carinfo_1[,-1]

#dummy variable for company name
dummy_1 <- model.matrix(~Company,data = carinfo_1)
dummy_1 <- dummy_1[,-1]
carinfo_1 <- cbind(carinfo_1[,-1],dummy_1)

#creating new variables as this indicates more the ratio the more is performance increase
carinfo_1$powerweightratio <- carinfo_1$horsepower/carinfo_1$curbweight
carinfo_1$widthlengthratio <- carinfo_1$carwidth/carinfo_1$carlength
carinfo_1$heightwidthratio <- carinfo_1$carheight/carinfo_1$carwidth
carinfo_1$heightlengthratio <- carinfo_1$carheight/carinfo_1$carlength

str(train)

#-------------------------------------------------------------------------------------------
#                     Model building process starts here
#-------------------------------------------------------------------------------------------

set.seed(100)

trainindices<- sample(1:nrow(carinfo_1), 0.7*nrow(carinfo_1))
train <- carinfo_1[trainindices,]
test<- carinfo_1[-trainindices,]

#creating linear model with all variables and found lots of variables are insignificant
model_1 <- lm(price~.,data = train)
summary(model_1)  #Multiple R-squared:  0.9811,	Adjusted R-squared:  0.966 

step <- stepAIC(model_1, direction="both")

#removing the insignificant variable and regenerating model 2 .
model_2 <- lm(price~ widthlengthratio    
              + fuelsystemspdi       
              + Companysaab          
              + carbodyhardtop      
              + carwidth            
              +enginetypeohcv       
              + Companydodge        
              + Companyjaguar        
              + Companyplymouth      
              + Companynissan        
              + fuelsystem2bbl       
              + fuelsystemmpfi       
              + carbodysedan         
              + carbodyhatchback     
              + carlength            
              + enginetypeohc        
              + drivewheelrwd       
              + heightwidthratio    
              + carbodywagon         
              + heightlengthratio   
              + enginetypedohcv      
              + Companyrenault       
              + cylindernumberfive   
              + aspiration           
              + Companyvolkswagen    
              + Companymitsubishi   
              + Companymazda         
              + Companytoyota       
              + cylindernumberfour   
              + enginetypeohcf       
              + enginetypel         
              + cylindernumbersix    
              + Companybuick         
              +powerweightratio    
              + horsepower          
              + Companybmw           
              + enginelocation     ,data = train)

# Model 2 : Multiple R-squared:  Multiple R-squared:  0.9795,	Adjusted R-squared:  0.9722 
summary(model_2)
sort(vif(model_2))

#removing widthlengthratio as VIF :1491.166184 and p value : 0.112364

model_3 <- lm(price~ fuelsystemspdi       
              + Companysaab          
              + carbodyhardtop      
              + carwidth            
              +enginetypeohcv       
              + Companydodge        
              + Companyjaguar        
              + Companyplymouth      
              + Companynissan        
              + fuelsystem2bbl       
              + fuelsystemmpfi       
              + carbodysedan         
              + carbodyhatchback     
              + carlength            
              + enginetypeohc        
              + drivewheelrwd       
              + heightwidthratio    
              + carbodywagon         
              + heightlengthratio   
              + enginetypedohcv      
              + Companyrenault       
              + cylindernumberfive   
              + aspiration           
              + Companyvolkswagen    
              + Companymitsubishi   
              + Companymazda         
              + Companytoyota       
              + cylindernumberfour   
              + enginetypeohcf       
              + enginetypel         
              + cylindernumbersix    
              + Companybuick         
              +powerweightratio    
              + horsepower          
              + Companybmw           
              + enginelocation     ,data = train)

# Model 3 : Multiple R-squared:  0.979,	Adjusted R-squared:  0.9718 
summary(model_3)
sort(vif(model_3))



#remvoing carlength as VIF IS 551.088014 and  p value cannot be relied upon

model_4 <- lm(price~        
                Companysaab          
              + carbodyhardtop      
              + carwidth            
              +enginetypeohcv       
              + Companydodge        
              + Companyjaguar        
              + Companyplymouth      
              + Companynissan        
              + fuelsystem2bbl       
              + fuelsystemmpfi       
              + carbodysedan         
              + carbodyhatchback     
              + enginetypeohc        
              + drivewheelrwd       
              + heightwidthratio    
              + carbodywagon         
              + heightlengthratio   
              + enginetypedohcv      
              + Companyrenault       
              + cylindernumberfive   
              + aspiration           
              + Companyvolkswagen    
              + Companymitsubishi   
              + Companymazda         
              + Companytoyota       
              + cylindernumberfour   
              + enginetypeohcf       
              + enginetypel         
              + cylindernumbersix    
              + Companybuick         
              +powerweightratio    
              + horsepower          
              + Companybmw           
              + enginelocation +fuelsystemspdi    ,data = train)

# Model 4 : Multiple R-squared: 0.9752,	Adjusted R-squared:  0.9671 
summary(model_4)
sort(vif(model_4))


#removing  heightwidthratio as VIF is 14.412168 and p value is insignificant 0.188644

model_5 <- lm(price~        
                Companysaab          
              + carbodyhardtop      
              + carwidth            
              +enginetypeohcv       
              + Companydodge        
              + Companyjaguar        
              + Companyplymouth      
              + Companynissan        
              + fuelsystem2bbl       
              + fuelsystemmpfi       
              + carbodysedan         
              + carbodyhatchback     
              + enginetypeohc        
              + drivewheelrwd       
              + carbodywagon         
              + heightlengthratio   
              + enginetypedohcv      
              + Companyrenault       
              + cylindernumberfive   
              + aspiration           
              + Companyvolkswagen    
              + Companymitsubishi   
              + Companymazda         
              + Companytoyota       
              + cylindernumberfour   
              + enginetypeohcf       
              + enginetypel         
              + cylindernumbersix    
              + Companybuick         
              +powerweightratio    
              + horsepower          
              + Companybmw           
              + enginelocation +fuelsystemspdi    ,data = train)

# Model 5 : Multiple R-squared: 0.9748,	Adjusted R-squared:  0.9669 
summary(model_5)
sort(vif(model_5))


#removing heightlengthratio as VIF is 4.731674  and p value = 0.716054

model_6 <- lm(price~        
                Companysaab          
              + carbodyhardtop      
              + carwidth            
              +enginetypeohcv       
              + Companydodge        
              + Companyjaguar        
              + Companyplymouth      
              + Companynissan        
              + fuelsystem2bbl       
              + fuelsystemmpfi       
              + carbodysedan         
              + carbodyhatchback     
              + enginetypeohc        
              + drivewheelrwd       
              + carbodywagon         
               + enginetypedohcv      
              + Companyrenault       
              + cylindernumberfive   
              + aspiration           
              + Companyvolkswagen    
              + Companymitsubishi   
              + Companymazda         
              + Companytoyota       
              + cylindernumberfour   
              + enginetypeohcf       
              + enginetypel         
              + cylindernumbersix    
              + Companybuick         
              +powerweightratio    
              + horsepower          
              + Companybmw           
              + enginelocation +fuelsystemspdi    ,data = train)

# Model 6 : Multiple R-squared: .9748,	Adjusted R-squared:  0.9671 
summary(model_6)
sort(vif(model_6))

#removing powerweightratio as the VIF is  23.783569  and is related to horsepower ratio though both p value is significantly lower



model_7 <- lm(price~        
                Companysaab          
              + carbodyhardtop      
              + carwidth            
              +enginetypeohcv       
              + Companydodge        
              + Companyjaguar        
              + Companyplymouth      
              + Companynissan        
              + fuelsystem2bbl       
              + fuelsystemmpfi       
              + carbodysedan         
              + carbodyhatchback     
              + enginetypeohc        
              + drivewheelrwd       
              + carbodywagon         
              + enginetypedohcv      
              + Companyrenault       
              + cylindernumberfive   
              + aspiration           
              + Companyvolkswagen    
              + Companymitsubishi   
              + Companymazda         
              + Companytoyota       
              + cylindernumberfour   
              + enginetypeohcf       
              + enginetypel         
              + cylindernumbersix    
              + Companybuick         
              +horsepower
             + Companybmw           
              + enginelocation +fuelsystemspdi    ,data = train)

# Model 7 : Multiple R-squared: 0.9647,	Adjusted R-squared:  0.9544 
summary(model_7)
sort(vif(model_7))


#removing enginetypeohc as VIF is 8.098867 and p value is 0.237658


model_8 <- lm(price~        
                Companysaab          
              + carbodyhardtop      
              + carwidth            
              +enginetypeohcv       
              + Companydodge        
              + Companyjaguar        
              + Companyplymouth      
              + Companynissan        
              + fuelsystem2bbl       
              + fuelsystemmpfi       
              + carbodysedan         
              + carbodyhatchback     
              + drivewheelrwd       
              + carbodywagon         
              + enginetypedohcv      
              + Companyrenault       
              + cylindernumberfive   
              + aspiration           
              + Companyvolkswagen    
              + Companymitsubishi   
              + Companymazda         
              + Companytoyota       
              + cylindernumberfour   
              + enginetypeohcf       
              + enginetypel         
              + cylindernumbersix    
              + Companybuick         
              +horsepower
              + Companybmw           
              + enginelocation +fuelsystemspdi    ,data = train)

# Model 8 : Multiple R-squared:   0.9642,	Adjusted R-squared:  0.9542 
summary(model_8)
sort(vif(model_8))


#removing fuelsystemmpfi  as 0.445090 and vif is 5.568169 





model_9 <- lm(price~        
                Companysaab          
              + carbodyhardtop      
              + carwidth            
              +enginetypeohcv       
              + Companydodge        
              + Companyjaguar        
              + Companyplymouth      
              + Companynissan        
              + fuelsystem2bbl       
              + carbodysedan         
              + carbodyhatchback     
              + drivewheelrwd       
              + carbodywagon         
              + enginetypedohcv      
              + Companyrenault       
              + cylindernumberfive   
              + aspiration           
              + Companyvolkswagen    
              + Companymitsubishi   
              + Companymazda         
              + Companytoyota       
              + cylindernumberfour   
              + enginetypeohcf       
              + enginetypel         
              + cylindernumbersix    
              + Companybuick         
              +horsepower
              + Companybmw           
              + enginelocation +fuelsystemspdi    ,data = train)

# Model 9 : Multiple R-squared:    0.964,	Adjusted R-squared:  0.9544 
summary(model_9)
sort(vif(model_9))

#removing Companysaab as p value is 0.407377




model_10 <- lm(price~        
                 carbodyhardtop      
              + carwidth            
              +enginetypeohcv       
              + Companydodge        
              + Companyjaguar        
              + Companyplymouth      
              + Companynissan        
              + fuelsystem2bbl       
              + carbodysedan         
              + carbodyhatchback     
              + drivewheelrwd       
              + carbodywagon         
              + enginetypedohcv      
              + Companyrenault       
              + cylindernumberfive   
              + aspiration           
              + Companyvolkswagen    
              + Companymitsubishi   
              + Companymazda         
              + Companytoyota       
              + cylindernumberfour   
              + enginetypeohcf       
              + enginetypel         
              + cylindernumbersix    
              + Companybuick         
              +horsepower
              + Companybmw           
              + enginelocation +fuelsystemspdi    ,data = train)

# Model 10 : Multiple R-squared:    0.9638,	Adjusted R-squared:  0.9545 
summary(model_10)
sort(vif(model_10))

#removing fuelsystemspdi as p  value is 0.893813


model_11 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companydodge        
               + Companyjaguar        
               + Companyplymouth      
               + Companynissan        
               + fuelsystem2bbl       
               + carbodysedan         
               + carbodyhatchback     
               + drivewheelrwd       
               + carbodywagon         
               + enginetypedohcv      
               + Companyrenault       
               + cylindernumberfive   
               + aspiration           
               + Companyvolkswagen    
               + Companymitsubishi   
               + Companymazda         
               + Companytoyota       
               + cylindernumberfour   
               + enginetypeohcf       
               + enginetypel         
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 11 : Multiple R-squared:    0.9638,	Adjusted R-squared:  0.9549 
summary(model_11)
sort(vif(model_11))


#removing enginetypel as p value is 0.439023



model_12 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companydodge        
               + Companyjaguar        
               + Companyplymouth      
               + Companynissan        
               + fuelsystem2bbl       
               + carbodysedan         
               + carbodyhatchback     
               + drivewheelrwd       
               + carbodywagon         
               + enginetypedohcv      
               + Companyrenault       
               + cylindernumberfive   
               + aspiration           
               + Companyvolkswagen    
               + Companymitsubishi   
               + Companymazda         
               + Companytoyota       
               + cylindernumberfour   
               + enginetypeohcf       
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 12 : Multiple R-squared:    0.9636,	Adjusted R-squared:  0.9551 
summary(model_12)
sort(vif(model_12))

#removing fuelsystem2bbl as p value is 0.165696




model_13 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companydodge        
               + Companyjaguar        
               + Companyplymouth      
               + Companynissan        
                + carbodysedan         
               + carbodyhatchback     
               + drivewheelrwd       
               + carbodywagon         
               + enginetypedohcv      
               + Companyrenault       
               + cylindernumberfive   
               + aspiration           
               + Companyvolkswagen    
               + Companymitsubishi   
               + Companymazda         
               + Companytoyota       
               + cylindernumberfour   
               + enginetypeohcf       
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 13 : Multiple R-squared:   0.963,	Adjusted R-squared:  0.9547 
summary(model_13)
sort(vif(model_13))


#removing Companytoyota as p value is 0.138608


model_14 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companydodge        
               + Companyjaguar        
               + Companyplymouth      
               + Companynissan        
               + carbodysedan         
               + carbodyhatchback     
               + drivewheelrwd       
               + carbodywagon         
               + enginetypedohcv      
               + Companyrenault       
               + cylindernumberfive   
               + aspiration           
               + Companyvolkswagen    
               + Companymitsubishi   
               + Companymazda         
               + cylindernumberfour   
               + enginetypeohcf       
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 14 : Multiple R-squared:    0.9623,	Adjusted R-squared:  0.9542
summary(model_14)
sort(vif(model_14))


#removing Companyplymouth as p value is 0.062572


model_15 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companydodge        
               + Companyjaguar        
               + Companynissan        
               + carbodysedan         
               + carbodyhatchback     
               + drivewheelrwd       
               + carbodywagon         
               + enginetypedohcv      
               + Companyrenault       
               + cylindernumberfive   
               + aspiration           
               + Companyvolkswagen    
               + Companymitsubishi   
               + Companymazda         
               + cylindernumberfour   
               + enginetypeohcf       
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 15 : Multiple R-squared:    0.9612,	Adjusted R-squared:  0.9533 
summary(model_15)
sort(vif(model_15))

#removing Companydodge as p value is 0.092637



model_16 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
                + Companyjaguar        
               + Companynissan        
               + carbodysedan         
               + carbodyhatchback     
               + drivewheelrwd       
               + carbodywagon         
               + enginetypedohcv      
               + Companyrenault       
               + cylindernumberfive   
               + aspiration           
               + Companyvolkswagen    
               + Companymitsubishi   
               + Companymazda         
               + cylindernumberfour   
               + enginetypeohcf       
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 16 : Multiple R-squared:    0.9602,	Adjusted R-squared:  0.9525 
summary(model_16)
sort(vif(model_16))


#removing aspiration  as p value is 0.116011



model_17 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + Companynissan        
               + carbodysedan         
               + carbodyhatchback     
               + drivewheelrwd       
               + carbodywagon         
               + enginetypedohcv      
               + Companyrenault       
               + cylindernumberfive   
               + Companyvolkswagen    
               + Companymitsubishi   
               + Companymazda         
               + cylindernumberfour   
               + enginetypeohcf       
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 17 : Multiple R-squared:   0.9594,	Adjusted R-squared:  0.9519 
summary(model_17)
sort(vif(model_17))


#removing Companynissan as p value is 0.114000


model_18 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + carbodysedan         
               + carbodyhatchback     
               + drivewheelrwd       
               + carbodywagon         
               + enginetypedohcv      
               + Companyrenault       
               + cylindernumberfive   
               + Companyvolkswagen    
               + Companymitsubishi   
               + Companymazda         
               + cylindernumberfour   
               + enginetypeohcf       
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 18 : Multiple R-squared:   0.9585,	Adjusted R-squared:  0.9513 
summary(model_18)
sort(vif(model_18))


#removing drivewheelrwd as p value is 0.144106



model_19 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + carbodysedan         
               + carbodyhatchback     
               + carbodywagon         
               + enginetypedohcv      
               + Companyrenault       
               + cylindernumberfive   
               + Companyvolkswagen    
               + Companymitsubishi   
               + Companymazda         
               + cylindernumberfour   
               + enginetypeohcf       
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 19 : Multiple R-squared:   0.9578,	Adjusted R-squared:  0.9509 
summary(model_19)
sort(vif(model_19))


#removing Companyrenault and Companyvolkswagen as p value is 0.10721 and 0.18993 respectively

model_20 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + carbodysedan         
               + carbodyhatchback     
               + carbodywagon         
               + enginetypedohcv      
               + cylindernumberfive   
               + Companymitsubishi   
               + Companymazda         
               + cylindernumberfour   
               + enginetypeohcf       
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 20 : Multiple R-squared:   0.9563,	Adjusted R-squared:   0.95 
summary(model_20)
sort(vif(model_20))

# removing enginetypeohcf as p value is 0.12402 


model_21 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + carbodysedan         
               + carbodyhatchback     
               + carbodywagon         
               + enginetypedohcv      
               + cylindernumberfive   
               + Companymitsubishi   
               + Companymazda         
               + cylindernumberfour   
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 21 : Multiple R-squared:   0.9555,	Adjusted R-squared:  0.9494 
summary(model_21)
sort(vif(model_21))


#at this point all variables p value is less then 0.05 
#removing carbodywagon as VIF is 6.145466 and p value is  0.022268 which is still high among the variables with high vif




model_22 <- lm(price~        
                 carbodyhardtop      
               + carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + carbodysedan         
               + carbodyhatchback     
               + enginetypedohcv      
               + cylindernumberfive   
               + Companymitsubishi   
               + Companymazda         
               + cylindernumberfour   
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 22 : Multiple R-squared:   0.9536,	Adjusted R-squared:  0.9477 
summary(model_22)
sort(vif(model_22))

#removing carbodysedan a p value is 0.97582 and carbodyhardtop as p value is 0.21686   



model_23 <- lm(price~        
                  carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + carbodyhatchback     
               + enginetypedohcv      
               + cylindernumberfive   
               + Companymitsubishi   
               + Companymazda         
               + cylindernumberfour   
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 23 : Multiple R-squared:   0.9529,	Adjusted R-squared:  0.9478 
summary(model_23)
sort(vif(model_23))


#removing cylindernumberfour as VIF is 6.636340




model_24 <- lm(price~        
                 carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + carbodyhatchback     
               + enginetypedohcv      
               + cylindernumberfive   
               + Companymitsubishi   
               + Companymazda         
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 24 : Multiple R-squared:   0.9465,	Adjusted R-squared:  0.9411 
summary(model_24)
sort(vif(model_24))

#removing cylindernumberfive as p value is 0.871109     



model_25 <- lm(price~        
                 carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + carbodyhatchback     
               + enginetypedohcv      
                + Companymitsubishi   
               + Companymazda         
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 25 : Multiple R-squared:   0.9465,	Adjusted R-squared:  0.9415 
summary(model_25)
sort(vif(model_25))

#removing Companymazda as p value is 0.642203

model_26 <- lm(price~        
                 carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + carbodyhatchback     
               + enginetypedohcv      
               + Companymitsubishi   
               + cylindernumbersix    
               + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 26 : Multiple R-squared:   0.9464,	Adjusted R-squared:  0.9419 
summary(model_26)
sort(vif(model_26))


#removing cylindernumbersix as p value is 0.0567 

model_27 <- lm(price~        
                 carwidth            
               +enginetypeohcv       
               + Companyjaguar        
               + carbodyhatchback     
               + enginetypedohcv      
               + Companymitsubishi   
                + Companybuick         
               +horsepower
               + Companybmw           
               + enginelocation    ,data = train)

# Model 27 : Multiple R-squared:  0.9449,	Adjusted R-squared:  0.9407  
summary(model_27)
sort(vif(model_27))





#----------------------------------------------------------------------------------------------------- 
#   finally all VIF and p value are satisfactory and we can conclude that following are the variables based on which prediction
#   can be done Companymitsubishi        Companybmw   enginetypedohcv     Companyjaguar  carbodyhatchback    enginetypeohcv    enginelocation
#Companybuick          carwidth        horsepower
#-----------------------------------------------------------------------------------------------------

# now doing model testing 


#predicting price using model_27

predict_price <- predict(model_27,test[,-18])
test$test_price <- predict_price
r <- cor(test$test_price,test$price)
rsquared <- r^2

#rsquared is 0.842

summary(model_27)


#Multiple R-squared:  0.9449,,	Adjusted R-squared:  0.9407 , so here there is not much diff b/w rsquared and adjusted rsquared 
##rsquared for predicted case is  0.842 and there is difference of 0.102 



# Key variables used for car price prediction

# 1. Engine location 
# 2. Luxury car brand - Companymitsubishi,Companybmw , Companyjaguar , Companybuick
# 3.enginetypedohcv , enginetypeohcv , carwidth , horsepower


