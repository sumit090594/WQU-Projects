library(shiny)
library(readxl)
library(caret)
#library(NbClust)
library(factoextra)
df = read_excel(("CountryData.xlsx"), sheet = 1)

#-----------------exploratory data analysis
# Renaming the vairbles 
names(df)[3:21]<- c("ForeignInvestment", "ElectricityAccess", "RenewableEnergy", "CO2Emission", "Inflation", "MobileSubscriptions", "InternetUse", "Exports", "Imports", "GDP", "MortalityMale", "MortalityFemale", "BirthRate", "DeathRate", "MortalityInfant", "LifeExpectancy", "FertilityRate", "PopulationGrowth", "UrbanPopulation")

#Looking at null values
#we check for NA's in rows
null_rows = apply(df, 1, function(x) sum(is.na(x)))

#we add the country names
row_nulls = data.frame(df$CountryName,null_rows)

#we select where not 0
row_nulls[as.numeric(row_nulls[,2])>0,]


#we check for nulls in columns
apply(df, 2, function(x) sum(is.na(x)))

# Setting seed so out results are imputation results are reprodcuible
set.seed(0)

#we impute missing values with a random forest
imputation_model = preProcess(x = df[,-c(1,2)],method = "bagImpute")
imputated_data = predict(object = imputation_model,newdata=df[,-c(1,2)])

#Adding country names to the rows
rownames(imputated_data)<- t(df[,2]) 

#we check for nulls in imputed data, success there are none :D
apply(imputated_data, 2, function(x) sum(is.na(x)))

########### MODEL BUILDING ##############

pca.out<-prcomp(imputated_data,scale=TRUE)

# Scaling our data
scaled_data = scale(imputated_data)

# K-means clustering, Choosing the number of k
#res<- NbClust(scaled_data, distance = "euclidean", min.nc=2, max.nc=10,method = "kmeans", index = "all")  

function(input, output) {
    output$chart <- renderPlot({
        title<-"Output"
        km.res <- kmeans(scaled_data, input$num, nstart = 50)
        fviz_cluster(km.res, 
                     data = scaled_data,
                     ellipse.type = input$rad, 
                     star.plot = TRUE, # Add segments from centroids to items
                     repel = TRUE, # Avoid label overplotting (slow)
                     ggtheme = theme_minimal()
        )
    })  
}