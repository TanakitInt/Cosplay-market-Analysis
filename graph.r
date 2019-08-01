#install addtional Packages
install.packages("Rttf2pt1", dependencies = TRUE)
library(Rttf2pt1)
install.packages("waffle", dependencies = TRUE)
library(waffle)
install.packages("ggthemes", dependencies = TRUE)
library(ggthemes)


#A pie graph for sex
sexPlot = function() 
{
    #sex data processing
    sexData = read.csv("data.csv")
    countSex = table(sexData$Sex)

    #convert to dataframe
    df = as.data.frame(countSex)

    vals = c(df$Freq)

    # Give the chart file a name
    #png(file = "Distribution of a Cosplay market by Sex.png")

    #graph
    val_names = sprintf("%s (%s)", c("Female", "Male"), scales::percent(round(vals/sum(vals), 2)))
    names(vals) = val_names

    waffle::waffle(vals)

    #dev.off()
}

#A bar graph for age
agePlot = function()
{
    #age data processing
    ageData = read.csv("data.csv")
    countAge = table(ageData$Age)

    #merging data for age >= 27
    #overRange = subset(countAge,  >= 27)

    #excluding "FALSE" age value
    excluding = names(countAge) %in% c("FALSE")
    excludedData = countAge[!excluding]

    data = excludedData
    
#    #Set a fixed axis value (For age)
#    xAxis = c('13', '14', '15', '16', '17', '18', '19', 
#        '20', '21', '22', '23', '24', '25', '26', '27 and up')
#
#    # Give the chart file a name
#    png(file = "Distribution of a Cosplay market by Age.png")
#
    #decoration
    color = c('#E6EE9C') #light lemon
#
#    #Chart
#    barplot(excludedData, names.arg = xAxis, xlab = "Ages", 
#        ylab = "People", col = color, 
#        main = "Distribution of a Cosplay market by Age")

    hist(data, breaks = 1, prob = TRUE, col = color)
    lines(density(data))

    dev.off()

}





#age data check
countAge = table(ageData$Age)
countAge

