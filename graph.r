#A graph for sex
sexPlot = function() 
{
    #sex data processing
    sexData = read.csv("data.csv")
    countSex = table(sexData$Sex)

    #convert to dataframe
    df = as.data.frame(countSex)

    data = c(df$Freq)

    #graph
    sex = sprintf("%s (%s)", c("Female", "Male"), scales::percent(round(data/sum(data), 2)))
    names(data) = sex

    #export to file when donw
    jpeg("Distribution of a Cosplay market by Sex.jpg", width = 1200, height = 600)

    #start plotting
    plot(waffle::waffle(data))

    #close a file
    dev.off()
}

#A graph for age
agePlot = function()
{
    library(ggplot2)

    #age data processing
    ageData = read.csv("data.csv")
    countAge = table(ageData$Age)

    #excluding "FALSE" age value
    excluding = names(countAge) %in% c("FALSE")
    excludedData = countAge[!excluding]

    #convert to dataframe
    df = as.data.frame(excludedData)
    data = c(df$Freq)

    #age range
    ageRange = c('13', '14', '15', '16', '17', '18', '19', '20', 
        '21', '22', '23', '24', '25', '26', '27 and up')

    #combined an over range age
    #For "R" ARRAY START AT ONE!
    overRange = c(data[15] + data[16] + data[17])

    #delete a data in Default dataframe
    data = data[-c(15, 16, 17)]

    #insert an Over range data in Default dataframe
    data[15] = overRange[1]

    #decoration
    #shading pink
    color = c('#EAD6E3', '#E4C9D9', '#DDBBD0', '#D6AEC7', '#CFA0BD', '#C892B4', '#C185AA', 
        '#BB77A1', '#B46A97', '#AD5C8E', '#A35284', '#954B79', '#88446E', '#7A3E63', '#6D3758') 

    #export to file when done
    jpeg("Distribution of a Cosplay market by Age.jpg", width = 1200, height = 600)

    #start plotting
    barplot(data, main="Distribution of a Cosplay market by Age",
    xlab = "Age", ylab = "People", col=c(color), names.arg = ageRange)

    #close a file
    dev.off()

}

ageDataVerify = function()
{
    ageData = read.csv("data.csv")
    countAge = table(ageData$Age)

    countAge
}

