codebook = read.csv('Codebook.csv', stringsAsFactors = F)
motives = read.csv('Motive Codes.csv', stringsAsFactors = F)
killers = read.csv('Serial Killers Data.csv', stringsAsFactors = F)
victims = read.csv('Victim Codes.csv', stringsAsFactors = F)

str(killers, list.len = length(killers))


#Aspects...

aspects = data.frame(colnames(killers))
aspects$Min = 0
aspects$Max = 0

i=1

while (i <= dim(killers)[2]) {
    
    aspects[i,2] = min(killers[,i], na.rm = T)
    aspects[i,3] = max(killers[,i], na.rm = T)
    i=i+1
    
}

#Where are the NAs?
nas = data.frame(sapply(killers, function(x) sum(is.na(x) | x == "")))
aspects = cbind(aspects, nas)
rownames(aspects) = seq(1, length(aspects$colnames.killers.),1)
aspects$NA_pc = round(aspects$sapply.killers..function.x..sum.is.na.x... / length(killers$Name) * 100, 2)

i=1

while (i <= dim(killers)[2]) {
    
    aspects[i,6] = length(unique(killers[,i]))
    i=i+1
    
}

aspects = aspects[order(-aspects$`No. of NAs`),] #Get them in order of scale

colnames(aspects) = c('Feature', 'Min', 'Max', 'No. of NAs', '% NAs', 'No. of unique values')




library(lubridate)
library(ggplot2)
library(dplyr)


#Do motives fluctuate over a given period of time?

killers$Type = as.factor(killers$Type)
killers$DateFirst = mdy(killers$DateFirst)
killers$Year = year(killers$DateFirst)

by_type_year = group_by(killers, Type, Year)
NumVicsByType = data.frame(summarise(by_type_year, Total = sum(NumVics, na.rm = T)))
NumVicsByType = NumVicsByType[order(-NumVicsByType$Total),]

ex = is.na(NumVicsByType$Year)
NumVicsByType = NumVicsByType[!ex,]

NumVicsByType$Type

NumVicsByType$Motive = "Other"
NumVicsByType$Motive[grep("Anger", NumVicsByType$Type)] = "Anger"
NumVicsByType$Motive[grep("Convenience", NumVicsByType$Type)] = "Convenience"
#NumVicsByType$Motive[grep("Criminal", NumVicsByType$Type)] = "Criminal"
NumVicsByType$Motive[grep("Enjoyment", NumVicsByType$Type)] = "Enjoyment"
NumVicsByType$Motive[grep("FinancialGain", NumVicsByType$Type)] = "FinancialGain"
#NumVicsByType$Motive[grep("Mentalillness", NumVicsByType$Type)] = "Mentalillness"
#NumVicsByType$Motive[grep("Multiplemotives", NumVicsByType$Type)] = "Multiplemotives"
NumVicsByType$Motive[grep("Other", NumVicsByType$Type)] = "Other"

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(NumVicsByType, aes(x=Year, y=Total, col = Motive, group = Motive)) +
            geom_line(size=1) + 
            scale_colour_manual(values=cbbPalette) + 
            theme_bw()

#What about methods (weapon usage)?

killers$MethodDescription = as.factor(killers$MethodDescription)

by_method_year = group_by(killers, MethodDescription, Year)
NumVicsByMethod = data.frame(summarise(by_method_year, Total = sum(NumVics, na.rm = T)))
NumVicsByMethod = NumVicsByMethod[order(-NumVicsByMethod$Total),]

ex = is.na(NumVicsByMethod$Year)
NumVicsByMethod = NumVicsByMethod[!ex,]

NumVicsByMethod$Method = "Other"
NumVicsByMethod$Method[grep("^Strangle$", NumVicsByMethod$MethodDescription)] = "Strangle"
NumVicsByMethod$Method[grep("^Pills$", NumVicsByMethod$MethodDescription)] = "Pills"
NumVicsByMethod$Method[grep("^Poison$", NumVicsByMethod$MethodDescription)] = "Poison"
NumVicsByMethod$Method[grep("^Shoot$", NumVicsByMethod$MethodDescription)] = "Shoot"
NumVicsByMethod$Method[grep("^Strangle$", NumVicsByMethod$MethodDescription)] = "Strangle"
NumVicsByMethod$Method[grep("^Stab$", NumVicsByMethod$MethodDescription)] = "Stab"
NumVicsByMethod$Method[grep(",", NumVicsByMethod$MethodDescription)] = "Multiple"

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(NumVicsByMethod, aes(x=Year, y=Total, col = Method, group = Method)) +
    geom_line(size=1) + 
    scale_colour_manual(values=cbbPalette) + 
    theme_bw()



#Do offenders with certain motives prefer certain types of weapons?

by_motive_method = group_by(killers, Type, MethodDescription)
NumVicsByMotiveMethod = data.frame(summarise(by_motive_method, Total = sum(NumVics, na.rm = T)))
NumVicsByMotiveMethod = NumVicsByMotiveMethod[order(-NumVicsByMotiveMethod$Total),]

ex = (NumVicsByMotiveMethod$MethodDescription == "" | NumVicsByMotiveMethod$Type == "")
NumVicsByMotiveMethod = NumVicsByMotiveMethod[!ex,]


#What factors impact an offender’s decision to use multiple methods (weapons) over their series?

killers$Multi = 0
killers$Multi[killers$Type == 'Multiplemotives'] = 1

l = length(killers$Name)
len_s = length(killers$Sex[killers$Multi == 0])
len_m = length(killers$Sex[killers$Multi == 1])

len_s / l * 100
len_m / l * 100

sex_0 = data.frame(table(killers$Sex[killers$Multi == 0]) / len_s*100)
sex_1 = data.frame(table(killers$Sex[killers$Multi == 1]) / len_m*100)
sex = data.frame(cbind(sex_0$Freq, sex_1$Freq))
rownames(sex) = c('Male', 'Female')
colnames(sex) = c('% single method', '% multiple methods')
sex$`% single method` = round(sex$`% single method`, 2)
sex$`% multiple methods` = round(sex$`% multiple methods`, 2)
sex

race_0 = data.frame(table(killers$Race[killers$Multi == 0]) / len_s*100)
race_1 = data.frame(table(killers$Race[killers$Multi == 1]) / len_m*100)
race_1$Var1 = as.character(race_1$Var1)
race_1 = rbind(race_1, c("6",0))
race = data.frame(cbind(race_0$Freq, race_1$Freq))
race$X1 = as.numeric(as.character(race$X1))
race$X2 = as.numeric(as.character(race$X2))
rownames(race) = c('White', 'Black', 'Hispanic', 'Asian', 'Native American', 'Aboriginal')
colnames(race) = c('% single method', '% multiple methods')
race$`% single method` = round(race$`% single method`, 2)
race$`% multiple methods` = round(race$`% multiple methods`, 2)

race

m_iq1 = mean(killers$IQ1[killers$Multi == 0], na.rm = T)
m_iq2 = mean(killers$IQ1[killers$Multi == 1], na.rm = T)
m_iqs = data.frame(c(m_iq1,m_iq2))

m_age1 = mean(killers$Age1stKill[killers$Multi == 0], na.rm = T)
m_age2 = mean(killers$Age1stKill[killers$Multi == 1], na.rm = T)
m_ages = data.frame(c(m_age1,m_age2))

m_age1b = mean(killers$AgeLastKill[killers$Multi == 0], na.rm = T)
m_age2b = mean(killers$AgeLastKill[killers$Multi == 1], na.rm = T)
m_agesb = data.frame(c(m_age1b,m_age2b))

m_height1 = mean(killers$Height[killers$Multi == 0], na.rm = T)
m_height2 = mean(killers$Height[killers$Multi == 1], na.rm = T)
m_heights = data.frame(c(m_height1,m_height2))

m_numvic1 = mean(killers$NumVics[killers$Multi == 0], na.rm = T)
m_numvic2 = mean(killers$NumVics[killers$Multi == 1], na.rm = T)
m_numvics = data.frame(c(m_numvic1,m_numvic2))

#Merge plots,
my_plots = data.frame(m_iqs, m_ages, m_agesb, m_heights, m_numvics)

#Plot all,
barplot(as.matrix(my_plots), names.arg=c("IQ", "Age (1st kill)", "Age (last kill)", "Heights", "No. victims"), 
        beside=TRUE, col=terrain.colors(2))

#Set the legend,
legend(6, 80, c("Single method", "Multiple methods"), cex=0.8, fill=terrain.colors(2))



#What is the most common victim type for killers with expressive motives such as anger, revenge and lust?

by_victim_method = group_by(killers, Type, VictimCode)
VictimByType= data.frame(summarise(by_victim_method, Total = sum(NumVics, na.rm = T)))
VictimByType = VictimByType[order(-VictimByType$Total),]

ex = is.na(VictimByType$VictimCode)
VictimByType = VictimByType[!ex,]

anger = grep("anger|revenge|lust", VictimByType$Type, ignore.case = T)
VictimByType = VictimByType[anger,]
VictimByType = VictimByType[1:10,]

VictimByType$Type_name = ""
VictimByType$Type_name[VictimByType$VictimCode == 11.00] = 'Multiple victim types'
VictimByType$Type_name[VictimByType$VictimCode == 8.30] = 'Street - General public'
VictimByType$Type_name[VictimByType$VictimCode == 5.00] = 'Family'
VictimByType$Type_name[VictimByType$VictimCode == 8.50] = 'Street - Acquaintences'
VictimByType$Type_name[VictimByType$VictimCode == 5.70] = 'Family - Girl/Boy friends'
VictimByType$Type_name[VictimByType$VictimCode == 8.32] = 'Street - Men'
VictimByType$Type_name[VictimByType$VictimCode == 4.10] = 'Patients/Wards - Hospital patients'



#What is the most common victim type for killers with instrumental motives such as financial gain?

by_victim_method = group_by(killers, Type, VictimCode)
VictimByType2= data.frame(summarise(by_victim_method, Total = sum(NumVics, na.rm = T)))
VictimByType2 = VictimByType2[order(-VictimByType2$Total),]

ex = is.na(VictimByType2$VictimCode)
VictimByType2 = VictimByType2[!ex,]

financial = grep("financial", VictimByType2$Type, ignore.case = T)
VictimByType2 = VictimByType2[financial,]
VictimByType2 = VictimByType2[1:10,]

VictimByType2$Type_name = ""
VictimByType2$Type_name[VictimByType2$VictimCode == 8.33] = 'Street - Adults - men & women'
VictimByType2$Type_name[VictimByType2$VictimCode == 8.30] = 'Street - General public'
VictimByType2$Type_name[VictimByType2$VictimCode == 4.30] = 'Patients/Wards - Child care'
VictimByType2$Type_name[VictimByType2$VictimCode == 6.00] = 'Employees/Customers'
VictimByType2$Type_name[VictimByType2$VictimCode == 7.10] = 'Home invasion - Men & Women'
VictimByType2$Type_name[VictimByType2$VictimCode == 6.10] = 'Employees/Customers - Employees'
VictimByType2$Type_name[VictimByType2$VictimCode == 11.00] = 'Multiple victim types'
VictimByType2$Type_name[VictimByType2$VictimCode == 4.20] = 'Patients/Wards - Wards'
VictimByType2$Type_name[VictimByType2$VictimCode == 8.31] = 'Street - Women'




#Do the characteristics of the victims interact with those of the offenders in some unforeseen way? How are offenders and victims similar and different?

#Do men that kill exclusively women vary from those that kill men and women?

#What are the observable differences between offenders that kill for sexual reasons and those with financial motives?

#Are there stark differences between offenders that kill two victims as opposed to three and above?

#What role does the age of the offender play in impacting the other variables? Do older offenders kill more victims over time? Are older offenders able to remain unapprehended for longer timespans?

#What does the offender’s choice of location tell us? Do region based offenders have different motives and methods than multistate killers? Do any particular counties experience more series than others?

#Is there an association between the offender's chosen state of operation and the weapon they use, such as a handgun?

#What are some variables unique to partnerships?



library(caret)

boxplot(killers$NumVics)
boxplot(killers$NumVics, outline = F)

killers$Severity = 1
killers$Severity[killers$NumVics > 2] = 2
killers$Severity[killers$NumVics > 12] = 3

table(killers$Severity)

killers$Severity = as.factor(killers$Severity)
killers$Sex = as.factor(killers$Sex)
killers$Race = as.factor(killers$Race)
killers$WhiteMale = as.factor(killers$WhiteMale)

model = train(NumVics ~ WhiteMale + AgeLastKill, method = 'ctree', data = killers)

plot(model$finalModel)



