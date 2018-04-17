#LOAD LIBRARIES AND DATA
########################

library(ggplot2)
FoodFacts = read.csv("data\\FoodFacts.csv", header = TRUE) #Load the CSV file


#CLEAN UP THE DATA
##################

#Find the products in the catagory 'meat' and the product labelled 'vegan',
meat = grep("meat|Meat", FoodFacts$categories_en)
vegan = grep("vegan|Vegan", FoodFacts$labels)

#Create a new dataframe for countries vs number of products
Countries = as.data.frame(table(FoodFacts$countries_en))
Countries = Countries[-1,] #Remove the first, empty row

#What are the countries with the highest levels?
Top = head(Countries[order(-Countries$Freq),],10)
Top = droplevels(Top)

#However, these country names appears in many more rows as part of combined lists of countries.
#We need to add these values to the individual countries.

#The following while loop does the following,
# - Picks out all the rows where each coountry name appears,
# - Adds up all the totals for these countries,
# - Updates the total in the original table with the new, larger number,

i = 1

while (i < length(Top$Var1)+1) {
    
    Country = grep(Top[i,1], Countries$Var1)
    Country_totals = sum(Countries$Freq[Country])
    Top[i,2] = Country_totals
    i=i+1
}

#Quick check of the number of products per country,
barplot(Top$Freq, names.arg = Top$Var1, col = "grey", main = "Number of Products by Country"
        , ylab = "Counts", las=1)

#Create a new dataframe for countries vs number of meat products
Countries_meat = as.data.frame(table(FoodFacts$countries_en[meat]))
Countries_meat = Countries_meat[-1,] #Remove the first, empty row

#What are the countries with the highest meat products?
Top_meat = head(Countries_meat[order(-Countries_meat$Freq),],10)
Top_meat = Top_meat[-grep(",", Top_meat$Var1),] #Get rid of any rows that are a combination of countries by looking for a comma
Top_meat = droplevels(Top_meat)

#Again, all the cases from the combined countries need adding to the totals,

i = 1

while (i < length(Top_meat$Var1)+1) {
 
    Country = grep(Top[i,1], Countries_meat$Var1)
    Country_totals = sum(Countries_meat$Freq[Country])
    Top_meat[i,2] = Country_totals
    i=i+1
}


#Create a new dataframe for countries vs number of vegan-labelled products
Countries_vegan = as.data.frame(table(FoodFacts$countries_en[vegan]))
Countries_vegan = Countries_vegan[-1,] #Remove the first, empty row

#What are the countries with the highest vegan products?
Top_vegan = head(Countries_vegan[order(-Countries_vegan$Freq),],10)
Top_vegan = Top_vegan[-grep(",", Top_vegan$Var1),] #Get rid of any rows that are a combination of countries by looking for a comma
Top_vegan = droplevels(Top_vegan)

#Again, all the cases from the combined countries need adding to the totals,

i = 1

while (i < length(Top_vegan$Var1)+1) {
    
    Country = grep(Top_vegan[i,1], Countries_vegan$Var1)
    Country_totals = sum(Countries_vegan$Freq[Country])
    Top_vegan[i,2] = Country_totals
    i=i+1
}

#Remove the starting data to save memory,
rm(FoodFacts)

#Change column names,
colnames(Top) = c("Country", "Count")
colnames(Top_meat) = c("Country", "Count")
colnames(Top_vegan) = c("Country", "Count")

#Do some merging to get overall results,
Results_meat = merge(Top, Top_meat, by = "Country")
Results_vegan = merge(Top, Top_vegan, by = "Country")
colnames(Results_meat) = c("Country", "Total no. of products", "No. of meat products")
colnames(Results_vegan) = c("Country", "Total no. of products", "No. of vegan products")

#Work out how many meat and vegan products there are for each country as a % of total number in the dataset,
Results_meat$MeatPerc = Results_meat$`No. of meat products` / Results_meat$`Total no. of products` * 100
Results_vegan$VeganPerc = Results_vegan$`No. of vegan products` / Results_vegan$`Total no. of products` * 100



#MAKE SOME PLOTS
################

#Have a look at the countries with the highest % of meat products (in terms of number of products in the dataset),
m = ggplot(Results_meat, aes(x=reorder(Country,-MeatPerc), y=MeatPerc))

m + geom_bar(stat = "identity", fill="pink", colour="black") +
    ggtitle("Counties with Highest % of Meat Products \n (in terms of no. of products submitted)") +
    ylab("Percentage %") +
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(size=15, angle = 90)) +
    scale_x_discrete(name="")

#Have a look at the countries with the highest % of vegan-labelled products (in terms of number of products in the dataset),
v = ggplot(Results_vegan, aes(x=reorder(Country,-VeganPerc), y=VeganPerc))

v + geom_bar(stat = "identity", fill="dark green", colour="black") +
    ggtitle("Counties with Highest % of Vegan-labelled Products \n (in terms of no. of products submitted)") +
    ylab("Percentage %") +
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(size=15, angle = 90)) +
    scale_x_discrete(name="")

#Note - This makes Spain look obsessed with vegan products. However, it seems to be more of a labelling convention. 
#The Spanish seem to label even the most obviously vegan products as vegan, such as tomatoes!

#What does it look like if we remove Spain?
Results_vegan = Results_vegan[-grep("Spain", Results_vegan$Country),]

#Have a look at the countries with the highest % of vegan-labelled products (in terms of number of products in the dataset),
#with Spain removed,
v = ggplot(Results_vegan, aes(x=reorder(Country,-VeganPerc), y=VeganPerc))

v + geom_bar(stat = "identity", fill="green", colour="black") +
    ggtitle("Counties with Highest % of Vegan-labelled Products \n (in terms of no. of products submitted)") +
    ylab("Percentage %") +
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(size=15, angle = 90)) +
    scale_x_discrete(name="")

###########################