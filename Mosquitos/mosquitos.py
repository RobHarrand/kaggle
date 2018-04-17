# -*- coding: utf-8 -*-
"""
Created on Wed Oct 19 12:32:31 2016

@author: rob.harrand
"""

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import os
cwd = os.getcwd()

df = pd.read_csv('C:\\Users\\rob.harrand\\Desktop\\WORK\\Kaggle\\Python\\Mosquitos\\aegypti_albopictus.csv')

df.info()

#Pie chart...

x = df.iloc[:,0].tolist() #Get the list of names
a_alb = x.count('Aedes albopictus') #Count how many 
alb_pc = round(a_alb/len(x)*100,2) #Percentage of Aedes albopictus cases

a_aeg = x.count('Aedes aegypti') #Count how many
aeg_pc = round(a_aeg/len(x)*100,2) #Percentage of Aedes aegypti cases

labels = ['Aedes albopictus', 'Aedes aegypti']
cols = ['green', 'lightblue']
sizes = [alb_pc,aeg_pc]

plt.pie(sizes, labels=labels, autopct='%1.1f%%', shadow=True, colors = cols)



#Barplot

country_group = df.groupby(['COUNTRY']).count()
country_group.iloc[:,1] = country_group.iloc[:,1].astype(float)

country_group_sorted = country_group.sort('VECTOR', ascending = 0)
countries = country_group_sorted.index.tolist()

country_group_sorted_sub = country_group_sorted.iloc[0:50,:]
countries_sub = countries[0:50]

p = sns.barplot(x=countries_sub, y=country_group_sorted_sub.iloc[:,0])
p.set(yscale="log")
p.set_xticklabels(p.get_xticklabels(), rotation=90)
p.set(ylabel='Log No. of Cases')



#Taiwan breakdown
taiwan = df.loc[df['COUNTRY'] == 'Taiwan']
taiwan['YEAR'] = taiwan['YEAR'].astype(float)

max(taiwan['YEAR'])
min(taiwan['YEAR'])

#Are there any missing values in the year data?
taiwan['YEAR'].isnull().value_counts()

#Get rid of them...
taiwan_years = taiwan['YEAR'].dropna()

#Plot the year distribution for Taiwan...
taiwan_years = taiwan_years.astype(int)
p = sns.countplot(taiwan_years)
p.set_xticklabels(p.get_xticklabels(), rotation=90)
#sns.distplot(taiwan_years)



#Kenya breakdown
kenya = df.loc[df['COUNTRY'] == 'Kenya']
kenya['YEAR'] = kenya['YEAR'].astype(float)

#Are there any missing values in the year data?
kenya['YEAR'].isnull().value_counts()

#Get rid of them...
kenya_years = kenya['YEAR'].dropna()

#Plot the year distribution for Taiwan...
kenya_years = kenya_years.astype(int)
p = sns.countplot(kenya_years)
p.set_xticklabels(p.get_xticklabels(), rotation=90)


p = sns.distplot(kenya_years.astype(float), bins=len(set(kenya_years)))


