require(dplyr)
require(ggplot2)
cols <- rainbow(16, alpha=0.2)

aa_2015 = read.csv('AA_2015.csv', stringsAsFactors = F)
aa_2016 = read.csv('AA_2016.csv', stringsAsFactors = F)
aa_2017 = read.csv('AA_2017.csv', stringsAsFactors = F)

dt = rbind(aa_2015,aa_2016,aa_2017)

by_month = dt %>%
  group_by(Month.of.Incident) %>%
  summarize(mean_victims = mean(number_of_victims, na.rm = TRUE))

ggplot(by_month, aes(x=Month.of.Incident, y=mean_victims, fill=Month.of.Incident))+
  geom_bar(stat="identity", color="black")+
  scale_fill_manual(values=c(cols))+
  theme_minimal()


by_cow = dt %>%
  group_by(cow_related_violence) %>%
  summarize(mean_victims = mean(number_of_victims, na.rm = TRUE))

ggplot(by_cow, aes(x=cow_related_violence, y=mean_victims, fill=cow_related_violence))+
  geom_bar(stat="identity", color="black")+
  scale_fill_manual(values=c(cols))+
  theme_minimal()



by_party = dt %>%
  group_by(party_in_power) %>%
  summarize(mean_victims = mean(number_of_victims, na.rm = TRUE))

ggplot(by_party, aes(x=party_in_power, y=mean_victims, fill=party_in_power))+
  geom_bar(stat="identity", color="black")+
  scale_fill_manual(values=c(cols))+
  theme_minimal()





muslim = data.frame(number_of_victims = dt$number_of_victims[dt$identity_muslim == 'Yes'], rel = 'muslim')
christian = data.frame(number_of_victims = dt$number_of_victims[dt$identity_christian == 'Yes'], rel = 'christian')
dalit = data.frame(number_of_victims = dt$number_of_victims[dt$identity_dalit == 'Yes'], rel = 'dalit')

by_rel_no = rbind(muslim,christian,dalit)

boxplot(number_of_victims ~ rel, data = by_rel_no)



muslim = data.frame(victim_ages = dt$victim_ages[dt$identity_muslim == 'Yes'], rel = 'muslim')
christian = data.frame(victim_ages = dt$victim_ages[dt$identity_christian == 'Yes'], rel = 'christian')
dalit = data.frame(victim_ages = dt$victim_ages[dt$identity_dalit == 'Yes'], rel = 'dalit')

by_rel_age = rbind(muslim,christian,dalit)

boxplot(victim_ages ~ rel, data = by_rel_age)





plot(dt$longitude, dt$latitude)



table(dt$state)














