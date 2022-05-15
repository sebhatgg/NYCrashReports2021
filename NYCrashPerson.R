#File imported using import function on the main menu
View(Motor_Vehicle_Collisions_Person)
summary(Motor_Vehicle_Collisions_Person)
mean(Motor_Vehicle_Collisions_Person$PERSON_AGE)

#Data cleaning
NYclean2 <- Motor_Vehicle_Collisions_Person[!is.na(Motor_Vehicle_Collisions_Person$PERSON_AGE),]
NYclean <- NYclean2[!is.na(Motor_Vehicle_Collisions_Person$PERSON_SEX),]

NYclean <- NYclean%>% mutate(agegroup=case_when(PERSON_AGE  >=60 ~ "Over60", PERSON_AGE >= 50 ~ "Btw50and60",PERSON_AGE >= 40 ~ "Btw40and50", PERSON_AGE >= 30 ~ "Btw30and40", PERSON_AGE >=20 ~ "Btw20and30", TRUE ~ "Lt20"))

NYtimeacci <- NYclean%>%
  select(agegroup, COLLISION_ID, CRASH_TIME, AMPM, PERSON_AGE, PERSON_TYPE, PERSON_SEX)%>%
  filter(PERSON_SEX=='M'|PERSON_SEX=='F')%>%
  
  filter(AMPM=='AM' | AMPM=='PM')
NYtimeacci
summary(NYtimeacci)

#t.test
t.test(data=NYtimeacci, PERSON_AGE ~ PERSON_SEX)

t.test(data=NYtimeacci,COLLISION_ID ~ AMPM)

t.test(data=NYtimeacci,PERSON_AGE ~ AMPM)


NYdata1 <- NYclean%>%
  select(agegroup, CRASH_TIME, PERSON_AGE, PERSON_SEX, PERSON_TYPE,COLLISION_ID, AMPM)%>%
  filter(AMPM=='AM' | AMPM=='PM')%>%
  filter(PERSON_AGE > 0)%>%
  filter(PERSON_SEX=='M'|PERSON_SEX=='F')

t.test(data=NYdata1, COLLISION_ID ~ AMPM)
t.test(data=NYdata1, agegroup ~ AMPM)

summary(NYdata1)

TabNY1 <- table(NYdata1$PERSON_SEX, NYdata1$AMPM)
TabNY2 <- table( NYdata1$PERSON_TYPE, NYdata1$PERSON_SEX)
TabNY3 <- table( NYdata1$agegroup, NYdata1$PERSON_SEX)
TabNY2
TabNY1
TabNY3
barplot(TabNY1, beside=T, legend=T)
barplot(TabNY2, beside=T, legend=T)
barplot(TabNY3, beside=T, legend=T, col = "#1b98e0")
mean(TabNY3)

#chi square test
chisq.test(TabNY1, correct = T)
chisq.test(TabNY2, correct = T)
chisq.test(TabNY3, correct = T)
prop.table(TabNY1)
prop.table(TabNY2)
x <- prop.table(TabNY3)
x
NYdata1 %>% ggplot(aes(x = agegroup, y = PERSON_SEX)) + geom_bar(stat = "identity", aes(fill = agegroup))
x %>% ggplot(aes(x = agegroup, y = PERSON_SEX)) + geom_bar(stat = "identity", aes(fill = agegroup))

       