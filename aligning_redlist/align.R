##comparing lists

norway<- read_csv('aligning_redlist/norway_red_list.csv')
ohi<-read_csv('aligning_redlist/ohi_list.csv')
ohi$ohi_present<- 'Y'

compare<- norway %>% left_join(ohi, by='species')

write.csv(compare, 'aligning_redlist/norway_list_compare.csv')

ohi_compare<- ohi %>% left_join(norway, by='species')
write.csv(ohi_compare, 'aligning_redlist/ohi_list_compare.csv')