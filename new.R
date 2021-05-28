x = as.data.frame(summary(vgsales$Genre))
normal = x$`summary(vgsales$Genre)`/ sum(x$`summary(vgsales$Genre)`)
normal = as.data.frame(normal)
normal
head(x,8)
library(tidyverse)

length(unique(vgsales$Genre))
dim(vgsales)

a = vgsales%>%select(Year,Genre)


b = a%>%group_by(Year)%>%
  count(Genre)
view(b)
class(vgsales$Year)
year = vgsales$Year
year
year = as.integer(year)
hist(year,
     col = "darkblue")


any(is.na(vgsales))
Platform_sales <- vgsales %>% group_by(Platform) %>% summarise(sum_global_sales = sum(Global_Sales), .groups = 'drop') %>% 
  arrange(desc(sum_global_sales)) %>%
  mutate(percent = sum_global_sales/sum(sum_global_sales)*100)
view(Platform_sales)

Platform_Satis = vgsales%>%group_by(Platform)%>%
  summarise(toplam_Satis = sum(Global_Sales))%>%
  arrange(desc(toplam_Satis))%>%mutate(yuzde = toplam_Satis/sum(toplam_Satis)*100)
view(Platform_Satis)


ggplot(data= head(Platform_sales, 10), aes(x= "", y=percent, fill = Platform)) +
  geom_bar(stat="identity", color = 'white') +coord_polar("y", start=0)


Year_Sales = vgsales%>%group_by(Year)%>%
  summarise(toplam_satis = sum(Global_Sales),.groups = "drop")%>%
  arrange(desc(toplam_satis))
view(Year_Sales)
nrow(Year_Sales)

max(vgsales$Global_Sales)
which(vgsales$Global_Sales ==max(vgsales$Global_Sales))

Year_Sales = Year_Sales[-40,]

a = max(Year_Sales$toplam_satis)
which(Year_Sales$toplam_satis == a)
Year_Sales[29,]

## Yıllara Göre Satışlar##
fig = ggplot(data = Year_Sales,
             aes(x = Year,
                 y = toplam_satis,group = 1))+geom_line()+geom_point()+theme_light()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1),
        legend.position = "top" )+
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_text(size = 12,
                                  face = "italic",
                                  colour = "darkblue"),
        plot.title = element_text(size = 14,
                                  face = "bold",colour = "darkred"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "aliceblue",size = 2))
fig
#####
### Top_10_Platform Pasta Graph
Platform_sales = vgsales%>%group_by(Platform)%>%
  summarise(toplam_satis = sum(Global_Sales))%>%
  arrange(desc(toplam_satis))%>%
  mutate(percent = toplam_satis/sum(toplam_satis)*100)

fig_2 = ggplot(data = head(Platform_sales,10),
               aes(x = "", y = percent,fill = Platform))+
  geom_bar(stat = "identity",color = "white")+
  coord_polar("y",start = 0)+theme_light()+
  theme(axis.text.x = element_text(angle = 0,vjust = 0.5,hjust = 1),legend.position = "top")+
  theme(panel.grid.major = element_line(linetype = "blank"),
        axis.title = element_text(size = 12,
                                  face = "italic",colour = "cadetblue"),
        plot.title = element_text(size = 14,
                                  face = "bold",colour = "darkred"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "aliceblue",
                                       colour = "aliceblue",size = 1))+
  labs(title = "Global Sales By Percent",y = "",x = "",
       colour = "Blue")

fig_2
#######
##Global Sales by genre
Genre_Sales = vgsales%>%group_by(Genre)%>%
  summarise(toplam_satis = sum(Global_Sales))%>%
  arrange(desc(toplam_satis))

fig_3 = ggplot(data = Genre_Sales,
               aes(x = Genre,y = toplam_satis,fill = Genre))+
  geom_bar(stat = "identity")+geom_label(aes(label = toplam_satis),size = 3)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1),
        legend.position = "top")+theme(panel.grid.major = element_line(linetype = "blank"),
                                       panel.grid.minor = element_line(linetype = "blank"),
                                       axis.title = element_text(size = 12,
                                                                 face = "italic",color = "cadetblue4"))
fig_3


## Top 10 Publisher Horizontol
Publisher_Sales = vgsales%>%group_by(Publisher)%>%
  summarise(toplam_satis = sum(Global_Sales))%>%
  arrange(desc(toplam_satis))
view(Publisher_Sales)

head(Publisher_Sales,10)
tail(Publisher_Sales,10)

fig_4 = ggplot(data = head(Publisher_Sales,10),
               aes(x = Publisher,y = toplam_satis,
                   fill = Publisher))+geom_bar(stat = "identity")+
  geom_label(aes(label = toplam_satis),size = 4.3)+theme_light()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 0,vjust = 0.5,hjust = 1),
        legend.position = "top")+theme(panel.grid.major = element_line(linetype = "blank"),
                                       panel.grid.minor = element_line(linetype = "blank"),
                                       
                                       axis.title = element_text(size = 12,
                                                                 face = "italic",
                                                                 color = "cadetblue4"),
                                       plot.title = element_text(size = 14,
                                                                 face = "bold",
                                                                 colour = "cadetblue"),
                                       panel.background = element_rect(fill = NA),
                                       plot.background = element_rect(fill = "aliceblue",
                                                                      colour = "aliceblue",
                                                                      size = 1))+
  
  labs(title = "Top 10 Publisher",y = "Global Sales",
       colour = "Blue")+theme(legend.background = element_rect(fill = "aliceblue"),
                              legend.position = "right",axis.text.y= element_blank())

fig_4
########################################
## Yıllar Göre Hangi Tür Daha Fazla Satılmış
library(tidyverse)
Genre_Game_num = vgsales%>%group_by(Year,Genre)%>%
  summarise(count_name = length(unique(Name)))%>%
  arrange(desc(Year))


new = ggplot(data = Genre_Game_num,aes(x = Year,y = count_name,fill = Genre))+
  geom_bar(stat = "identity",position = "stack")+theme_light()+
  theme(axis.text.x = element_text(
    angle = 90,vjust = 0.5,hjust =1 
  ))

new
length(unique(vgsales$Name))

view(Genre_Game_num)

