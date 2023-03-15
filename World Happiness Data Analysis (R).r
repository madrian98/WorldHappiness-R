library(dplyr) 
library(janitor)
library(gridExtra)
library(reshape2)
library(png)
library(stringr)
library(plotly)
library(ggplot2) 
library(rworldmap)
library(gganimate)
library(ggalt)
library(ggthemes)
library(ggpubr)
library(corrplot)
library(viridis)
library(IRdisplay)
library(leaflet)
library(maps)
display_html("<style>.container { width:100% !important; }</style>")
options(warn=-1)

library(caTools)#regression/prediction
library(e1071)#regression
library(rpart)#regression
library(rpart.plot)#regression
library(randomForest)#regression
library(caret)#regression (main)
library(glmnet)#regression
library(xgboost)#regression
library(elasticnet)#regression
library(kknn)#regression
library(arm)#regression
library(xgboost)#regression
library(factoextra)#clustering
library(cluster)#clustering
library(dbscan)#dbscan cluster
library(fpc)#dbscan cluster
library(kernlab)#Spectral cluster
library(mclust)#GMM cluster

data2021 <- read.csv("https://github.com/madrian98/WorldHappiness-R/blob/main/Data/2021.csv?raw=true",header=TRUE, sep = ",")

str(data2021)

summary(data2021)

sum(is.na(data2021))
sum(duplicated(data2021))

head(data2021,10)

data2021 <- data2021 %>% rename("Happiness_score"="Ladder.score","Happiness_score_in_Dystopia"="Ladder.score.in.Dystopia","Country_name"="Country.name")
str(data2021)

dataH <- read.csv("https://github.com/madrian98/WorldHappiness-R/blob/main/Data/HistoricalData.csv?raw=true",header=TRUE,sep= ';',dec= ',')

str(dataH)

summary(dataH)

sum(is.na(dataH))
sum(duplicated(dataH))

head(dataH,10)

dataH <- dataH %>% rename("Happiness_score"="Life.Ladder","Country_name"="Country.name")
dataH <-dataH[order(dataH$Country_name, dataH$year),]
str(dataH)

str(data2021)

reg_21<-data2021 %>% dplyr::select(Regional.indicator,Happiness_score ) %>% group_by(Regional.indicator)%>% summarize(
Happiness_score=(count=n()))%>% ungroup() %>% mutate(perc = `Happiness_score` / sum(`Happiness_score`)) %>% arrange(perc) %>% mutate(udzial_procentowy = scales::percent(perc))
head(reg_21,10)

bp<- ggplot(reg_21, aes(x="", y=Happiness_score, fill=Regional.indicator))+
    geom_bar(stat = "identity")+ geom_label(aes(x=2.0,label = udzial_procentowy),
             position = position_stack(vjust = 0.28),
show.legend = FALSE)+
guides(fill = guide_legend(title = "Region")) +
theme_bw() +
  theme(axis.text.x = element_text(size = 15,face = "bold"), axis.title.x = element_text(size = 15,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)  
bp + coord_polar("y")+labs(title="% of countries in each region",y="%")
options(repr.plot.width=25, repr.plot.height=15)

regions <- data2021 %>%
  dplyr::select(2,3,7,8,9,10,11,12,20) %>%
  group_by(Regional.indicator) %>%
  summarise_if(is.numeric, funs(mean)) %>%
  arrange(desc(Happiness_score))

head(regions,10)

regions.a <- melt(regions[, -c(4,5,6,7,8)])
regions.b <- melt(regions[, -c(2,3,5,9)])
regions.c <- melt(regions[, -c(2,3,4,6,7,8,9)])
p <- ggplot(regions.a, aes(x = Regional.indicator, -value, y = value, fill = Regional.indicator)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~variable) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7,face = "bold"), axis.title.x = element_text(size = 7,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+
  labs(title = "Average values for each region") +
  guides(fill = guide_legend(title = "Region"))+ 
  xlab('') + ylab('')
p
p <- ggplot(regions.b, aes(x = Regional.indicator, -value, y = value, fill = Regional.indicator)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~variable) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7,face = "bold"), axis.title.x = element_text(size = 7,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+
  guides(fill = guide_legend(title = "Region"))+ 
  xlab('') + ylab('')
p
p <- ggplot(regions.c, aes(x = Regional.indicator, -value, y = value, fill = Regional.indicator)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~variable) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7,face = "bold"), axis.title.x = element_text(size = 7,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+
  guides(fill = guide_legend(title = "Region"))+ 
  xlab('') + ylab('')
p
options(repr.plot.width=45, repr.plot.height=20)

data2021 %>%
ggplot(aes(x=Regional.indicator,y=Happiness_score,fill=Regional.indicator))+
geom_boxplot()+
labs(title = "Happiness score in regions", 
         y = "Happiness score" , x = "Region")+
theme_bw() +
  theme(axis.text.x = element_text(size = 13,face = "bold"), axis.title.x = element_text(size = 13,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
    scale_fill_viridis(option = "D",discrete = T,name = "Region")+
coord_flip()
options(repr.plot.width=35, repr.plot.height=20)

# GDP

data2021 %>%
ggplot(aes(x=Regional.indicator,y=Logged.GDP.per.capita,fill=Regional.indicator))+
geom_boxplot()+
labs(title = "Distribution of logged GDP per capita in each region", 
         y = "Logged GDP per capita" , x = "Region")+
theme_bw() +
  theme(axis.text.x = element_text(size = 13,face = "bold"), axis.title.x = element_text(size = 13,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
    scale_fill_viridis(option = "D",discrete = T,name = "Region")+
coord_flip()
options(repr.plot.width=35, repr.plot.height=20)

# Social support

data2021 %>%
ggplot(aes(x=Regional.indicator,y=Social.support,fill=Regional.indicator))+
geom_boxplot()+
labs(title = "Distribution of social support in each region", 
         y = "Social support" , x = "Region")+
theme_bw() +
    theme(axis.text.x = element_text(size = 13,face = "bold"), axis.title.x = element_text(size = 13,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
    scale_fill_viridis(option = "D",discrete = T,name = "Region")+
coord_flip()
options(repr.plot.width=35, repr.plot.height=20)

#Healthy.life.expectancy

data2021 %>%
ggplot(aes(x=Regional.indicator,y=Healthy.life.expectancy,fill=Regional.indicator))+
geom_boxplot()+
labs(title = "Distribution of healty life expectancy in each region", 
         y = "Healty life expectancy" , x = "Region")+
theme_bw() +
    theme(axis.text.x = element_text(size = 13,face = "bold"), axis.title.x = element_text(size = 13,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
    scale_fill_viridis(option = "D",discrete = T,name = "Region")+
coord_flip()
options(repr.plot.width=35, repr.plot.height=20)

#Freedom to make life choices

data2021 %>%
ggplot(aes(x=Regional.indicator,y=Freedom.to.make.life.choices,fill=Regional.indicator))+
geom_boxplot()+
labs(title = "Distribution of freedom to make life choices in each region", 
         y = "Freedom to make life choices" , x = "Region")+
theme_bw() +
   theme(axis.text.x = element_text(size = 13,face = "bold"), axis.title.x = element_text(size = 13,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
    scale_fill_viridis(option = "D",discrete = T,name = "Region")+
coord_flip()
options(repr.plot.width=35, repr.plot.height=20)

#Generosity

data2021 %>%
ggplot(aes(x=Regional.indicator,y=Generosity,fill=Regional.indicator))+
geom_boxplot()+
labs(title = "Distribution of generosity in each region", 
         y = "Generosity" , x = "Region")+
theme_bw() +
    theme(axis.text.x = element_text(size = 13,face = "bold"), axis.title.x = element_text(size = 13,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
    scale_fill_viridis(option = "D",discrete = T,name = "Region")+
coord_flip()
options(repr.plot.width=35, repr.plot.height=20)

#Perceptions of corruption

data2021 %>%
ggplot(aes(x=Regional.indicator,y=Perceptions.of.corruption,fill=Regional.indicator))+
geom_boxplot()+
labs(title = "Distribition of perceptions of corruption in each region", 
         y = "Perceptions of corruption" , x = "Region")+
theme_bw() +
  theme(axis.text.x = element_text(size = 13,face = "bold"), axis.title.x = element_text(size = 13,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
   scale_fill_viridis(option = "D",discrete = T,name = "Region")+
coord_flip()
options(repr.plot.width=35, repr.plot.height=20)

#Dystopia+Residual

data2021 %>%
ggplot(aes(x=Regional.indicator,y=Dystopia...residual,fill=Regional.indicator))+
geom_boxplot()+
labs(title = "Distribution of Dystopia+residual in each region", 
         y = "Dystopia+Residual" , x = "Region")+
theme_bw() +
    theme(axis.text.x = element_text(size = 13,face = "bold"), axis.title.x = element_text(size = 13,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T,name = "Region")+
coord_flip()
options(repr.plot.width=25, repr.plot.height=15)

data2021 %>% 
  top_n(10,Happiness_score) %>%
  mutate(Country_name=reorder(Country_name,Happiness_score))%>%
  ggplot(aes(x=Country_name,y=Happiness_score,fill=Happiness_score))+
  geom_bar(stat = "identity")+
  xlab("Country")+
  ylab("Happiness score")+
  ggtitle("Countries with highest happiness score")+
  coord_flip()+
 theme_bw() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20),legend.title=element_text(size=16))+
  scale_fill_viridis(option = "D",name = "Happiness score")
options(repr.plot.width=35, repr.plot.height=20)

data2021 %>% 
  top_n(-10,Happiness_score) %>%
  mutate(Country_name=reorder(Country_name,Happiness_score))%>%
  ggplot(aes(x=Country_name,y=Happiness_score,fill=Happiness_score))+
  geom_bar(stat = "identity")+
  xlab("Country")+
  ylab("Happiness score")+
  ggtitle("Countries with lowest happiness score")+
  coord_flip()+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20),legend.title=element_text(size=16))+
  scale_fill_viridis(option = "D",name = "Happiness score")
options(repr.plot.width=35, repr.plot.height=20)

top10<-data2021 %>% dplyr::select(Country_name,14,15,16,17,18,19,20) %>% head(n=10)
                                                                                                          
top10 %>%
  group_by(Country_name)%>%
  summarise_each(list(mean)) %>%
  melt(id="Country_name")%>%
  ggplot(aes(x=Country_name,y=value,fill=variable))+
  geom_bar(position="fill",stat="identity")+
  labs(title = "Variables influence of countries with highest happiness score", 
         y = "%" , x = "Country")+
 coord_flip()+
 theme_bw() +
  theme(axis.text.x = element_text(size = 7,face = "bold"), axis.title.x = element_text(size = 7,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T,name = "Variable")

options(repr.plot.width=35, repr.plot.height=20)

bottom10<-data2021 %>% dplyr::select(Country_name,14,15,16,17,18,19,20) %>% tail(n=10) 

bottom10 %>%
  group_by(Country_name)%>%
  summarise_each(list(mean)) %>%
  melt(id="Country_name")%>%
  ggplot(aes(x=Country_name,y=value,fill=variable))+
  geom_bar(position="fill",stat="identity")+
  labs(title = "Variables influence of countries with lowest happiness score", 
         y = "%" , x = "Country")+
 coord_flip()+
  theme_bw() +
  theme(axis.text.x = element_text(size = 7,face = "bold"), axis.title.x = element_text(size = 7,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T,name = "Variable")
options(repr.plot.width=35, repr.plot.height=20)

data2021 %>%
  dplyr::select(2,14,15,16,17,18,19,20)%>%
  group_by(Regional.indicator)%>%
  summarise_each(list(mean)) %>%
  melt(id="Regional.indicator")%>%
  ggplot(aes(x=Regional.indicator,y=value,fill=variable))+
  geom_bar(position="fill",stat="identity")+
  labs(title = "Variables influence of regions", 
         y = "%" , x = "Region")+
 coord_flip()+
theme(legend.text = element_text(size = 25))+
   theme_bw() +
  theme(axis.text.x = element_text(size = 7,face = "bold"), axis.title.x = element_text(size = 7,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T,name = "Variable")
options(repr.plot.width=35, repr.plot.height=20)

# GDP

cbbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#A020F0")
ggplot(data2021,aes(x=Happiness_score,y=Logged.GDP.per.capita,color=Regional.indicator,size=Logged.GDP.per.capita))+geom_point(alpha=0.8)+labs(title="Relation between happiness score and Logged GDP", y = "Logged GDP" , x = "Happiness score",size="Logged GDP",color="Region")+theme_bw() +
theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+
 scale_colour_manual(values=cbbPalette)

# Social support

cbbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#A020F0")
ggplot(data2021,aes(x=Happiness_score,y=Social.support,color=Regional.indicator,size=Social.support))+geom_point(alpha=0.8)+labs(title="Relation between happiness score and social support", y = "Social support" , x = "Happiness score",size="Social support",color="Region")+theme_bw() +
  theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+
 scale_colour_manual(values=cbbPalette)

# Healthy life expectancy

cbbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#A020F0")
ggplot(data2021,aes(x=Happiness_score,y=Healthy.life.expectancy,color=Regional.indicator,size=Healthy.life.expectancy))+geom_point(alpha=0.8)+labs(title="Relation between happiness score and healthy life expectancy", y = "Healthy life expectancy" ,x = "Happiness score",size="Healthy life expectancy",color="Region")+theme_bw() +
  theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+
 scale_colour_manual(values=cbbPalette)

#Freedom to make life choices

cbbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#A020F0")
ggplot(data2021,aes(x=Happiness_score,y=Freedom.to.make.life.choices,color=Regional.indicator,size=Freedom.to.make.life.choices))+geom_point(alpha=0.8)+labs(title="Relation between happiness score and freedom to make life choices", y = "Freedom to make life choices" , x = "Happiness score",size="Freedom to make life choices",color="Region")+theme_bw() +
theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+
 scale_colour_manual(values=cbbPalette)

#Generosity

cbbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#A020F0")
ggplot(data2021,aes(x=Happiness_score,y=Generosity,color=Regional.indicator,size=Generosity))+geom_point(alpha=0.8)+labs(title="Relation between happiness score and generosity", y = "Generosity" , x = "Happiness score",size="Generosity",color="Region")+theme_bw() +
  theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+
 scale_colour_manual(values=cbbPalette)

#Perceptions of corruption

cbbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#A020F0")
ggplot(data2021,aes(x=Happiness_score,y=Perceptions.of.corruption,color=Regional.indicator,size=Perceptions.of.corruption))+geom_point(alpha=0.8)+labs(title="Relation between happiness score and perceptions of corruption", y = "Perceptions of corruption" , x = "Happiness score",size="Perceptions of corruption",color="Region")+theme_bw() +
   theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+
 scale_colour_manual(values=cbbPalette)

#Dystopia...residual

cbbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#A020F0")
ggplot(data2021,aes(x=Happiness_score,y=Dystopia...residual,color=Regional.indicator,size=Dystopia...residual))+geom_point(alpha=0.8)+labs(title="Relation between happiness score and Dystopia + residual", y = "Dystopia+Residual" , x = "Happiness score",size="Dystopia+Residual",color="Region")+theme_bw() +
    theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 20,face = "bold"),legend.title=element_text(size=16,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+
 scale_colour_manual(values=cbbPalette)

data2021 %>% dplyr::select(3,7,8,9,10,11,12,20) %>% cor() %>% 
  corrplot::corrplot(method = "number",type = "lower",tl.cex = 1,number.cex = 1,cl.cex = 1,title="2021 data correlation", sig.level = 0.05, insig = "blank",mar=c(0,0,1,0),col = c('white', 'black'), bg = 'gold2' )
options(repr.plot.width=30, repr.plot.height=30)

data2021$Country_name[data2021$Country_name=="Taiwan Province of China"]<-"Taiwan"
data2021$Country_name[data2021$Country_name=="Hong Kong S.A.R. of China"]<-"Hong Kong"
data2021$Country_name[data2021$Country_name=="North Macedonia"]<-"Macedonia"
data2021$Country_name[data2021$Country_name=="Palestinian Territories"]<-"Palestine"
data2021$Country_name[data2021$Country_name=="North Cyprus"]<-"Cyprus"

df <- data.frame(country = data2021$Country_name,
  value = data2021$Happiness_score)

j <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "country")

mapCountryData(j, nameColumnToPlot = "value", 
              mapTitle = "World map - happiness score", 
               colourPalette = 'rainbow')
options(repr.plot.width=25, repr.plot.height=15)

df <- data.frame(country = data2021$Country_name,
  value = data2021$Social.support)

j <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "country")

mapCountryData(j, nameColumnToPlot = "value", 
               mapTitle = "World map - social support", 
               colourPalette = 'rainbow')
options(repr.plot.width=25, repr.plot.height=15)

df <- data.frame(country = data2021$Country_name,
  value = data2021$Logged.GDP.per.capita)

j <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "country")

mapCountryData(j, nameColumnToPlot = "value", 
               mapTitle = "World map - logged GDP per capita", 
               colourPalette = 'rainbow')
options(repr.plot.width=25, repr.plot.height=15)

df <- data.frame(country = data2021$Country_name,
  value = data2021$Healthy.life.expectancy)

j <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "country")

mapCountryData(j, nameColumnToPlot = "value", 
               mapTitle = "World map - healthy life expectancy", 
               colourPalette = 'rainbow')
options(repr.plot.width=25, repr.plot.height=15)

df <- data.frame(country = data2021$Country_name,
  value = data2021$Freedom.to.make.life.choices)

j <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "country")

mapCountryData(j, nameColumnToPlot = "value", 
               mapTitle = "World map - freedom to make life choices", 
               colourPalette = 'rainbow')
options(repr.plot.width=25, repr.plot.height=15)

df <- data.frame(country = data2021$Country_name,
  value = data2021$Generosity)

j <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "country")

mapCountryData(j, nameColumnToPlot = "value", 
               mapTitle = "World map - generosity", 
               colourPalette = 'rainbow')
options(repr.plot.width=25, repr.plot.height=15)

df <- data.frame(country = data2021$Country_name,
  value = data2021$Perceptions.of.corruption)

j <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "country")

mapCountryData(j, nameColumnToPlot = "value", 
               mapTitle = "World map - perceptions of corruption", 
               colourPalette = 'rainbow')
options(repr.plot.width=25, repr.plot.height=15)

df <- data.frame(country = data2021$Country_name,
  value = data2021$Dystopia...residual)

j <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "country")

mapCountryData(j, nameColumnToPlot = "value", 
               mapTitle = "World map - Dystopia + residual", 
               colourPalette = 'rainbow')
options(repr.plot.width=25, repr.plot.height=15)

data(world.cities)

dfLeaf <- world.cities %>%
    filter(capital == 1) %>%
    dplyr::select(country = country.etc, lat, lng = long) %>%
    inner_join(data2021,by=c("country"="Country_name"))

p<-leaflet(dfLeaf)%>%
addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(lat=dfLeaf$lat, lng=dfLeaf$lng, clusterOptions = markerClusterOptions(),
             popup= paste(dfLeaf$Happiness_score,
        "<br><strong>Country name: </strong>", dfLeaf$country,
        "<br><strong>Happiness_score: </strong>", dfLeaf$Happiness_score
          ))
htmlwidgets::saveWidget(p, 'map.html', selfcontained = FALSE)
IRdisplay::display_html('<iframe width=100% height=1000 src="./map.html"></iframe>')

codes <- read.csv('https://github.com/madrian98/WorldHappiness-R/blob/main/Data/plotly_countries_and_codes.csv?raw=true')
colnames(codes)[1] <- 'Country_name'

dfPlotmap <- merge(x = data2021, y = codes, by = 'Country_name', all.x = TRUE)
dfPlotmap$CODE <- as.character(dfPlotmap$CODE)
head(dfPlotmap,10)

which(is.na(dfPlotmap$CODE))
dfPlotmap$Country[c(29, 44, 53,63, 94,103, 124,130)]

dfPlotmap$CODE[c(29, 44, 53,63, 94, 103,124,130)] <- c("COG", "GMB", "HKG","CIV","MMR","MKD","KOR","TWN")

dfPlotmapH <- merge(x = dataH, y = codes, by = 'Country_name', all.x = TRUE)
dfPlotmapH$CODE <- as.character(dfPlotmapH$CODE)
dfPlotmapH <-dfPlotmapH[order(dfPlotmapH$Country_name, dfPlotmapH$year),]
head(dfPlotmapH,10)

p <- plot_geo(dfPlotmapH, locationmode = 'ISO-3', colorscale="Rainbow",colorbar = list(title = "Happiness score"),width = 1500,height=1000,title_font_size=22) %>%
add_trace( z = ~dfPlotmapH$Happiness_score, locations = ~dfPlotmapH$CODE, frame=~dfPlotmapH$year) %>%
layout(title = 'Happiness score 2005-2021')
embed_notebook(p)

p <- plot_geo(dfPlotmapH, locationmode = 'ISO-3', colorscale="Rainbow",colorbar = list(title = "Logged GDP per capita"),width = 1500,height=1000,title_font_size=22) %>%
add_trace( z = ~dfPlotmapH$Log.GDP.per.capita, locations = ~dfPlotmapH$CODE, frame=~dfPlotmapH$year) %>%
 layout(title = 'Logged GDP per capita 2005-2021')
embed_notebook(p)

p <- plot_geo(dfPlotmapH, locationmode = 'ISO-3', colorscale="Rainbow",colorbar = list(title = "Social support"),width = 1500,height=1000,title_font_size=22) %>%
add_trace( z = ~dfPlotmapH$Social.support, locations = ~dfPlotmapH$CODE, frame=~dfPlotmapH$year) %>%
 layout(title = 'Social support 2005-2021')
embed_notebook(p)

p <- plot_geo(dfPlotmapH, locationmode = 'ISO-3', colorscale="Rainbow",colorbar = list(title = "Healthy life expectancy"),width = 1500,height=1000,title_font_size=22) %>%
add_trace( z = ~dfPlotmapH$Healthy.life.expectancy.at.birth, locations = ~dfPlotmapH$CODE, frame=~dfPlotmapH$year) %>%
 layout(title = 'Healthy life expectancy 2005-2021')
embed_notebook(p)

p <- plot_geo(dfPlotmapH, locationmode = 'ISO-3', colorscale="Rainbow",colorbar = list(title = "Freedom to make life choices"),width = 1500,height=1000,title_font_size=22) %>%
add_trace( z = ~dfPlotmapH$Freedom.to.make.life.choices, locations = ~dfPlotmapH$CODE, frame=~dfPlotmapH$year) %>%
 layout(title = 'Freedom to make life choices 2005-2021')
embed_notebook(p)

p <- plot_geo(dfPlotmapH, locationmode = 'ISO-3', colorscale="Rainbow",colorbar = list(title = "Generosity"),width = 1500,height=1000,title_font_size=22) %>%
add_trace( z = ~dfPlotmapH$Generosity, locations = ~dfPlotmapH$CODE, frame=~dfPlotmapH$year) %>%
 layout(title = 'Generosity 2005-2021')
embed_notebook(p)

p <- plot_geo(dfPlotmapH, locationmode = 'ISO-3', colorscale="Rainbow",colorbar = list(title = "Perceptions of corruption"),width = 1500,height=1000,title_font_size=22) %>%
add_trace( z = ~dfPlotmapH$Perceptions.of.corruption, locations = ~dfPlotmapH$CODE, frame=~dfPlotmapH$year) %>%
 layout(title = 'Perceptions of corruption 2005-2021')
embed_notebook(p)

dataML <- data2021 %>% dplyr::select(3,7,8,9,10,11,12,20)
head(dataML,10)

split = sample.split(dataML$Happiness_score, SplitRatio=0.8)
training_set = subset(dataML, split == TRUE)
test_set = subset(dataML, split == FALSE)
preProcValues <- preProcess(training_set, method = c("center", "scale"))
training_set <- predict(preProcValues, training_set)
test_set <- predict(preProcValues, test_set)

set.seed(123)
regressor_lm = lm(formula = Happiness_score ~ .,data = training_set)
y_pred_lm = predict(regressor_lm, newdata = test_set)
Pred_Actual_lm <- as.data.frame(cbind(Prediction = y_pred_lm, Actual = test_set$Happiness_score))
summary(regressor_lm)

gg.lm <- ggplot(Pred_Actual_lm, aes(Actual, Prediction )) + theme_bw() + geom_abline() +
  labs(title = "Linear Regression", x = "Real results",
       y = "Linear regression results") +
        theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 25,face = "bold"),legend.title=element_text(size=25,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+geom_point(colour = "red", size = 5)
gg.lm

set.seed(123)
regressor_svr = train(Happiness_score ~ .,training_set,method='svmLinear')
y_pred_svr = predict(regressor_svr,  newdata = test_set)
Pred_Actual_svr <- as.data.frame(cbind(Prediction = y_pred_svr, Actual = test_set$Happiness_score))
summary(regressor_svr)

gg.svr <- ggplot(Pred_Actual_svr, aes(Actual, Prediction ))  + theme_bw() + geom_abline() +
  labs(title = "SVR Regression", x = "Real results",
       y = "SVR Regression results") +
         theme_bw() +
theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 25,face = "bold"),legend.title=element_text(size=25,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+geom_point(colour = "red", size = 5)
gg.svr

set.seed(123)
regressor_dt = train(Happiness_score ~ .,training_set,method='rpart')
y_pred_dt = predict(regressor_dt, newdata = test_set)
Pred_Actual_dt <- as.data.frame(cbind(Prediction = y_pred_dt, Actual = test_set$Happiness_score))

gg.dt <- ggplot(Pred_Actual_dt, aes(Actual, Prediction )) + theme_bw() + geom_abline() +
  labs(title = "Decision Tree Regression", x = "Real results",
       y = "Decision Tree Regression Results ") +
                theme_bw() +
theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 25,face = "bold"),legend.title=element_text(size=25,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+geom_point(colour = "red", size = 5)
gg.dt

set.seed(123)
regressor_rf = train(Happiness_score ~ .,training_set,method='rf')
y_pred_rf = predict(regressor_rf, newdata = test_set)
Pred_Actual_rf <- as.data.frame(cbind(Prediction = y_pred_rf, Actual = test_set$Happiness_score))
summary(regressor_rf)

gg.rf <- ggplot(Pred_Actual_rf, aes(Actual, Prediction )) +theme_bw() + geom_abline() +
  labs(title = "Random Forest Regression", x = "Real results",
       y = "Random Forest Regression results") +
        theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 25,face = "bold"),legend.title=element_text(size=25,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+geom_point(colour = "red", size = 5)
gg.rf

set.seed(123)
regressor_en <- train(Happiness_score ~ .,training_set,method='glmnet')
y_pred_en = predict(regressor_en,  newdata = test_set)
Pred_Actual_en <- as.data.frame(cbind(Prediction = y_pred_en, Actual = test_set$Happiness_score))
summary(regressor_en)

gg.en <- ggplot(Pred_Actual_en, aes(Actual, Prediction )) +
 theme_bw() + geom_abline() +
  labs(title = "Elastic Net Regression", x = "Real results",
       y = "Elastic Net Regression results") +
       theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 25,face = "bold"),legend.title=element_text(size=25,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+geom_point(colour = "red", size = 5)
gg.en

set.seed(123)
regressor_ridg <- train(Happiness_score ~ .,training_set,method='ridge')
y_pred_ridg = predict(regressor_ridg,  newdata = test_set)
Pred_Actual_ridg <- as.data.frame(cbind(Prediction = y_pred_ridg, Actual = test_set$Happiness_score))
summary(regressor_ridg)

gg.ridg <- ggplot(Pred_Actual_ridg, aes(Actual, Prediction )) + theme_bw() + geom_abline() +
  labs(title = "Ridge Regression", x = "Real results",
       y = "Ridge regression results") +
       theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 25,face = "bold"),legend.title=element_text(size=25,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+geom_point(colour = "red", size = 5)
gg.ridg

set.seed(123)
regressor_knn <- train(Happiness_score ~ .,training_set,method='kknn')
y_pred_knn = predict(regressor_knn,  newdata = test_set)
Pred_Actual_knn <- as.data.frame(cbind(Prediction = y_pred_knn, Actual = test_set$Happiness_score))
summary(regressor_knn)

gg.knn <- ggplot(Pred_Actual_knn, aes(Actual, Prediction )) +theme_bw() + geom_abline() +
  labs(title = "K Nearest Neighbors Regression ", x = "Real results",
       y = "K Nearest Neighbors Regression results") +
        theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 25,face = "bold"),legend.title=element_text(size=25,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+geom_point(colour = "red", size = 5)
gg.knn

set.seed(123)
regressor_bay <- train(Happiness_score ~ .,training_set,method='bayesglm')
y_pred_bay = predict(regressor_bay,  newdata = test_set)
Pred_Actual_bay <- as.data.frame(cbind(Prediction = y_pred_bay, Actual = test_set$Happiness_score))
summary(regressor_bay)

gg.bayes <- ggplot(Pred_Actual_bay, aes(Actual, Prediction )) + theme_bw() + geom_abline() +
  labs(title = "Bayessian Ridge Regression ", x = "Real results",
       y = "Bayessian Ridge Regression results") +
        theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 25,face = "bold"),legend.title=element_text(size=25,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+geom_point(colour = "red", size = 5)
gg.bayes

set.seed(123)
regressor_xgb <- train(Happiness_score ~ .,training_set,method='xgbLinear')
y_pred_xgb = predict(regressor_xgb,  newdata = test_set)
Pred_Actual_xgb <- as.data.frame(cbind(Prediction = y_pred_xgb, Actual = test_set$Happiness_score))
summary(regressor_xgb)

gg.xgb <- ggplot(Pred_Actual_xgb, aes(Actual, Prediction )) + theme_bw() + geom_abline() +
  labs(title = "XGB Regression ", x = "Real results",
       y = "XGB Regression results") +
        theme(axis.text.x = element_text(size = 14,face = "bold"), axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"), axis.title.y = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
       legend.text = element_text(size = 25,face = "bold"),legend.title=element_text(size=25,face = "bold"))+
  scale_fill_viridis(option = "D",discrete = T)+geom_point(colour = "red", size = 5)
gg.xgb

ggarrange(gg.lm, gg.svr, gg.dt, gg.rf,gg.en,gg.ridg,gg.knn,gg.bayes,gg.xgb , ncol = 3, nrow = 3)
options(repr.plot.width=50, repr.plot.height=45)

dfClusters <- dfPlotmap%>% dplyr::select(3,7,8,9,10,11,12,20)
dfClusters <- scale(dfClusters)

k = 8 # number of clusters
clusters <- kmeans(dfClusters, k)
df$Cluster1 <- as.factor(clusters$cluster)

dfPlotmap%>% dplyr::select(3,7,8,9,10,11,12,20) %>%
  mutate(Cluster = df$Cluster1) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

p<-plot_ly(type = 'choropleth', text = dfPlotmap$Country_name, locations = dfPlotmap$CODE, colorscale="Rainbow",
        colorbar = list(title = "Cluster group"),
        locationmode = 'ISO-3', z = df$Cluster1, width = 1500,height=1000) %>% 
        layout(title = 'K-means clustering',
         geo = list(showframe = TRUE, showcoastlines = TRUE,showcountries=TRUE),
         margin = list(l = 0, r = 0, b = 0, t = 30,projection = list(type = 'Mercator')),title_font_size=22,
            autosize = F)
embed_notebook(p)

d <- dist(dfClusters, method = 'euclidean')
fit <- hclust(d, method = 'ward.D')
groups <- cutree(fit, k = 8)
df$Cluster2 <- groups

dfPlotmap%>% dplyr::select(3,7,8,9,10,11,12,20) %>%
  mutate(Cluster = df$Cluster2) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

p<-plot_ly(type = 'choropleth', text = dfPlotmap$Country_name, locations = dfPlotmap$CODE, colorscale="Rainbow",
        colorbar = list(title = "Cluster group"),
        locationmode = 'ISO-3', z = df$Cluster2, width = 1500,height=1000) %>% 
        layout(title = 'Hierarchical Agglomerative clustering',
         geo = list(showframe = TRUE, showcoastlines = TRUE,showcountries=TRUE),
         margin = list(l = 0, r = 0, b = 0, t = 30,projection = list(type = 'Mercator')),title_font_size=22,
            autosize = F)
embed_notebook(p)

fit <- hkmeans(dfClusters, 8)
df$Cluster3 <- fit$cluster

dfPlotmap%>% dplyr::select(3,7,8,9,10,11,12,20) %>%
  mutate(Cluster = df$Cluster3) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

p<-plot_ly(type = 'choropleth', text = dfPlotmap$Country_name, locations = dfPlotmap$CODE, colorscale="Rainbow",
        colorbar = list(title = "Cluster group"),
        locationmode = 'ISO-3', z = df$Cluster3, width = 1500,height=1000) %>% 
        layout(title = 'Hierarchive  K-means clustering',
         geo = list(showframe = TRUE, showcoastlines = TRUE,showcountries=TRUE),
         margin = list(l = 0, r = 0, b = 0, t = 30,projection = list(type = 'Mercator')),title_font_size=22,
            autosize = F)
embed_notebook(p)

dbFit<-fpc::dbscan(dfClusters, eps=1.5 ,MinPts=8)
df$Cluster4 <- dbFit$cluster

dfPlotmap%>% dplyr::select(3,7,8,9,10,11,12,20) %>%
  mutate(Cluster = df$Cluster4) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

p<-plot_ly(type = 'choropleth', text = dfPlotmap$Country_name, locations = dfPlotmap$CODE, colorscale="Jet",
        colorbar = list(title = "Cluster group"),
        locationmode = 'ISO-3', z = df$Cluster4, width = 1500,height=1000) %>% 
        layout(title = 'DBSCAN clustering',
         geo = list(showframe = TRUE, showcoastlines = TRUE,showcountries=TRUE),
         margin = list(l = 0, r = 0, b = 0, t = 30,projection = list(type = 'Mercator')),title_font_size=22,
            autosize = F)
embed_notebook(p)

k=8#number of clusters
cmeansFit <- cmeans(dfClusters, k, m=2, method="cmeans")
df$Cluster5 <- cmeansFit$cluster

dfPlotmap%>% dplyr::select(3,7,8,9,10,11,12,20) %>%
  mutate(Cluster = df$Cluster5) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

p<-plot_ly(type = 'choropleth', text = dfPlotmap$Country_name, locations = dfPlotmap$CODE, colorscale="Rainbow",
        colorbar = list(title = "Cluster group"),
        locationmode = 'ISO-3', z = df$Cluster5, width = 1500,height=1000) %>% 
        layout(title = 'C-means clustering',
         geo = list(showframe = TRUE, showcoastlines = TRUE,showcountries=TRUE),
         margin = list(l = 0, r = 0, b = 0, t = 30,projection = list(type = 'Mercator')),title_font_size=22,
            autosize = F)
embed_notebook(p)

GMMfit <- Mclust(dfClusters, 8)

dfPlotmap%>% dplyr::select(3,7,8,9,10,11,12,20) %>%
  mutate(Cluster = GMMfit$classification) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

p<-plot_ly(type = 'choropleth', text = dfPlotmap$Country_name, locations = dfPlotmap$CODE, colorscale="Jet",
        colorbar = list(title = "Cluster group"),
        locationmode = 'ISO-3', z = GMMfit$classification, width = 1500,height=1000) %>% 
        layout(title = 'Gaussian Mixture clustering',
         geo = list(showframe = TRUE, showcoastlines = TRUE,showcountries=TRUE),
         margin = list(l = 0, r = 0, b = 0, t =30,p=0,projection = list(type = 'Mercator')),title_font_size=22,
            autosize = F)
embed_notebook(p)
