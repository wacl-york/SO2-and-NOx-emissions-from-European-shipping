library(ggplot2)
library(scales)


dm1 <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/SFC_ACRUISE_1_preliminary.csv", stringsAsFactors = F)


dm2 <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-2_integration/ACRUISE-2_integration_preliminary.csv", stringsAsFactors = F)

dm_seca <- dm2 %>% filter(location != 1)
dm_free <- dm2 %>% filter(location == 1)



mean(dm1$SFC1)
median(dm1$SFC1)
sd(dm1$SFC1)

mean(dm2$SFC)
median(dm2$SFC)
sd(dm2$SFC)


ggplot()+
  geom_histogram(aes(dm1$SFC1),
                 bins=10,
                 colour="black",
                 fill="black",
                 alpha=0.3)+
  geom_histogram(aes(dm2$SFC),
                 bins=10,
                 colour="blue",
                 fill="blue",
                 alpha=0.3)


ggplot()+
  geom_histogram(aes(x=dm1$SFC1, 
                     y=(..count..)/sum(..count..),
                     colour="a",
                     fill="a"),
                 bins=10,
                 alpha=0.8)+
  geom_histogram(aes(x=dm_free$SFC, 
                     y=(..count..)/sum(..count..),
                     colour="b",
                     fill="b"),
                 bins=10,
                 alpha=0.4)+
  scale_colour_manual(name = "Colors", 
                     values = c("a" = "black", 
                                "b" = "blue"),
                     labels = c("a"="2019", 
                              "b"="2021"))+
  scale_fill_manual(name = "Colors", 
                     values = c("a" = "black", 
                                "b" = "blue"),
                     labels = c("a"="2019", 
                              "b"="2021"))+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(labels=percent)+
  theme_bw()+
  theme(text = element_text(size=14))+
  labs(y= "Percentage of plumes", 
       x="Apparent sulphur fuel content (%)")


ggplot()+
  geom_histogram(aes(x=dm_seca$SFC, 
                     y=(..count..)/sum(..count..)),
                 colour="black",
                 fill="black",
                 bins=10,
                 alpha=0.8)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(labels=percent)+
  theme_bw()+
  theme(text = element_text(size=14))+
  labs(y= "Percentage of plumes", 
       x="Apparent sulphur fuel content (%)")



