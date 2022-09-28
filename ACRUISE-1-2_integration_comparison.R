library(ggplot2)
library(scales)


dm1 <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/SFC_ACRUISE_1_preliminary.csv", stringsAsFactors = F)


dm2 <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/SFC_ACRUISE_2_preliminary.csv", stringsAsFactors = F)



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
                 alpha=0.3)+
  geom_histogram(aes(x=dm2$SFC, 
                     y=(..count..)/sum(..count..),
                     colour="b",
                     fill="b"),
                 bins=10,
                 alpha=0.3)+
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


