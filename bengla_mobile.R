#Internet use and mobile phone ownership
#author: Masanori Matsuura
#2023/02/07

#import packages
install.packages("pacman")
pacman::p_load("tidyverse", "ggrepel")
#Import data

ownership <- read_csv("~/Research/mobile_money/mobile_data/API_IT.CEL.SETS.P2_DS2_en_csv_v2_4772619/API_IT.CEL.SETS.P2_DS2_en_csv_v2_4772619.csv", skip = 3)
internet <- read_csv("~/Research/mobile_money/mobile_data/API_IT.NET.USER.ZS_DS2_en_csv_v2_4770719/API_IT.NET.USER.ZS_DS2_en_csv_v2_4770719.csv", skip = 3)

#data cleaning
#  mobile subscription
ownership <- subset(ownership, ownership$`Country Name` == "Bangladesh")
ownership_b <-  subset(ownership, select = c("Country Name", seq(2010, 2020)))
ownership_b <- ownership_b[-1]
ownership_b <- gather(ownership_b, key=Year, value=Share, seq(1:11))
Indicator <- c("Mobile cellular subscriptions (per 100 people)")
ownership_b <-cbind(ownership_b, Indicator)
# internet user
internet <- subset(internet, internet$`Country Name` == "Bangladesh") 
internet_b <-  subset(internet, select = c("Country Name", seq(2010, 2020)))
internet_b <- internet_b[-1]
internet_b <- gather(internet_b, key=Year, value=Share, seq(1:11))
Indicator <- c("Individuals using the internet (% of population)")
internet_b <-cbind(internet_b, Indicator)


# Figures
bgd <- rbind(internet_b, ownership_b)
theme_set(theme_minimal())
png(file="~/Research/mobile_money/BIHS/figure/mobile_indicator.png",
    width = 869, height = 579)
bgd %>% 
  ggplot(aes(x=Year, y=Share, group=Indicator, linetype=Indicator)) +
  geom_line(linewidth = 1) +
  theme(legend.position="bottom", text = element_text(size=20))
dev.off()

