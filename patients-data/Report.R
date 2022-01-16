# Libraries
library(ggplot2)
library(dplyr)

# Alpha's Hospital Patients Data
hospital = read.csv('hospitalData.csv')


# UNIVARIATE QUANTITATIVE VARIABLE ()

ggplot(hospital, aes(x=age)) +
  geom_density(alpha=0.1, fill="blue") +


summary(hospital$age)

# UNIVARIATE QUALITATIVE VARIABLE (PIE CAHRT & BAR PLOT)
data = data.frame(table(hospital$gender))
names(data)[1] <- 'Gender'

#     PIE CHART

data <- data %>% 
  arrange(desc(Gender)) %>%
  mutate(prop = Freq / sum(data$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )



ggplot(data,aes(x="", y=Freq, fill=Gender)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y") +


  theme_void() +
  geom_text(aes(y = ypos, label = Gender), color = "white", size=6)



#     BAR PLOT
barplot(data$Freq,
        main = "Distribution of patients Genotype",
        xlab = "Genotype",
        ylab = "Frequency",
        names.arg = data$Genotype,
        col = "darkred",
        horiz = FALSE
)

# BIVARIATE QUANITATIVE VARIABLE
#     SCATTER PLOT
ggplot(data = hospital, aes(x = weight, y = bmi, color = gender))+
  geom_point() +
  geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95) +
  ggtitle("Relationship Between Weight and Bmi")


# 2 QUALITATIVE 1 QUANITATIVE MULTIVARIATE VARIABLE(BOX PLOT)
ggplot(data = hospital, aes(x=gender, y=age)) + 
  geom_boxplot(
    
    # custom boxes
    color="blue",
    fill= c("blue", "darkred"),
    alpha=0.1,
    
    # Notch?
    notch=FALSE,
    notchwidth = 0.5,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
) +
ggtitle("Age distribution of patients")


#  DENSITY PLOT

ggplot(data = hospital, aes(x=bmi)) +
geom_density(fill="#1185ba", color="#e9ecef", alpha=0.8) +
  geom_vline(aes(xintercept=mean(bmi)), color="blue", linetype="dashed", size=1) +
  ggtitle("Overall distribution of the Bmi") +
  theme(
    plot.title = element_text(size=15))



#  LOLIPOP CHART

group = aggregate(hospital,by=list(hospital$bloodgroup), FUN=length)
data = data.frame(
  x= group$Group.1,
  y= group$X
)

ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +

  xlab("Bloodgroup") +
  ylab("Frequency") +
  ggtitle("Population of patients by Bloodgroup")



print("Five number summary")
summary(hospital$bmi)

fivenum(hospital$bmi)
