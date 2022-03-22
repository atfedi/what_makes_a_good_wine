library(readr)
winequality_red <- read_delim("winequality-red.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)
View(winequality_red)


head(winequality_red)


dim(winequality_red)



model <- lm(quality ~ fixed_acidity + volatile_cidity + citric_acid + residual_sugar + chlorides + free_sulfur_dioxide + total_sulfur_dioxide + density + pH + sulphates + alcohol, data = winequality_red)
summary(model)


model1 <- lm(quality ~ sulphates, data = winequality_red)
summary(model1)   #(Check it)

#volatile acidity, chlorides, free sulfur dioxide, total sulfur dioxide, pH, sulphates, alcohol

plot(x = winequality_red$quality, y = winequality_red$volatile_cidity) 
plot(x = winequality_red$quality, y = winequality_red$chlorides)
plot(x = winequality_red$quality, y = winequality_red$free_sulfur_dioxide)
plot(x = winequality_red$quality, y = winequality_red$total_sulfur_dioxide)
plot(x = winequality_red$quality, y = winequality_red$pH)
plot(x = winequality_red$quality, y = winequality_red$sulphates) 
plot(x = winequality_red$quality, y = winequality_red$alcohol) 
  

new_data <- subset(winequality_red, select = c("volatile_cidity", "sulphates", "alcohol", "quality"))


install.packages("ggplot2")
library(ggplot2)

plot(x = winequality_red$quality, y = winequality_red$volatile_cidity) 
ggplot(winequality_red)+
geom_point(mapping = aes(quality, volatile_cidity), color = "blue") + 
  geom_smooth(aes(quality,volatile_cidity),method='lm', color="blue")

plot(x = winequality_red$quality, y = winequality_red$chlorides)
plot(x = winequality_red$quality, y = winequality_red$free_sulfur_dioxide)
plot(x = winequality_red$quality, y = winequality_red$total_sulfur_dioxide)
plot(x = winequality_red$quality, y = winequality_red$pH)

plot(x = winequality_red$quality, y = winequality_red$sulphates) 
ggplot(winequality_red)+
  geom_point(mapping = aes(quality, sulphates), color = "red") + 
  geom_smooth(aes(quality,sulphates),method='lm', color="blue")


plot(x = winequality_red$quality, y = winequality_red$alcohol) 
ggplot(winequality_red)+
  geom_point(mapping = aes(quality, alcohol), color = "yellow") + 
  geom_smooth(aes(quality,alcohol),method='lm', color="blue")


mine.heatmap <- ggplot(data = winequality_red, mapping = aes(x = alcohol, y = sulphates, fill = quality)) +
  geom_tile() +
  xlab(label = "Alcohol in each sample") +
  ylab(label = "Sulphates in the wine") +
  scale_fill_gradient(low="white", high="red")+
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0,0))

mine.heatmap

#Between 11 and 12 grades for alcohol and 0.4 and 0.6 for sulphates, there are the best wine quality

