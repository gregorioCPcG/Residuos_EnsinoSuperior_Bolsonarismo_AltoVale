# modelos para copiar em #
#galton$predicted <- predict(fit)   # Save the predicted values
#galton$residuals <- residuals(fit) # Save the residual values
library(ggplot2)
#ggplot(galton, aes(x = parent, y = child)) +
  #geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  #geom_segment(aes(xend = parent, yend = predicted), alpha = .2) +      # draw line from point to line
  #geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  #scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  #guides(color = FALSE, size = FALSE) +                             # Size legend removed
  #geom_point(aes(y = predicted), shape = 1) +
  #theme_bw()

#d1 <- fit$residuals
#d2 <- galton$child
#d <- data.frame(d1,d2)

#fit2 <- lm(d2 ~d1, d)
#d$predicted <- predict(fit2)   # Save the predicted values
#d$residuals <- residuals(fit2) # Save the residual values
#ggplot(d, aes(x = d1, y = d2)) +
 # geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
#  geom_segment(aes(xend = d1, yend = predicted), alpha = .2) +      # draw line from point to line
 # geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  #scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  #guides(color = FALSE, size = FALSE) +                             # Size legend removed
  #geom_point(aes(y = predicted), shape = 1) +
  #theme_bw()
#plot(fit, which=1, col=c("blue"))
#plot(fit2, which=1, col=c("blue"))
#plot(fit, which=2, col=c("red"))  # Q-Q Plot
#plot(fit2, which=2, col=c("red"))  # Q-Q Plot


## análise
library(readxl)
Milton27 <- read_excel("D:/ATUALIZA_PASTA_d/A Nova pasta/Milton27/Milton27.xlsx")

Milton27$Bozo <- 100*Milton27$Bolsonaro2018_turno2

fit <- lm(Hobus18~Hobus14, data=Milton27)
Milton27$predicted <- predict(fit)   # Save the predicted values
Milton27$residuals <- residuals(fit) # Save the residual values

summary(fit)
cor(Milton27$Hobus18, Milton27$predicted)
cor(Milton27$Hobus18, Milton27$residuals)


ggplot(Milton27, aes(x = Hobus14, y = Hobus18)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = Hobus18, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


ggplot(Milton27, aes(x = Hobus14, y = Hobus18)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + geom_text(label=Milton27$Cidade)+       
  geom_segment(aes(xend = Hobus18, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()



# Outro Modelo # usado no site

base <- Milton27

base$Bolsonaro <- Milton27$Bozo
cor.test(base$Bolsonaro, base$superior_comp)#deu
base$`% Superior Completo` <- base$superior_comp

plot(base$Bolsonaro, base$`% Superior Completo`)

fit <- lm(Bolsonaro ~ `% Superior Completo`, data=base)
summary(fit)


base$predicted <- predict(fit)   # Save the predicted values
base$residuals <- residuals(fit) # Save the residual values


cor(base$Bolsonaro, base$predicted)
cor(base$Bolsonaro, base$residuals)

plot(base$Bolsonaro, base$predicted)

ggplot(base, aes(x =`% Superior Completo`,  y = Bolsonaro)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = `% Superior Completo`, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


ggplot(base, aes(x = `% Superior Completo`, y =predicted)) + geom_point(colour = "red", size = 3) + geom_line()

ggplot(base, aes(x = `% Superior Completo`, y =Bolsonaro)) + geom_point(colour = "red", size = 3) + geom_text(label=Milton27$Cidade)
         

ggplot(base, aes(x = `% Superior Completo`, y =Bolsonaro)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + geom_text(label=Milton27$Cidade)+       
  geom_segment(aes(xend = `% Superior Completo`, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()



ggplot(base, aes(x = Bolsonaro, y =predicted)) + geom_point(colour = "red", size = 3) + geom_text(label=Milton27$Cidade)


library(tidyverse)
library(knitr)
library(kableExtra)

b5 <- base %>% 
  dplyr::select(Cidade, residuals) %>% 
  arrange(residuals)
b5 %>%
  kbl(caption = "Resíduos por Cidade") %>%
  kable_classic(full_width = F, html_font = "Garamond")


b5 <- base %>% 
  dplyr::select(Cidade, Bolsonaro, predicted, residuals) %>% 
  arrange(Cidade)
b5 %>%
  kbl(caption = "Resíduos por Cidade") %>%
  kable_classic(full_width = F, html_font = "Garamond")

library(coefplot)
coefplot(fit, intercept=FALSE, interactive=FALSE)
library(sjPlot)
tab_model(fit, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars") 
summary(base)

base <- subset(base, select=c(`% Superior Completo`,Bolsonaro, residuals, predicted))
library(BAS)


#??bas.lm
superior.bic =  bas.lm(Bolsonaro ~ `% Superior Completo` ,data=base, n.models=2^15, prior="BIC",modelprior=beta.binomial(1,1),initprobs= "eplogp") 
summary(superior.bic)
#plot(superior.bic)
#image(superior.bic, subset=-1)


g <- ggplot(base, aes(Bolsonaro))
g + geom_boxplot() + coord_flip()

g <- ggplot(base, aes(`% Superior Completo`))
g + geom_boxplot() + coord_flip()



g <- ggplot(base, aes( predicted))
g + geom_boxplot() + coord_flip()

summary(base)

