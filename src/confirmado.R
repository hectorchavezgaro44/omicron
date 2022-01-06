
library(tidyverse)
library(tidyquant)

casos <- read_csv("Downloads/Casos_Diarios_Estado_Nacional_Confirmados_20220104.csv") %>% 
      gather(fecha, total, `26-02-2020`: `03-01-2022`) %>% 
      mutate(fecha=lubridate::dmy(fecha)) %>% 
      filter(fecha>"2021-01-01" & fecha <= max(fecha)-4) %>%
      group_by(fecha) %>% 
      summarise(total=sum(total, na.rm=T))

casos %>% 
 ggplot(aes(x = fecha, y = total)) +
  geom_line(size = 1.1, alpha = .2, color = "#016392") +
  geom_point(size = 1.1, alpha = .3, color = "#016392") +
 geom_ma(size = 1.1, color = "#c72e29", n = 7,
           linetype = 1) +
  labs(title="Evolución de contagios \ny positividad COVID-19",
       subtitle = "En todo el país")+
  scale_x_date(date_breaks= "8 weeks", date_labels = "%d/%b") +
  ggthemes::theme_wsj()


# facet estados -----------------------------------------------------------

df <- read_csv("Downloads/Casos_Diarios_Estado_Nacional_Confirmados_20211227.csv") %>% 
  gather(fecha, total, `26-02-2020`: `27-12-2021`) %>% 
  mutate(fecha=lubridate::dmy(fecha)) %>% 
  filter(fecha>"2021-10-15" & fecha <= max(fecha)-3 &
           nombre!="Nacional" & nombre=="MEXICO") 


df %>% ggplot( aes(x = fecha, y = total)) +
  geom_line(size = 1.1, alpha = .2, color = "#016392") +
  geom_point(size = 1.1, alpha = .3, color = "#016392") +
  geom_ma(size = 1.1, color = "#c72e29", n = 7,
          linetype = 1) +
  labs(title="Evolución de contagios \ny positividad COVID-19",
       subtitle = "En CDMX")+
  scale_x_date(date_breaks= "8 weeks", date_labels = "%d/%b") +
  ggthemes::theme_wsj()



# Defunciones -------------------------------------------------------------




def <- read_csv("Downloads/Casos_Diarios_Estado_Nacional_Defunciones_20220103.csv") %>% 
  gather(fecha, total, `17-03-2020`: `03-01-2022`) %>% 
  mutate(fecha=lubridate::dmy(fecha)) %>% 
  filter(fecha>"2021-01-01" & fecha <= max(fecha)-4) %>%
  group_by(fecha) %>% 
  summarise(total=sum(total, na.rm=T))


df %>% ggplot( aes(x = fecha, y = total)) +
  # geom_line(size = 1.1, alpha = .2, color = "#016392") +
  # geom_point(size = 1.1, alpha = .3, color = "#016392") +
  geom_ma(size = 1.1, color = "#c72e29", n = 7,
          linetype = 1) +
  labs(title="Evolución de contagios \ny positividad COVID-19",
       subtitle = "En todo el país")+
  scale_x_date(date_breaks= "8 weeks", date_labels = "%d/%b") +
  ggthemes::theme_wsj()


# facte estados defunciones -----------------------------------------------

def <- read_csv("Downloads/Casos_Diarios_Estado_Nacional_Defunciones_20220103.csv") %>% 
  gather(fecha, total, `17-03-2020`: `03-01-2022`) %>% 
  mutate(fecha=lubridate::dmy(fecha)) %>% 
  filter(fecha>"2021-01-01" & fecha <= max(fecha)-3 &
           nombre!="Nacional" & nombre=="CHIHUAHUA") 

def %>% ggplot( aes(x = fecha, y = total)) +
  # geom_line(size = 1.1, alpha = .2, color = "#016392") +
  # geom_point(size = 1.1, alpha = .3, color = "#016392") +
  geom_ma(size = 1.1, color = "#c72e29", n = 7,
          linetype = 1) +
  facet_wrap(~nombre, scales="free")+
  labs(title="Evolución de contagios \ny positividad COVID-19",
       subtitle = "En todo el país")+
  scale_x_date(date_breaks= "8 weeks", date_labels = "%d/%b") +
  ggthemes::theme_wsj()
