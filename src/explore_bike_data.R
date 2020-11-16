# Explore bike data to see if there is a relationship between weather and ridership

library(tidyverse)

df = read_csv("data/daily_bike_data.csv")
df$dteday <- as.Date(df$dteday, format = "%m/%d/%Y")

# Create a base layer for a graphic and keep adding to it
ggplot(data = df) +
  geom_line(aes(x = dteday, y = cnt))

ggplot(data = df, aes(x = temp, y = cnt)) +
  geom_point() +
  geom_smooth()

# Dplyr verbs (some of them):
# mutate: adds new columns to your df or adds new variables to your dataset. Add the variable in addition to all the variables
# transmute: will only keep the new columns generated
# select will pick the columns from the dataset
# filter will filter the rows

df2 = df %>% dplyr::mutate(
            weather_fac = factor(weathersit,
                                 levels = c(1,2,3,4),
                                 labels = c("Clear", "Cloudy","Rainy","Heavy Rain")))
            
dftest = df2 %>% dplyr::select(dteday, weathersit, weather_fac)


df2 %>% dplyr::filter(weather_fac == "Rainy") %>%
  ggplot(aes(x = temp, y = cnt))+
  geom_point() +
  geom_smooth()

# dply select can also drop variables
df3 = df2 %>% dplyr::select(-weathersit)

#can also use chaacter list
keep_vars = c("dteday","weather_fac","temp","cnt")

df4 = df2 %>% select(all_of(keep_vars))

#other ways of filtering
weather_factors_we_like = c("Rainy","Cloudy")
df2 %>% dplyr::filter(weather_fac == "Rainy"| weather_fac == "Cloudy")
df2 %>% dplyr::filter(weather_fac %in% weather_factors_we_like)
df2 %>% dplyr::filter(weather_fac != "Rainy")
df2 %>% dplyr::filter(!(weather_fac %in% weather_factors_we_like))

#sumarize
df2 %>% dplyr::group_by(season, weather_fac) %>%
  dplyr::summarize(
    cnt_mean = mean(cnt)
  )

#Transforming data from long to wide (it was many columns) or vice-versa
# df_wide = df2 %>% dplyr::select(mnth,temp) %>%
#   dplyr::group_by(season,mnth) %>%
#   dplyr::summarize(temp_mean = mean(temp)) %>%
#   tidyr::pivot_wider(names_prefix = "temp_")


## Pivoting longer. From lots of columns to lots of rows
df_long = df2 %>% tidyr::pivot_longer(cols = c(temp,atemp,hum,windspeed), values_to = 'value', names_to = 'variable')

## Now we will use pivot wider to make it goback to how it was before
df_wider = df_long %>% 
  tidyr::pivot_wider(names_prefix = "v_", names_from = variable, values_from = value)


#facetting
ggplot(data = df2, aes(x=temp, y = cnt)) +
  geom_point(shape = 21, color = "orangered") +
  geom_smooth(method = "lm", formula = y ~ poly(x,2)) +
  facet_wrap(~weather_fac, scales = "free_y")+
  labs(x = "temperature", y = "Ridership")+
  theme_linedraw()+
  theme(strip.background = element_rect(fill =NA), strip.text = element_text(color = "black"))+
  theme(panel.grid  = element_line(colour = "gray"))


#Plotting with a longer data set

ggplot(data = df_long, aes(x = value, y = cnt, color = variable))+
  geom_point()+
  facet_wrap(~weather_fac)

#if you are working with large data folders you might have to add to git ignore your data forlder so it is not uploaded