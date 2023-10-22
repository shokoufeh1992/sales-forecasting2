
library(vcd)
library(jalcal)
library("readxl")
library("writexl")
library("tidyverse")
library("forecast")
library("tseries")
library("greybox")
library("smooth")
library(fpp3)
library(lubridate)
library(zoo)
library(slider)
windowsFonts(BMitra="B Mitra")
windowsFonts(BZar="B Zar")
windowsFonts(BTitr="B Titr")


totalsale <-read_excel(file.choose())
(totalsale)
 
#calculate  each products' sales by date 
  totalsale <- totalsale %>% 
    select(-category) %>% 
    group_by(date,name) %>% 
    mutate(numb = sum(numb), rials= sum(rials)) %>% 
    distinct()
  
# convert to time series
  sale_tsib <- totalsale %>% 
    tsibble( index = date, key = c(name,numb,rials,month))
  
### Adding a 'weekday' Column
  sale_tsib <- sale_tsib %>% 
    mutate(weekday=weekdays(date))

### Analyzing Sales Data by Product, Month, and Weekday
  venus <- sale_tsib
  products <- tibble(levels(as.factor(venus$name)))
  venus_by_month_weekday <- venus %>% 
    group_by(name,month,weekday) %>% 
    summarise(mean_numb= mean(numb))

  ### Iterating through Product Data
  
product_names <- unique(venus_by_month_weekday$name)
for (product_name in product_names) {
  product_data <- venus_by_month_weekday %>%
    filter(name == product_name)
  
  # Loop through each month
  for (month_val in unique(product_data$month)) {
    product_month_data <- filter(product_data, month == month_val)
    
    # Create the plot
    
    plot <- ggplot(product_month_data, aes(x = weekday, y = mean_numb)) + 
      geom_bar(stat = "identity")+
      geom_text(aes(label = round(mean_numb, 2)), position = position_stack(vjust = 0.5), color = "black")+
      labs(x = "روز های هفته",
           y = "میانگین فروش",
           title = paste("میانگین فروش ماهانه به تفکیک روز", month_val))
    
    # Save the plot
    filename <- paste(product_name, month_val, ".png", sep = "_")
    ggsave(filename, plot)
  }
}

```

