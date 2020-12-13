oecd <- read_csv("finalproject/oecd_data.csv",
                 col_types = cols(LOCATION = col_character(),
                                  INDICATOR = col_character(),
                                  SUBJECT = col_character(),
                                  MEASURE = col_character(),
                                  FREQUENCY = col_character(),
                                  TIME = col_double(),
                                  Value = col_double(),
                                  'Flag Codes' = col_logical())) %>% 
  select(LOCATION, SUBJECT, TIME, Value) %>% 
  arrange()

oecd_country_continent <- read_csv("finalproject/oecd_country_continent.csv",
                                   col_types = cols(X1 = col_double(), 
                                                    LOCATION = col_character(),
                                                      SUBJECT = col_character(),
                                                     TIME = col_double(),
                                                    Value = col_double(),
                                                    Continent = col_character()))

V_Dem <- read_csv("finalproject/V-Dem-CY-Full+Others-v10.csv")

model <- lm(v2x_gencl ~ v2x_polyarchy,
         data = V_Dem)

summary(model)
                                  