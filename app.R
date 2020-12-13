# Libraries loaded

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)
library(shinythemes)
library(rstanarm)
library(gtsummary)
library(gt)
library(broom.mixed)
library(plotly)
library(ggthemes)

# Read in the csv I wrote in from my gather.Rmd

oecd_country_continent <- read_csv("oecd_country_continent.csv",
                                   col_types = cols(
                                       X1 = col_double(),
                                       LOCATION = col_character(),
                                       SUBJECT = col_character(),
                                       TIME = col_double(),
                                       Value = col_double(),
                                       Continent = col_character()
                                   ))

vdem1 <- read_csv("vdem1.csv",
                  col_types = cols(
                      country_name = col_character(),
                      LOCATION = col_character(),
                      year = col_double(),
                      e_p_polity = col_double(),
                      e_polity2 = col_double(),
                      e_democ = col_double(),
                      e_autoc = col_double()
                  ))

modeldata2 <- read_csv("modeldata2.csv",
                       col_types = cols(
                           X1 = col_double(),
                           LOCATION = col_character(),
                           TIME = col_double(),
                           Continent = col_character(),
                           country_name = col_character(),
                           year = col_double(),
                           e_p_polity = col_double(),
                           e_polity2 = col_double(),
                           e_democ = col_double(),
                           e_autoc = col_double(),
                           DISCRIMFAMCODE = col_double(),
                           RESTRCIVLIB = col_double(),
                           RESTRRESASSETS = col_double(),
                           TOT = col_double(),
                           RESTRPHYSINTEG = col_double()
                       ))

# Read in the RDS I created for my models from the gather.Rmd

model_discrimfamcode <- readRDS("model_discrimfamcode.RDS")

model_restrcivlib <- readRDS("model_restrcivlib.RDS")

model_tot <- readRDS("model_tot.RDS")

model_restrresassets <- readRDS("model_restrresassets.RDS")

model_restrphysinteg <- readRDS("model_restrphysinteg.RDS")

# In ui, called navbarPage, set theme to sandstone, titled final project. 

ui <- navbarPage(theme = shinytheme("sandstone"),
    "Polity and the Social Institutions of Gender Discrimination",
    
# First tab is the background page, introduces general trends in gender
# discrimination as well as in democracy. 
    
    tabPanel("Background", 
             h2("Background"),
             p("Over the past decade, and more specifically the past couple of 
               years, progress towards gender equality has slowed significantly.
               The  inaugural article of the March 2020 Proceedings of the 
               National Academy of Sciences of the United States of America 
               stated that “progress towards gender equality in the United 
               States has slowed or stalled”, citing the slowing down of gender 
               equality following the “gender revolution” of the mid to late 
               20th century.The United Nations 2020 World’s Women Trends and 
               Statistics would confirm that internationally, progress towards 
               equal rights and the abolition of gender discrimination “remains 
               elusive”, and definitively states that no single country has 
               reached gender equality, warning of the potential of the 
               Covid-19 pandemic to undo progress that has been made in the 
               past decades."),
             p("Recently, there have been significant decreases in democratic 
               traits and values globally. With this comes the rise 
               of autocratization. As the Varieties of Democracy 2020 report 
               states, there are more people living in autocracies than not in 
               the world for the first time since 2001.  The 2020 Democracy 
               Report reminds that the EU has its first non-democratic member 
               with Hungary, and that “major G20 nations and all regions of the
               world are part of the ‘third wave of autocratization’”, 
               economically and politically significant nations under this 
               category include Brazil, India, the United States of America, 
               and Turkey."),
             p("You can find more information from the links to the
               sources referenced here:"),
             a("The World’s Women 2020: Trends and Statistics, ", 
               href = 
                   "https://www.un.org/en/desa/world%E2%80%99s-women-2020#:~:
               text=The%20World's%20Women%202020%3A%20Trends%20and%20Statistics
               %20compiles%20100%20data,state%20of%20gender%20equality%20
               worldwide.&text=In%202020%2C%20only%2047%25%20of,remained%20
               relatively%20constant%20since%201995."),
            a("PNAS: Progress Toward Gender Equality..., ", 
              href = "https://www.pnas.org/content/117/13/6990"),
            a("V-Dem: Varieties of Democracy, ", 
              href = "https://www.v-dem.net/en/"),

             
# Brief overview of dataset and important terminology/variables defined. 
             
             h2("Project Overview"),
             p("The OECD’s Social Institutions and Gender Index (SIGI) measures 
               discrimination against women in social institutions. These social
               institutions that discriminate on a gender basis work all 
               throughout the lives of girls and women to exacerbate inequality 
               and restrict rights, opportunities, and freedom. 
               The Social Institutions and Gender Index covers four dimensions 
               of discriminatory social institutions, all of which I use in my
               project."),
             p("Varieties of Democracy (V-Dem) measures democracy through a 
               number of datasets and dimensions that span the nuances of the 
               conceptualization of democracy. The aspect of the V-dem dataset 
               I focused on was polity, and I worked with four variables 
               related to polity for my project. They are provided below as
               defined by the V-Dem codebook"),
             h4("Dimensions of Discriminatory Social Institutions"),
               p("Discriminatory family code, 
               Restricted physical integrity, Restricted resources and assets,
               Restricted civil liberty"),
             h4("V-Dem variables"),
             p("Institutionalized autocracy (e_autoc): 'Autocracy is defined 
             operationally in terms of the presence of a distinctive set of 
             political characteristics. In mature form, autocracies sharply 
             restrict or suppress competitive political participation. Their 
             chief executives are chosen in a regularized process of selection 
             within the political elite, and once in office they exercise power 
             with few institutional constraints.'"),
             p("Institutionalized democracy (e_democ): 'Democracy is conceived 
               as three essential, interdependent elements. One is the presence 
               of institutions and procedures through which citizens can express 
               effective preferences about alternative policies and leaders. 
               Second is the existence of institutionalized constraints on the 
               exercise of power by the executive. Third is the guarantee of 
               civil liberties to all citizens in their daily lives and in acts 
               of political participation.'"),
             p("Polity combined score (e_p_polity): 'The Polity score is 
               computed by subtracting the autocracy score from the democracy 
               score. The resulting unified POLITY scale ranges from +10 
               (strongly democratic) to -10 (strongly autocratic).'"),
             p("Polity revised combined score (e_polity2): 'This variable is a 
             modified version of the Polity variable added in order to 
             facilitate the use of the Polity regime measure in time-series 
             analyses. It modifies the combined annual Polity score by applying 
             a simple treatment, or 'fix', to convert instances of 
             'standardized authority scores' (i.e., -66, -77, and -88) to 
             conventional polity scores (i.e., within the range, 
               -10 to +10).'"),

# Heading for the trends in institutionalized democracy interactive plot.

             h3("Trends in Institutionalized Democracy, 1900- present"),
             h5("Choose a country below to explore its trends in 
             institutionalized democracy."),

# fluidPage and using selectInput to make it interactive. 
# Specifying the plot2. 

             fluidPage(
                 selectInput("violencetyped", "Country",
                             choice = distinct(vdem1, country_name)),
                 plotOutput("plot2")),

# Heading for the trends in institutionalized autocracy interactive plot.

             h3("Trends in Institutionalized Autocracy, 1900- present"),
             h5("Choose a country below to explore its trends in 
             institutionalized autocracy."),

# fluidPage and using selectInput to make it interactive. 
# Specifying the plot3. 

             fluidPage(
                 selectInput("violencetypea", "Country",
                             choice = distinct(vdem1, country_name)),
                 plotOutput("plot3")),
             ),
    
# A Closer Look tab. 

    tabPanel("A Closer Look",
             
# Heading for the trends in institutionalized autocracy interactive plot.
             
             h3("Trends in Combined Polity Scores, 1900- present"),
             h5("Choose a country below to explore its trends in 
             polity."),

# fluidPage and using selectInput to make it interactive. 
# Specifying the plot4. 

             fluidPage(
                 selectInput("violencetype2", "Country", 
                             choice = distinct(vdem1, country_name)),
                 plotOutput("plot4")),

# Heading for the trends in institutionalized autocracy interactive plot.

             h3("Trends in Revised Polity Scores, 1900- present"),
             h5("Choose a country below to explore its trends in 
             polity."),

# fluidPage and using selectInput to make it interactive. 
# Specifying the plot5. 

             fluidPage(
                 selectInput("violencetypep", "Country", 
                             choice = distinct(vdem1, country_name)),
                 plotOutput("plot5")),

# Heading for the values of gender discrimination by type interactive plot.

             h3("Values of Gender Discrimination through Dimensions of 
                Discriminatory Social Institution"),
             h5("Choose a type of gender discrimination below to explore social 
                institutions of gender discrimination across continents."),

# fluidPage and using selectInput to make it interactive. 
# Specifying the plot6.

             fluidPage(
                 selectInput("violencetype", "Type of Discriminatory Social 
                             Institution", 
                             choice = 
                                 distinct(oecd_country_continent, SUBJECT)),
                 plotOutput("plot6"))),


# tabPanel for Model page. 
    
    tabPanel("Model",
             titlePanel("Standard Generalized Linear Model Output"),
             
# Header for the equations.
             
             h3("Equations"),
             
# Calling all five equations through fluidPage and uiOutput.
             
             fluidPage(uiOutput("equation1")),
             fluidPage(uiOutput("equation2")),
             fluidPage(uiOutput("equation3")),
             fluidPage(uiOutput("equation4")),
             fluidPage(uiOutput("equation5")),

# Explaining the variables and coefficients of the equations.

             p("dimension of discriminatory social institution = the outcome 
               variable in relation to the e_polity2 and year variables"),
             p("e_polity2 = the variable for the revised polity combined score 
               that demonstrates the level of polity of a nation at a specific 
               year"),
             p("year = the variable for the year of said score for a country"),
             p("beta1 = the coefficient of the variable that represents the 
               revised polity combined score, 
               which can be between -10 and +10"),
             p("beta2 = the coefficient of the variable that represents the 
               year"),
             p("epsilon =  the error, representing the unknowns of the model"),

# # Header for the regression tables.

             h3("Regression Tables"),

# Calling all five regression tables through fluidPage and uiOutput.

             fluidPage(tableOutput("table2")),
             fluidPage(tableOutput("table3")),
             fluidPage(tableOutput("table4")),
             fluidPage(tableOutput("table5")),
             fluidPage(tableOutput("table1")),

# Interpretation of regressions.

             h3("Interpretation"),
             p("The purpose of these models were to see the relationship 
               between the dimension of a discriminatory social institution and 
               the polity (revised) and year of a country. We know that for the 
               polity revised combined score variable (e_polity2), there is a 
               scale of -10 (strongly autocratic) to +10 (strongly democratic). 
               By running the regression, we are trying to quantify the 
               difference in whichever dimension of a discriminatory social 
               institution that we choose in relation to polity. Across the 
               different dimensions –discriminatory family code, restricted 
               civil liberty, restricted resources and assets, restricted 
               physical integrity, and total– it can be seen that the e_polity2 
               coefficients are all negative, ranging from -0.57 to -1.1. This 
               means that as polity scores increase, the values for 
               discrimination decrease across the tables. Interestingly enough, 
               the year coefficients are all positive, ranging from 0.07 to 
               0.13, meaning that values of gender discrimination from these 
               social institutions increase slightly as time goes on. We know 
               that these values are statistically significant because the 95% 
               confidence interval values do not cross 0. ")),


# About tabPanel for the About page. 
    
    tabPanel("About", 
             
# Project Background and Motivations
             
             h2("Project Background and Motivations"),
             p("This is my final project for Gov50: Data. My research 
             concentration for a global studies course my senior year of high 
             school had to do with violence against women and more specifically, 
             femicide. Initially, I had been pretty set on having my final 
             project be specifically on international trends of violence against 
             women. However, I stumbled across the OECD’s Social Institutions 
             and Gender Index  and was really interested in the idea of 
             different dimensions of gender discrimination through social 
             institutions, some of which included violence against women, or 
             more specifically, the physical integrity or civil liberties of 
             women. From there, I also wanted to find trends or reasons that 
             might be able to describe what I was seeing in the SIGI data. I 
             used data from Varieties of Democracy and settled on the polity 
             variable specifically to try to test out the relationship between 
             polity and dimensions of discriminatory social institutions."),

# Data explanation/descriptions.
             
             h2("Data"),
             h3("Dataset 1: OECD Data: Social Institutions and Gender, Violence 
                Against Women"),
             p("The Social Institutions and Gender Index (SIGI) internationally 
               measures discrimination against women in social institutions. 
               SIGI covers four aspects of discriminatory social institutions: 
               discriminatory family code, restricted physical integrity, 
               restricted resources and assets, and restricted civil liberty. 
               I used this dataset for a plot on my 'A Closer Look' page and
               used a joint dataset with data from this dataset and dataset 2
               (V-Dem) for my models.", 
               a("OECD data", 
                 href = 
                "https://data.oecd.org/inequality/social-institutions-and-gender
                .htm#indicator-chart")),
             h3("Dataset 2: V-Dem: Varieties of Democracy"),
             p("I used V-Dem: Global Standards, Local Knowledge for my 
               exploration of democracy, autocracy, and polity. Varieties of 
               Democracy (V-Dem) works to analyze and measure democracy around 
               the world. Varieties of Democracy focuses on five principles of 
               democracy: electoral, liberal, participatory, deliberative, and 
               egalitarian. I decided to focus on polity. You can find more 
               information on polity in the Introduction page. I used this data 
               for my graphs on the 'Introduction' and 'A Closer Look' pages and
               used a joint dataset with data from this dataset and dataset 1
               (OECD) for my models.",
               a("Vdem data", 
                 href = "https://github.com/vdeminstitute/vdemdata")),

# About Me description and contact information.
             
             h2("About Me"),
             p("My name is Sara Park and I am a sophomore at Harvard University. 
             I am a Government concentrator with a secondary in Economics and 
             a citation in Chinese. I am also on the Harvard Varsity Women’s 
             Basketball team."),
             
             h2("Contact"),
             p("Please don't hesitate to contact me with any questions. 
               My email is spark@college.harvard.edu"),
             p("My githube repo for this project can be found", a("here.",
                 href = "https://github.com/ssarapark/polity_
                 genderdiscrimination_project.git"),
               "Feel free to take a look!")))
           

# Define server logic.
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(alpha = 0.5),
               column = geom_col,
               jitter = geom_jitter()
        )
    })
    
# Code for making plot6 interactive. 
# Specified oecd_country_continent, clarified the aesthetics to be inputs
# using .data[[input$x]] and .data[[input$y]].
    
    output$plot <- renderPlot({
        ggplot(oecd_country_continent, aes(.data[[input$x]], .data[[input$y]], 
                         label = LOCATION)) +
            plot_geom() 
    }, res = 96)
    
# Plot6 for the dimension of discriminatory social institution by 
# continent using oecd_country_continent and output$plot and renderPlot. 
# Facet wrapped by Continent to make clearer distinctions. 
# Adjusted axis text, title, and ticks through theme. 
    
    output$plot6 <- renderPlot({
        oecd_country_continent %>% 
            filter(SUBJECT == input$violencetype) %>% 
        ggplot(aes(x = LOCATION, y = Value)) +
            geom_text(aes(label = LOCATION)) +
            labs(x = "Type of Violence") +
            facet_wrap(~Continent) +
            theme_minimal() +
            theme(axis.text = element_text(size = 12)) +
            theme(axis.title = element_text(size = 14, face = "bold")) + 
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
    }, res = 96)
    
    
# Plots for the trends in institutionalized democracy, institutionalized 
# autocracy, polity combined score, and revised polity combined scores.
# Plots 2, 3, 4, 5 created from vdem and used in an interactive plot.
# They are all essentially the same except with a different variable at the
# y aesthetic for each and different titles. 
# Used output$plot and renderPlot.
    
    output$plot4 <- renderPlot({
        vdem1 %>% 
            filter(country_name == input$violencetype2) %>% 
            select(country_name, LOCATION, year,  
                   e_p_polity, e_polity2, e_democ) %>% 
            ggplot(aes(x = year, y = e_p_polity)) +
            geom_line() +
            theme_light() +
            theme(axis.text = element_text(size = 12)) +
            theme(axis.title = element_text(size = 14, face = "bold")) + 
            labs(y = "Value",
                 x = "Year")
    })
    
    output$plot3 <- renderPlot({
        vdem1 %>% 
            filter(country_name == input$violencetypea) %>% 
            select(country_name, LOCATION, year,  
                   e_p_polity, e_polity2, e_democ, e_autoc) %>% 
            ggplot(aes(x = year, y = e_autoc)) +
            geom_line() +
            theme(axis.text = element_text(size = 12)) +
            theme(axis.title = element_text(size = 14, face = "bold")) + 
            theme_light() +
            labs(y = "Value",
                 x = "Year")
    })
    
    output$plot2 <- renderPlot({
        vdem1 %>% 
            filter(country_name == input$violencetyped) %>% 
            select(country_name, LOCATION, year,  
                   e_p_polity, e_polity2, e_democ, e_autoc) %>% 
            ggplot(aes(x = year, y = e_democ)) +
            geom_line() +
            theme(axis.text = element_text(size = 12)) +
            theme(axis.title = element_text(size = 14, face = "bold")) + 
            theme_light() +
            labs(y = "Value",
                 x = "Year")
    })
    
    output$plot5 <- renderPlot({
        vdem1 %>% 
            filter(country_name == input$violencetypep) %>% 
            select(country_name, LOCATION,  year,  
                   e_p_polity, e_polity2, e_democ, e_autoc) %>% 
            ggplot(aes(x = year, y = e_polity2)) +
            geom_line() +
            theme(axis.text = element_text(size = 12)) +
            theme(axis.title = element_text(size = 14, face = "bold")) + 
            theme_light() +
            labs(y = "Value",
                 x = "Year")
    })
    
# Code for regression tables for shiny.
# Created tables 1 through 5 here with output$table, render_gt.
# Had trouble here because I forgot to load the gt library. 
# Also had trouble with tab_header because it was not in the right
# parenthesis. 
    
output$table1 <- render_gt(tbl_regression(model_tot, intercept = TRUE) %>% 
        as_gt() %>% 
        tab_header(title = "Total",
                   subtitle = "Emphasis on coefficient of e_polity2, or
                   Revised Polity Score")) 

output$table2 <- 
    render_gt(tbl_regression(model_discrimfamcode, intercept = TRUE) %>% 
    as_gt() %>% 
    tab_header(title = "Discriminatory Family Code on Polity (Revised)",
               subtitle = "Emphasis on coefficient of e_polity2, or
                   Revised Polity Score"))

output$table3 <- 
    render_gt(tbl_regression(model_restrcivlib, intercept = TRUE) %>% 
    as_gt() %>% 
    tab_header(title = "Restricted Civil Liberty on Polity 
               (Revised)",
               subtitle = "Emphasis on coefficient of e_polity2, or
                   Revised Polity Score"))

output$table4 <- 
    render_gt(tbl_regression(model_restrresassets, intercept = TRUE) %>% 
    as_gt() %>% 
        tab_header(title = "Restricted Resources and 
                   Assets on Polity (Revised)",
                   subtitle = "Emphasis on coefficient of e_polity2, or
                   Revised Polity Score"))

output$table5 <- 
    render_gt(tbl_regression(model_restrphysinteg, intercept = TRUE) %>% 
    as_gt() %>% 
    tab_header(title = "Restricted Physical Integrity on 
               Polity (Revised)",
               subtitle = "Emphasis on coefficient of e_polity2, or
                   Revised Polity Score"))

# Creating mathemotical formula equations with MathJax.
# Needed help with MathJax but got it and figured it help.
# Created equations 1 through 5 here with output$equation, renderUI, 
# withMathJax and helpText.

output$equation1 <- renderUI({
    withMathJax(helpText('$$ totaldiscrimination_i = \\beta_0 + \\beta_1 
                         polity_{i} + \\beta_2 year_{i} + \\epsilon_i$$'))
})
    
output$equation2 <- renderUI({
    withMathJax(helpText('$$ discriminatoryfamilycode_i = \\beta_0 + \\beta_1 
                         polity_{i} + \\beta_2 year_{i} + \\epsilon_i$$'))
})

output$equation3 <- renderUI({
    withMathJax(helpText('$$ restrictedcivilliberty_i = \\beta_0 + \\beta_1 
                         polity_{i} + \\beta_2 year_{i} + \\epsilon_i$$'))
})

output$equation4 <- renderUI({
withMathJax(helpText('$$ restrictedresourcesandassets_i = \\beta_0 + \\beta_1 
                         polity_{i} + \\beta_2 year_{i} + \\epsilon_i$$'))
})

output$equation5 <- renderUI({
    withMathJax(helpText('$$ restrictedphysicalintegrity_i = \\beta_0 + \\beta_1 
                         polity_{i} + \\beta_2 year_{i} + \\epsilon_i$$'))
})

}

# Run the application 
shinyApp(ui = ui, server = server)
