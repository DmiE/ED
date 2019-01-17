#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
install.packages("rworldmap")

library(ggplot2)
library(dplyr)
library(lubridate)
library(shiny)
library(rworldmap)

raw <- read.csv("athlete_events.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("120 years of Olimpic history",
  tabPanel("avarage weight",
  
    fluidPage(
   
      # Application title
      titlePanel("Avarage athlets weight"),
   
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          selectInput("Tab1_Sex", "Choose sex:",choices = c("M", "F"))
        ),
      
          # Show a plot of the generated distribution
          mainPanel(
          plotOutput("Tab1_WeightPlot")
        )
      )
    )
  ),
  tabPanel("avarage BMI",
           
           fluidPage(
             
             # Application title
             titlePanel("Avarage athlets BMI"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("Tab2_Sex", "Choose sex:",choices = c("M", "F"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("Tab2_BMIPlot")
               )
             )
           )
  ),
  tabPanel("BMI to Medals",
           
           fluidPage(
             
             # Application title
             titlePanel("Avarage athlets BMI"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("Tab3_Sport", "Choose sport", choices = c("Alpine Skiing", "Archery", "Art Competitions", "Athletics", "Badminton", "Baseball", "Basketball", "Beach Volleyball", "Biathlon", "Bobsleigh", "Boxing", "Canoeing", "Cross Country Skiing",
                                                                        "Curling", "Cycling", "Diving", "Equestrianism", "Fencing", "Figure Skating", "Football", "Freestyle Skiing", "Golf", "Gymnastics", "Handball", "Hockey", "Ice Hockey", "Judo", "Lacrosse", "Luge",
                                                                        "Modern Pentathlon", "Motorboating", "Nordic Combined", "Rhythmic Gymnastics", "Rowing", "Rugby", "Rugby Sevens", "Sailing", "Shooting", "Short Track Speed Skating", "Skeleton", "Ski Jumping", "Snowboarding", "Softball", "Speed Skating", "Swimming",
                                                                        "Synchronized Swimming", "Table Tennis", "Taekwondo", "Tennis", "Trampolining", "Triathlon", "Tug-Of-War", "Volleyball", "Water Polo", "Weightlifting", "Wrestling")),
                 selectInput("Tab3_Sex", "Choose sex:",choices = c("M", "F"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("Tab3_BMIMedalsPlot")
               )
             )
           )
  ),
           tabPanel("Countries",
                    
                    fluidPage(
                      
                      # Application title
                      titlePanel("Medals per Country"),
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("Tab4_Sport", "Choose sport", choices = c("Alpine Skiing", "Archery", "Art Competitions", "Athletics", "Badminton", "Baseball", "Basketball", "Beach Volleyball", "Biathlon", "Bobsleigh", "Boxing", "Canoeing", "Cross Country Skiing",
                                                                                "Curling", "Cycling", "Diving", "Equestrianism", "Fencing", "Figure Skating", "Football", "Freestyle Skiing", "Golf", "Gymnastics", "Handball", "Hockey", "Ice Hockey", "Judo", "Lacrosse", "Luge",
                                                                                "Modern Pentathlon", "Motorboating", "Nordic Combined", "Rhythmic Gymnastics", "Rowing", "Rugby", "Rugby Sevens", "Sailing", "Shooting", "Short Track Speed Skating", "Skeleton", "Ski Jumping", "Snowboarding", "Softball", "Speed Skating", "Swimming",
                                                                                "Synchronized Swimming", "Table Tennis", "Taekwondo", "Tennis", "Trampolining", "Triathlon", "Tug-Of-War", "Volleyball", "Water Polo", "Weightlifting", "Wrestling")),
                          selectInput("Tab4_Sex", "Choose sex:",choices = c("M", "F")),
                          selectInput("Tab4_Medal", "Choose medal", choices = c("Gold", "Silver", "Bronze"))
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("Tab4_MedalsPerCountry")
                        )
                      )
                    )
                ),
          tabPanel("Over The Years",
           
           fluidPage(
             
             # Application title
             titlePanel("Over The Years"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("Tab5_Sport", "Choose sport", choices = c("Alpine Skiing", "Archery", "Art Competitions", "Athletics", "Badminton", "Baseball", "Basketball", "Beach Volleyball", "Biathlon", "Bobsleigh", "Boxing", "Canoeing", "Cross Country Skiing",
                                                                       "Curling", "Cycling", "Diving", "Equestrianism", "Fencing", "Figure Skating", "Football", "Freestyle Skiing", "Golf", "Gymnastics", "Handball", "Hockey", "Ice Hockey", "Judo", "Lacrosse", "Luge",
                                                                       "Modern Pentathlon", "Motorboating", "Nordic Combined", "Rhythmic Gymnastics", "Rowing", "Rugby", "Rugby Sevens", "Sailing", "Shooting", "Short Track Speed Skating", "Skeleton", "Ski Jumping", "Snowboarding", "Softball", "Speed Skating", "Swimming",
                                                                       "Synchronized Swimming", "Table Tennis", "Taekwondo", "Tennis", "Trampolining", "Triathlon", "Tug-Of-War", "Volleyball", "Water Polo", "Weightlifting", "Wrestling")),
                 selectInput("Tab5_Sex", "Choose sex:",choices = c("M", "F")),
                 selectInput("Tab5_Characteristic", "Choose sex:",choices = c("Age", "Height", "Weight"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("Tab5_OverTheYears")
               )
             )
           )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$Tab1_WeightPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      weight_filtered_raw <- raw %>% filter((Weight >= 35 & Age >= 18) | (Weight <= 160 & Age < 18)) #wywalamy osoby powyzej 18 wazace ponizej 35 i ponizej 18 wazace ponad 180kg 
      waga_do_dyscypliny_raw <- data.frame(weight_filtered_raw$Sport, weight_filtered_raw$Weight, sex = weight_filtered_raw$Sex) # utworzenie zmiennej - Dyscyplina - Waga - PLec
      waga_do_dyscypliny_bez_na <- waga_do_dyscypliny_raw %>% filter(!is.na(weight_filtered_raw.Weight)) # utworzenie zmiennej - Dyscyplina - Waga - Plec bez NA
      
      waga_do_dyscypliny_bez_na %>% group_by(Sport = weight_filtered_raw.Sport, sex)  %>% summarise(weight = mean(weight_filtered_raw.Weight)) -> waga_do_dyscypliny # Srednia waga do dyscypliny (osobno K i M)
      #sorted <- waga_do_dyscypliny[order(waga_do_dyscypliny$weight), ] # waga_do_dyscypliny posortowana po wadze
      
      waga_do_dyscypliny_Sex <- waga_do_dyscypliny[(waga_do_dyscypliny$sex == input$Tab1_Sex),]
      waga_do_dyscypliny_Sex_sorted <- waga_do_dyscypliny_Sex[order(waga_do_dyscypliny_Sex$weight), ]
      ggplot(data = waga_do_dyscypliny_Sex_sorted, aes(x = reorder(Sport,-weight), y = weight)) + geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
   })
    output$Tab2_BMIPlot <- renderPlot({
     
      BMI_Dyscyplina <- data.frame(Sport = raw$Sport, BMI = (raw$Weight/(raw$Height/100)^2), Sex = raw$Sex) # Dyscyplina - BMI - Plec
      BMI_Dyscyplina_bez_NA <- BMI_Dyscyplina %>% filter(!is.na(BMI)) # Dyscyplina - BMI - Plec -- Bez NA
      BMI_Dyscyplina_filtered <- BMI_Dyscyplina_bez_NA %>% filter(BMI>=12 & BMI<= 42) # Wartosci BMI ponizej 12(mega skrajna niedowaga) i powyzej 42(mega skrajna otylosc) usuniete
   
      Srednie_BMI_Dyscyplina <- BMI_Dyscyplina_filtered %>% group_by(Sport, Sex)  %>% summarise(BMI_MEAN = mean(BMI))
      
      BMI_Dyscyplina_Sex <- Srednie_BMI_Dyscyplina[(Srednie_BMI_Dyscyplina$Sex == input$Tab2_Sex),]
      BMI_Dyscyplina_Sex_sorted <- BMI_Dyscyplina_Sex[order(BMI_Dyscyplina_Sex$BMI_MEAN),]
      
      ggplot(data = BMI_Dyscyplina_Sex_sorted, aes(x = Sport, y = BMI_MEAN)) + geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
   })
    output$Tab3_BMIMedalsPlot <- renderPlot({
      # BMI DO ZDOBYTYCH MEDALI W DANEJ DYSCYPLINIE
    
      BMI_Dyscyplina_Medal_Plec <- data.frame(Sport = raw$Sport, BMI = round((raw$Weight/(raw$Height/100)^2), digits = 0), Sex = raw$Sex, Medal = raw$Medal) # Dyscyplina - BMI - Plec - Medal
      BMI_Dyscyplina_Medal_Plec_bez_NA <- BMI_Dyscyplina_Medal_Plec %>% filter((!is.na(BMI)) & (!is.na(Medal))) # Dyscyplina - BMI - Plec -- Bez NA
    
      BMI_Dla_Sportu_i_Plci <- BMI_Dyscyplina_Medal_Plec_bez_NA[(BMI_Dyscyplina_Medal_Plec_bez_NA$Sex == input$Tab3_Sex) & (BMI_Dyscyplina_Medal_Plec_bez_NA$Sport == input$Tab3_Sport),]
      Medale_dla_BMI <- BMI_Dla_Sportu_i_Plci %>% count(BMI, Medal, sort = TRUE) #zliczenie medali danego typu dla poszczegolnych BMI
      
      ggplot(data = Medale_dla_BMI, aes(x = BMI, y = n, fill = Medal)) + geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
   })
    
    output$Tab4_MedalsPerCountry <- renderPlot({
      Dyscyplina_Medal_Kraj_Plec <- data.frame(Sport = raw$Sport, Sex = raw$Sex, Country = raw$Team, Medal = raw$Medal) # Dyscyplina - Kraj - Plec - Medal
      Kraj_Dyscyplina_Medal_Plec_bez_NA <- Dyscyplina_Medal_Kraj_Plec %>% filter((!is.na(Country)) & (!is.na(Medal)) & (!is.na(Sport))) # Dyscyplina - BMI - Plec -- Bez NA
      
      #Medale_Dla_Sport_Plec <- Kraj_Dyscyplina_Medal_Plec_bez_NA[(Kraj_Dyscyplina_Medal_Plec_bez_NA$Sex == input$Tab4_Sex) & (Kraj_Dyscyplina_Medal_Plec_bez_NA$Sport == input$Tab4_Sport),]'
      Medale_Dla_Sport_Plec <- Kraj_Dyscyplina_Medal_Plec_bez_NA[(Kraj_Dyscyplina_Medal_Plec_bez_NA$Sex == input$Tab4_Sex) & (Kraj_Dyscyplina_Medal_Plec_bez_NA$Sport == input$Tab4_Sport) & (Kraj_Dyscyplina_Medal_Plec_bez_NA$Medal == input$Tab4_Medal),]
      #Medale_Dla_Sport_Plec <- Kraj_Dyscyplina_Medal_Plec_bez_NA[(Kraj_Dyscyplina_Medal_Plec_bez_NA$Sex == "F") & (Kraj_Dyscyplina_Medal_Plec_bez_NA$Sport == "Cycling") & (Kraj_Dyscyplina_Medal_Plec_bez_NA$Medal == "Gold"),]
      Kraje_do_Mapy <- as.data.frame(Medale_Dla_Sport_Plec$Country)
      #Medale_Dla_Kraj <- Medale_Dla_Sport_Plec %>% count(Country, Medal, sort = TRUE) # tego trzeba uzyc jesli wyswietlamy wszyskie medale
      Kraje_Count <- Kraje_do_Mapy %>% count(Country=Medale_Dla_Sport_Plec$Country, sort = TRUE) # zliczamy ilosc wystapienia danego kraju
      colnames(Kraje_Count) <- c("country", "value")
      matched <- joinCountryData2Map(Kraje_Count, joinCode = "NAME", nameJoinColumn = "country")
      
      mapCountryData(matched, nameColumnToPlot = "value", mapTitle = "Medals", catMethod = "pretty", colourPalette = "heat")
      ggplot(data = Medale_Dla_Kraj, aes(x = Country, y = n, fill = Medal)) + geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    })
    
    output$Tab5_OverTheYears <- renderPlot({
      # PARAMETRY ZAWODNIKOW DANEJ DYSCYPLINY W ZALEZNOSCI OD LAT
      
      Wiek_Waga_Wzrost_PrzezLata <- data.frame(Sport = raw$Sport, Sex = raw$Sex, Height = raw$Height, Weight = raw$Weight, Age = raw$Age, Year = raw$Year) # Parametry zawodnikow na przestrzeni lat
      Wiek_Waga_Wzrost_PrzezLata_bez_NA <- Wiek_Waga_Wzrost_PrzezLata %>% filter((!is.na(Sport)) & (!is.na(Weight)) & (!is.na(Age)) & (!is.na(Height) & (!is.na(Year)))) # Dyscyplina - BMI - Plec -- Bez NA
      Wiek_Waga_Wzrost_PrzezLata_filtered <- Wiek_Waga_Wzrost_PrzezLata_bez_NA %>% filter((Weight >= 35 & Age >= 18) | (Weight <= 160 & Age < 18))
      Wiek_Waga_Wzrost_PrzezLata_selected <- Wiek_Waga_Wzrost_PrzezLata_filtered[(Wiek_Waga_Wzrost_PrzezLata_filtered$Sex == input$Tab5_Sex) & (Wiek_Waga_Wzrost_PrzezLata_filtered$Sport == input$Tab5_Sport),]
      PrzezLata <- aggregate(Wiek_Waga_Wzrost_PrzezLata_selected[,3], list(Wiek_Waga_Wzrost_PrzezLata_selected$Year), mean)
      
      ggplot(data = PrzezLata, aes(x = Group.1, y = PrzezLata[,2])) + geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

