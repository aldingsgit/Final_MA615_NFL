pacman::p_load(
  tidyverse,
  data.table,
  XML,
  RCurl,
  httr,
  rvest,
  magrittr,
  benford.analysis,
  gridExtra,
  knitr,
  png,
  jpeg,
  shinydashboard,
  shiny,
  plotly,
  scales,
  zipcode,
  devtools,
  leaflet,
  rsconnect
)


#vector of URLs below:

nfl_teams <- c("arizona-cardinals", "atlanta-falcons", "baltimore-ravens", "buffalo-bills", "carolina-panthers", "chicago-bears", "cincinnati-bengals", "cleveland-browns", "dallas-cowboys", "denver-broncos","detroit-lions","green-bay-packers","houston-texans","indianapolis-colts","jacksonville-jaguars","kansas-city-chiefs","los-angeles-chargers","los-angeles-rams","miami-dolphins", "minnesota-vikings","new-england-patriots","new-orleans-saints","new-york-giants","new-york-jets","oakland-raiders","philadelphia-eagles","pittsburgh-steelers","san-francisco-49ers","seattle-seahawks","tampa-bay-buccaneers","tennessee-titans","washington-redskins") 

result  <- list() #formatting result as list

#for loop to extract tables of rosters below

for(i in 1:length(nfl_teams)) {
  url <- paste0("https://www.lineups.com/nfl/roster/", nfl_teams[i])
  tble <- read_html(url) %>%
    html_node("table") %>%
    html_table(fill = TRUE)
  
  result[[i]] <- tble #list of 32 tables - 1 for each team
  
  result[[i]]['Team_name'] = nfl_teams[i] #assigning variable to identify what team each player is on
}

all_teams <- do.call("rbind", result) %>% #merging all 32 tables
  mutate(
    Name1 = sub("^(\\S*\\s+\\S+).*", "\\1", Name)) 
#creating new column of names because existing has dulplicate of name from picture of player     on website

all_teams$Name <- NULL #deleting original name column

colnames(all_teams)[16] <- "Name" #changing name1 to name

clean1<-all_teams[c(16,1:15)] %>% #reorder columns to put name first
  filter(Height != 0 & Weight != 0) %>% #used to extract inches
  mutate(Height_length = nchar(Height)) %>% #see whether extracting 1 or 2 digits for inches
  mutate(Height_ft = as.numeric(substr(Height, start = 1, stop = 1)),
         Height_in = 
           ifelse(Height_length == 4,
                  as.numeric(substr(Height, start = 3, stop = 3)),
                  as.numeric(substr(Height, start = 3, stop = 4)))) %>% #extracting inches
  mutate(Height_inches = Height_ft*12 + Height_in) #converting to inches

clean1 <- subset(clean1,Name != "Andrew East") #Removing miscategorized backup long snapper

#Renaming columns for dplyr formatting
colnames(clean1)[colnames(clean1)=="Exp."] <- "Exp"
colnames(clean1)[colnames(clean1)=="Draft Round"] <- "Draft_round"
colnames(clean1)[colnames(clean1)=="Draft Pick"] <- "Draft_pick"

#Selecting relevant columns for final data frame
Final <- select(clean1, Name, Pos, Team_name, Height_inches, Weight, Age, Exp, Rating, Depth, Drafted, Draft_round, Draft_pick, College)

set.seed(2345)

Final_obs<-nrow(Final)
Final$Height_inches_uniform<- Final$Height_inches+runif(Final_obs, min = -0.5, max = 0.5)
Position_list<-c(unique(Final$Pos))
Team_list<-c(unique(Final$Team_name))
Final_Depth <- filter(Final, Depth <= 4)
OL_Final <- filter(Final, Pos == "OT"|Pos == "OG"|Pos == "OL"|Pos == "C")
OL_obs <- nrow(OL_Final)
set.seed(1222)
Uniform_decimals_OL <- runif(OL_obs, min =  -0.5, max = 0.5)
OL_Final$Height_inches_uniform<- OL_Final$Height_inches + Uniform_decimals_OL

Mean_Weight_OL<-mean(OL_Final$Weight)
SD_Weight_OL<-sd(OL_Final$Weight)

##ALL THIS BELOW IS CALCULATIONS TO RETRIEVE SUSPECTS FOR DIFFERENT OFFENSIVE LINE BINS
#BUCKET ONE (277-281 lbs)
Bucket_one_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 277 &  OL_Final$Weight <= 281.5,]))
Bucket_one_OL_weight_theoretical<-pnorm(q = 281, mean = Mean_Weight_OL, sd = SD_Weight_OL)*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_one_deviation<- Bucket_one_OL_weight_actual - Bucket_one_OL_weight_theoretical #Observed - actual
OL_Chi_squared_1<-Bucket_one_deviation^2 / Bucket_one_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET TWO (282-286 lbs)
Bucket_two_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 281.5 &  OL_Final$Weight <= 286.5,]))
Bucket_two_OL_weight_theoretical<-(pnorm(q = 286, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 282, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_two_deviation<- Bucket_two_OL_weight_actual - Bucket_two_OL_weight_theoretical #Observed - actual
OL_Chi_squared_2<-Bucket_two_deviation^2 / Bucket_two_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET THREE (287-291 lbs)
Bucket_three_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 286.5 &  OL_Final$Weight <= 291.5,]))
Bucket_three_OL_weight_theoretical<-(pnorm(q = 291, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 287, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_three_deviation<- Bucket_three_OL_weight_actual - Bucket_three_OL_weight_theoretical #Observed - actual
OL_Chi_squared_3<-Bucket_three_deviation^2 / Bucket_three_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET FOUR (292-296 lbs)
Bucket_four_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 291.5 &  OL_Final$Weight <= 296.5,]))
Bucket_four_OL_weight_theoretical<-(pnorm(q = 296, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 292, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_four_deviation<- Bucket_four_OL_weight_actual - Bucket_four_OL_weight_theoretical #Observed - actual
OL_Chi_squared_4<-Bucket_four_deviation^2 / Bucket_four_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET FIVE (297-301 lbs)
Bucket_five_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 296.5 &  OL_Final$Weight <= 301.5,]))
Bucket_five_OL_weight_theoretical<-(pnorm(q = 301, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 296, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_five_deviation<- Bucket_five_OL_weight_actual - Bucket_five_OL_weight_theoretical #Observed - actual
OL_Chi_squared_5<-Bucket_five_deviation^2 / Bucket_five_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET SIX (302-306 lbs)
Bucket_six_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 301.5 &  OL_Final$Weight <= 306.5,]))
Bucket_six_OL_weight_theoretical<-(pnorm(q = 306, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 302, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_six_deviation<- Bucket_six_OL_weight_actual - Bucket_six_OL_weight_theoretical #Observed - actual
OL_Chi_squared_6<-Bucket_six_deviation^2 / Bucket_six_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET SEVEN (307-311 lbs)
Bucket_seven_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 306.5 &  OL_Final$Weight <= 311.5,]))
Bucket_seven_OL_weight_theoretical<-(pnorm(q = 311, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 307, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_seven_deviation<- Bucket_seven_OL_weight_actual - Bucket_seven_OL_weight_theoretical #Observed - actual
OL_Chi_squared_7<-Bucket_seven_deviation^2 / Bucket_seven_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET EIGHT (312-316 lbs)
Bucket_eight_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 311.5 &  OL_Final$Weight <= 316.5,]))
Bucket_eight_OL_weight_theoretical<-(pnorm(q = 316, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 312, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_eight_deviation<- Bucket_eight_OL_weight_actual - Bucket_eight_OL_weight_theoretical #Observed - actual
OL_Chi_squared_8<-Bucket_eight_deviation^2 / Bucket_eight_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET NINE (317-321 lbs)
Bucket_nine_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 316.5 &  OL_Final$Weight <= 321.5,]))
Bucket_nine_OL_weight_theoretical<-(pnorm(q = 321, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 317, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_nine_deviation<- Bucket_nine_OL_weight_actual - Bucket_nine_OL_weight_theoretical #Observed - actual
OL_Chi_squared_9<-Bucket_nine_deviation^2 / Bucket_nine_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET TEN (322-326 lbs)
Bucket_ten_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 321.5 &  OL_Final$Weight <= 326.5,]))
Bucket_ten_OL_weight_theoretical<-(pnorm(q = 326, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 322, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_ten_deviation<- Bucket_ten_OL_weight_actual - Bucket_ten_OL_weight_theoretical #Observed - actual
OL_Chi_squared_10<-Bucket_ten_deviation^2 / Bucket_ten_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET ELEVEN (327-331 lbs)
Bucket_eleven_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 326.5 &  OL_Final$Weight <= 331.5,]))
Bucket_eleven_OL_weight_theoretical<-(pnorm(q = 331, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 327, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_eleven_deviation<- Bucket_eleven_OL_weight_actual - Bucket_eleven_OL_weight_theoretical #Observed - actual
OL_Chi_squared_11<-Bucket_eleven_deviation^2 / Bucket_eleven_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET TWELVE (332-336 lbs)
Bucket_twelve_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 331.5 &  OL_Final$Weight <= 336.5,]))
Bucket_twelve_OL_weight_theoretical<-(pnorm(q = 336, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 332, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_twelve_deviation<- Bucket_twelve_OL_weight_actual - Bucket_twelve_OL_weight_theoretical #Observed - actual
OL_Chi_squared_12<-Bucket_twelve_deviation^2 / Bucket_twelve_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET THIRTEEN (337-341 lbs)
Bucket_thirteen_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 336.5 &  OL_Final$Weight <= 341.5,]))
Bucket_thirteen_OL_weight_theoretical<-(pnorm(q = 341, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 337, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_thirteen_deviation<- Bucket_thirteen_OL_weight_actual - Bucket_thirteen_OL_weight_theoretical #Observed - actual
OL_Chi_squared_13<-Bucket_thirteen_deviation^2 / Bucket_thirteen_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET FOURTEEN (342-346 lbs)
Bucket_fourteen_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 341.5 &  OL_Final$Weight <= 346.5,]))
Bucket_fourteen_OL_weight_theoretical<-(pnorm(q = 346, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 341, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_fourteen_deviation<- Bucket_fourteen_OL_weight_actual - Bucket_fourteen_OL_weight_theoretical #Observed - actual
OL_Chi_squared_14<-Bucket_fourteen_deviation^2 / Bucket_fourteen_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET FIFTEEN (347-351 lbs)
Bucket_fifteen_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 346.5 &  OL_Final$Weight <= 351.5,]))
Bucket_fifteen_OL_weight_theoretical<-(pnorm(q = 351, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 347, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_fifteen_deviation<- Bucket_fifteen_OL_weight_actual - Bucket_fifteen_OL_weight_theoretical #Observed - actual
OL_Chi_squared_15<-Bucket_fifteen_deviation^2 / Bucket_fifteen_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET SIXTEEN (352-356 lbs)
Bucket_sixteen_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 351.5 &  OL_Final$Weight <= 356.5,]))
Bucket_sixteen_OL_weight_theoretical<-(pnorm(q = 356, mean = Mean_Weight_OL, sd = SD_Weight_OL)-pnorm(q = 352, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_sixteen_deviation<- Bucket_sixteen_OL_weight_actual - Bucket_sixteen_OL_weight_theoretical #Observed - actual
OL_Chi_squared_16<-Bucket_sixteen_deviation^2 / Bucket_sixteen_OL_weight_theoretical #Calculating chi squared for bucket

#BUCKET SEVENTEEN (357-361 lbs)
Bucket_seventeen_OL_weight_actual<-nrow((OL_Final[OL_Final$Weight >= 356.5 &  OL_Final$Weight <= 361.5,]))
Bucket_seventeen_OL_weight_theoretical<-(1-pnorm(q = 357, mean = Mean_Weight_OL, sd = SD_Weight_OL))*nrow(OL_Final) #Number of observations in set times expected CDF within the bucket
Bucket_seventeen_deviation<- Bucket_seventeen_OL_weight_actual - Bucket_seventeen_OL_weight_theoretical #Observed - actual
OL_Chi_squared_17<-Bucket_seventeen_deviation^2 / Bucket_seventeen_OL_weight_theoretical #Calculating chi squared for bucket

Chisq_OL<-rbind(OL_Chi_squared_1,OL_Chi_squared_2,OL_Chi_squared_3,OL_Chi_squared_4,OL_Chi_squared_5,OL_Chi_squared_6,OL_Chi_squared_7,OL_Chi_squared_8,OL_Chi_squared_9,OL_Chi_squared_10,OL_Chi_squared_11,OL_Chi_squared_12,OL_Chi_squared_13,OL_Chi_squared_14,OL_Chi_squared_15,OL_Chi_squared_16,OL_Chi_squared_17)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "NFL Player Stats"),
  dashboardSidebar(
    sidebarMenu(
      
      id = "sidebarmenu",
      menuItem("Summary", tabName = "a", icon = icon("info-sign", lib = "glyphicon")),
      menuItem("Analysis",
               tabName = "Analysis", icon = icon("signal", lib = "glyphicon"),
               menuItem("Benford Analysis - Height",
                        tabName = "bfd1",
                        icon = icon("folder-open", lib = "glyphicon")
               ),
               menuItem("Benford Analysis - Weight",
                        tabName = "bfd2",
                        icon = icon("book", lib = "glyphicon")
               ),
               menuItem("Distribution Analysis",
                        tabName = "vis",
                        icon = icon("globe", lib = "glyphicon")
               ),
               menuItem("Other Visualizations",
                        tabName = "data",
                        icon = icon("gift",lib = "glyphicon")
               ),
               menuItem("Suspect Analysis",
                        tabName = "map",
                        icon = icon("search",lib = "glyphicon")
               ),
               menuItem("Data Table",
                        tabName = "datatable",
                        icon = icon("list-alt",lib = "glyphicon")
               )
      ),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "a",
        
        fluidRow( column(12,
                         
                         box(width=NULL, solidHeader = TRUE, status = "success",
                             title="Overview",
                             h4("An Analysis of Reported Heights and Weights of Current NFL Players:",style = "font-family: 'Arial'," ),
                             h5("  Benford Analysis of Height: Do NFL player heights obey Benford's Law?"),
                             h5("  Benford Analysis of Weight: Do NFL player weights obey Benford's Law?"),
                             h5("  Distribution Analysis: Are the height and weight distributions normal? How about if we group players by position?"),
                             h5("  Other Visualizations: What other relationships can we see in the dataset?"),
                             h5("  Suspect Analysis: What are some suspect values in our dataset?")
                         )),
                  column(12,
                         
                         box(width=NULL, solidHeader = TRUE, status = "success",
                             title="Results",
                             h5("  Neither player height, nor player weights follow Benford distribution but should be approximately normal if they are similar to the overall population of adult male heights and weights"),
                             h5("  Population further segmented into groups by player position and examined for normality: most intra-position heights and weights are approximately normal"),
                             h5("  For offensive lineman weight distribution, I reproduced the Benford getsuspects function by running  chi-squared tests on different quantiles of the distribution to see which ranges of weights deviated the most from the expected standard normal densities"),
                             h5("  After significant analytical investigation, I visualize and explore other relationships in dataset")
                         ))
                  
                  
                  
        ),mainPanel(imageOutput("row1"))
      ),
      tabItem(
        tabName = "map",
        
        fluidRow(
          column(12,
                 
                 box(width=NULL, solidHeader = TRUE, status = "success",
                     title="Suspects Analysis within Offensive Line Distributions",
                     h5("  We visualize the offensive line height and weight distributions"),
                     h5("  Users can recreate the same analysis by checking just the positions belonging to the offensive line in the Distribution Analysis Tab: Center, Guard, Offensive Tackle, Offensive Line"),
                     h5("  We see that heights are normally distributed but not weights for offensive linemen"),
                     h5("  We plot the chi squared deviations of every weight bin and discover that the heaviest bin in the offensive line histogram has the largest deviation"),
                     h5("  This is how Benford Package getsuspects function works - picks out observations in the groups with the greatest chi squared deviations"),
                     h5("  After some Googling around, we discover that the two players, Marcus Cannon and Zach Banner, have been listed at different weight over the years and have struggled keeping their weight under control"),
                     h5("  Our method of getting suspects appears to have been fruitful in bringing unusual cases to our attention")               
                 )),
          tabBox(
            height = 500 , width = NULL,
            tabPanel("Chi Squred Deviations For Weight", plotOutput("plot22"), width = 500, height = 500),
            tabPanel("Suspect One - Marcus Cannon (358lbs)", imageOutput("row2")),
            tabPanel("Suspect Two - Zach Banner (361lbs)", imageOutput("row3")),
            tabPanel("Weight Histogram", plotOutput("plot18"), width = 500, height = 500),
            tabPanel("Weight Q-Q Plot", plotOutput("plot20"), width = 500, height = 300),
            tabPanel("Weight Normality Test", textOutput("print3"), width = 500, height = 500),
            tabPanel("Height Histogram", plotOutput("plot19"), width = 500, height = 500),
            tabPanel("Height Q-Q Plot", plotOutput("plot21"), width = 500, height = 300),
            tabPanel("Height Normality Test", textOutput("print4"), width = 500, height = 500)
          )
        )
      ),
      #First Tab
      tabItem(
        tabName = "vis",
        
        fluidRow(
          box(
            title = "Are These Distributions Normal? - Explore players' heights and weights below", status = "success", collapsible = TRUE,
            solidHeader = TRUE, 
            h5("Explore histograms, QQ-plots, and normality tests based on inclusion or exclusion of positions. (Bottom right)"),
            h5("Use slider to control bin size of heights and weights of players in inches and pounds respectively. (Bottom left)"),
            h5("For Shapiro-Wilk normality test, p-values less than 0.05 indicate we reject null hypothesis that samples came from normal distribution at 95% LOC."),
            width = 12, height = 150
          ),
          tabBox(
            height = 500 ,
            tabPanel("Histogram", plotlyOutput("plot2"), width = 500, height = 500),
            tabPanel("Q-Q Plot", plotOutput("plot8"), width = 500, height = 300),
            tabPanel("Normality Test", textOutput("print1"), width = 500, height = 500)
          ),
          tabBox(
            height = 500 ,
            tabPanel("Histogram", plotlyOutput("plot1"), width = 500, height = 500),
            tabPanel("Q-Q Plot", plotOutput("plot7"), width = 500, height = 300),
            tabPanel("Normality Test", textOutput("print2"), width = 500, height = 500)
          ),
          column(width = 6,
                 box(title = "Controls for Height and Weight Bins", width = NULL, status = "warning",
                     "Size of Height Bin in Inches",
                     sliderInput("slider1", "Number of Inches Per Bin:", 1, 6, value = 1)
                     # selectInput("Month","Select Month", 
                     #             choices = 1:12),
                     ,
                     "Size of Weight Bin in Pounds",
                     sliderInput("slider2", "Number of Pounds Per Bin", 1, 25, value = 5)
                     # selectInput("Month","Select Month", 
                     #             choices = 1:12),
                     
                 )
                 
          ),
          
          
          column(width = 6,
                 box(title = "Controls for Inclusion of Positions (Full-Screen and Scroll to Bottom)", width = NULL, status = "warning",
                     checkboxGroupInput("positions","Positions to include:",
                                        choices =  c("Quarterback" = "QB", 
                                                     "Running Back" = "RB",
                                                     "Full Back" = "FB",
                                                     "Wide Receiver" = "WR",
                                                     "Tight End" = "TE",
                                                     "Offensive Tackle" = "OT",
                                                     "Guard" = "G",
                                                     "Center" = "C",
                                                     "Offensive lineman" = "OL",
                                                     "Defensive Tackle" = "DT",
                                                     "Defensive End" = "DE",
                                                     "Nose Tackle" = "NT",
                                                     "Defensive Line" = "DL",
                                                     "Inside Linebacker" = "ILB",
                                                     "Outside Linebacker" = "OLB",
                                                     "Linebacker" = "LB",
                                                     "Cornerback" = "CB",
                                                     "Defensive Back" = "DB",
                                                     "Free Safety" = "FS",
                                                     "Strong Safety" = "SS",
                                                     "Safety" = "S",
                                                     "Punter" = "P",
                                                     "Kicker" = "K",
                                                     "Long Snapper" = "LS"
                                        ),  
                                        selected = Position_list))
          )
        )
      ),
      tabItem(
        tabName = "datatable",
        fluidRow(
          box(title = "Original Dataset", status = "success", collapsible = TRUE,
              solidHeader = TRUE, DT::dataTableOutput("table"), width = 12, height = 600),
          column(width = 6,
                 box(title = "Controls for Inclusion of Positions (Full-Screen and Scroll to Bottom)", width = NULL, status = "warning",
                     checkboxGroupInput("positions3","Positions to include:",
                                        choices =  c("Quarterback" = "QB", 
                                                     "Running Back" = "RB",
                                                     "Full Back" = "FB",
                                                     "Wide Receiver" = "WR",
                                                     "Tight End" = "TE",
                                                     "Offensive Tackle" = "OT",
                                                     "Guard" = "G",
                                                     "Center" = "C",
                                                     "Offensive lineman" = "OL",
                                                     "Defensive Tackle" = "DT",
                                                     "Defensive End" = "DE",
                                                     "Nose Tackle" = "NT",
                                                     "Defensive Line" = "DL",
                                                     "Inside Linebacker" = "ILB",
                                                     "Outside Linebacker" = "OLB",
                                                     "Linebacker" = "LB",
                                                     "Cornerback" = "CB",
                                                     "Defensive Back" = "DB",
                                                     "Free Safety" = "FS",
                                                     "Strong Safety" = "SS",
                                                     "Safety" = "S",
                                                     "Punter" = "P",
                                                     "Kicker" = "K",
                                                     "Long Snapper" = "LS"
                                        ),  
                                        selected = "QB")
                 )),
          column(width = 6,
                 box(title = "Controls for Inclusion of Teams (Full-Screen and Scroll to Bottom)", width = NULL, status = "warning",
                     checkboxGroupInput("teams2","Teams to include:",
                                        choices =  c("Arizone Cardinals" = "arizona-cardinals", 
                                                     "Atlanta Falcons" = "atlanta-falcons", 
                                                     "Baltimore Ravens" = "baltimore-ravens", 
                                                     "Buffalo Bills" = "buffalo-bills", 
                                                     "Carolina Panthers" = "carolina-panthers", 
                                                     "Chicago Bears" = "chicago-bears", 
                                                     "Cincinnati Bengals" = "cincinnati-bengals", 
                                                     "Cleveland Browns" = "cleveland-browns", 
                                                     "Dallas Cowboys" = "dallas-cowboys", 
                                                     "Denver Broncos" = "denver-broncos",
                                                     "Detroit Lions" = "detroit-lions",
                                                     "Green Bay Packers" = "green-bay-packers",
                                                     "Houston Texans" = "houston-texans",
                                                     "Indianapolis Colts" = "indianapolis-colts",
                                                     "Jacksonville Jaguars" = "jacksonville-jaguars",
                                                     "Kansas City Chiefs" = "kansas-city-chiefs",
                                                     "Los Angeles Chargers" = "los-angeles-chargers",
                                                     "Los Angeles Rams" = "los-angeles-rams",
                                                     "Miami Dolphins" = "miami-dolphins", 
                                                     "Minnesota Vikings" = "minnesota-vikings",
                                                     "New England Patriots" = "new-england-patriots",
                                                     "New Orleans Saints" = "new-orleans-saints",
                                                     "New York Giants" = "new-york-giants",
                                                     "New York Jets" = "new-york-jets",
                                                     "Oakland Raiders" = "oakland-raiders",
                                                     "Philiadelphia Eagles" = "philadelphia-eagles",
                                                     "Pittsburgh Steelers" = "pittsburgh-steelers",
                                                     "San Francisco 49ers" = "san-francisco-49ers",
                                                     "Seattle Seahawks" = "seattle-seahawks",
                                                     "Tampa Bay Buccaneers" = "tampa-bay-buccaneers",
                                                     "Tennessee Titans" = "tennessee-titans",
                                                     "Washington Redskins" = "washington-redskins"
                                        ),  
                                        selected = Team_list)
                 ))
          
          
          
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          tabBox(
            height = 500 , width = NULL,
            tabPanel("Height & Weight", plotlyOutput("plot9"), width = 1000, height = 500),
            tabPanel("Height & Rating", plotlyOutput("plot11"), width = 1000, height = 500),
            tabPanel("Weight & Rating", plotlyOutput("plot10"), width = 1000, height = 500),
            tabPanel("Experience &. Height", plotlyOutput("plot12"), width = 1000, height = 500),
            tabPanel("Experience & Weight", plotlyOutput("plot13"), width = 1000, height = 500),
            tabPanel("Height & Draft Position", plotlyOutput("plot14"), width = 1000, height = 500),
            tabPanel("Weight & Draft Position", plotlyOutput("plot15"), width = 1000, height = 500),
            tabPanel("Height & Draft Round", plotlyOutput("plot16"), width = 1000, height = 500),
            tabPanel("Weight & Draft Round", plotlyOutput("plot17"), width = 1000, height = 500)
          ),
          column(width = 6,
                 box(title = "Controls for Inclusion of Positions (Full-Screen and Scroll to Bottom)", width = NULL, status = "warning",
                     checkboxGroupInput("positions2","Positions to include:",
                                        choices =  c("Quarterback" = "QB", 
                                                     "Running Back" = "RB",
                                                     "Full Back" = "FB",
                                                     "Wide Receiver" = "WR",
                                                     "Tight End" = "TE",
                                                     "Offensive Tackle" = "OT",
                                                     "Guard" = "G",
                                                     "Center" = "C",
                                                     "Offensive lineman" = "OL",
                                                     "Defensive Tackle" = "DT",
                                                     "Defensive End" = "DE",
                                                     "Nose Tackle" = "NT",
                                                     "Defensive Line" = "DL",
                                                     "Inside Linebacker" = "ILB",
                                                     "Outside Linebacker" = "OLB",
                                                     "Linebacker" = "LB",
                                                     "Cornerback" = "CB",
                                                     "Defensive Back" = "DB",
                                                     "Free Safety" = "FS",
                                                     "Strong Safety" = "SS",
                                                     "Safety" = "S",
                                                     "Punter" = "P",
                                                     "Kicker" = "K",
                                                     "Long Snapper" = "LS"
                                        ),  
                                        selected = "QB")
                 )),
          column(width = 6,
                 box(title = "Controls for Inclusion of Teams (Full-Screen and Scroll to Bottom)", width = NULL, status = "warning",
                     checkboxGroupInput("teams","Teams to include:",
                                        choices =  c("Arizone Cardinals" = "arizona-cardinals", 
                                                     "Atlanta Falcons" = "atlanta-falcons", 
                                                     "Baltimore Ravens" = "baltimore-ravens", 
                                                     "Buffalo Bills" = "buffalo-bills", 
                                                     "Carolina Panthers" = "carolina-panthers", 
                                                     "Chicago Bears" = "chicago-bears", 
                                                     "Cincinnati Bengals" = "cincinnati-bengals", 
                                                     "Cleveland Browns" = "cleveland-browns", 
                                                     "Dallas Cowboys" = "dallas-cowboys", 
                                                     "Denver Broncos" = "denver-broncos",
                                                     "Detroit Lions" = "detroit-lions",
                                                     "Green Bay Packers" = "green-bay-packers",
                                                     "Houston Texans" = "houston-texans",
                                                     "Indianapolis Colts" = "indianapolis-colts",
                                                     "Jacksonville Jaguars" = "jacksonville-jaguars",
                                                     "Kansas City Chiefs" = "kansas-city-chiefs",
                                                     "Los Angeles Chargers" = "los-angeles-chargers",
                                                     "Los Angeles Rams" = "los-angeles-rams",
                                                     "Miami Dolphins" = "miami-dolphins", 
                                                     "Minnesota Vikings" = "minnesota-vikings",
                                                     "New England Patriots" = "new-england-patriots",
                                                     "New Orleans Saints" = "new-orleans-saints",
                                                     "New York Giants" = "new-york-giants",
                                                     "New York Jets" = "new-york-jets",
                                                     "Oakland Raiders" = "oakland-raiders",
                                                     "Philiadelphia Eagles" = "philadelphia-eagles",
                                                     "Pittsburgh Steelers" = "pittsburgh-steelers",
                                                     "San Francisco 49ers" = "san-francisco-49ers",
                                                     "Seattle Seahawks" = "seattle-seahawks",
                                                     "Tampa Bay Buccaneers" = "tampa-bay-buccaneers",
                                                     "Tennessee Titans" = "tennessee-titans",
                                                     "Washington Redskins" = "washington-redskins"
                                        ),  
                                        selected = Team_list)
                 ))
        )
      ),
      tabItem(
        tabName = "bfd1",
        h3("Heights of NFL players range from 66 to 81 inches and don't obey Benford's Law.",size = 10,style = "font-family: 'Arial'," ),
        plotOutput("plot5",height = 600)
      ),
      tabItem(
        tabName = "bfd2",
        h3("Weights of NFL players range from 149 to 362 lbs and also do not obey Benford's Law.",size = 10,style = "font-family: 'Arial'," ),
        plotOutput("plot6",height = 600)
      ),
      tabItem(
        tabName = "about",
        h3("This app includes data on NFL players from the sports analytics website lineups.com. The rosters are dynamically updated every time this app is run.",
           size = 10,style = "font-family: 'Arial'," ),
        br(),
        br(),
        valueBoxOutput("userguide")
      )
    )
  )
)
# Define server logic required to draw a histogram

server <- function(input, output) {
  output$table<- DT::renderDataTable({
    Final_1 <- subset(Final, Pos %in% input$positions3)
    Final_2 <- subset(Final_1, Team_name %in% input$teams2)
    tabledata <- Final_2
    DT::datatable(tabledata, options = list(searching = TRUE,pageLength = 50,lengthMenu = c( 50, 100, 500, 2000), scrollX = T,scrollY = "300px"),rownames= FALSE
    )
  })
  
  output$plot1 <- renderPlotly({
    Final_1 <- subset(Final, Pos %in% input$positions)
    p <- ggplot(Final_1) +
      geom_histogram(aes(x = Weight, y = ..count..),
                     binwidth = input$slider2, fill = "grey", color = "black") +
      ggtitle("NFL Players Weights") +
      ylab("\nCount") +
      xlab("Weights\n") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      coord_flip() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    ggplotly(p)
  })
  
  output$plot2 <- renderPlotly({
    Final_1 <- subset(Final, Pos %in% input$positions)
    pp <- ggplot(Final_1) +
      geom_histogram(aes(x = Height_inches, y = ..count..),
                     binwidth = input$slider1, fill = "grey", color = "black") +
      ggtitle("NFL Players Heights") +
      ylab("\nCount") +
      xlab("Height\n") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      coord_flip() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  
    ggplotly(pp)
  })
  
  output$plot5 <- renderPlot({
    bfd1 <- benford(Final$Height_inches)
    plot(bfd1)
  })
  
  output$plot6 <- renderPlot({
    bfd2 <- benford(Final$Weight)
    plot(bfd2)
  })
  
  output$plot7 <- renderPlot({
    Final_1 <- subset(Final, Pos %in% input$positions)
    G <- qqnorm(Final_1$Weight, main="Normal Q-Q Plot of Weight", ylab = "Sample Quantiles - Weight in lbs"); qqline(Final_1$Weight)
    G
  })
  
  output$plot8 <- renderPlot({
    Final_1 <- subset(Final, Pos %in% input$positions)
    H <- qqnorm(Final_1$Height_inches_uniform, main="Normal Q-Q Plot of Heights\n(Adjusted - See Report)", ylab = "Sample Quantiles - Height in Inches"); qqline(Final_1$Height_inches_uniform)
    H
  })  
  
  output$plot9 <- renderPlotly({
    Final_1 <- subset(Final, Pos %in% input$positions2)
    Final_2 <- subset(Final_1, Team_name %in% input$teams)
    ggplot(Final_2,aes(x = Height_inches, y = Weight, color = Pos)) +
      geom_point(position = "jitter", alpha = 0.3) +
      geom_smooth(method = lm, se = FALSE) +
      ggtitle("NFL Players Height versus Weight") +
      ylab("Weight in lbs\n") +
      xlab("\nHeight in inches")  +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot10 <- renderPlotly({
    Final_1 <- subset(Final, Pos %in% input$positions2)
    Final_2 <- subset(Final_1, Team_name %in% input$teams)
    ggplot(Final_2,aes(x = Weight, y = Rating, color = Pos)) +
      geom_point(position = "jitter", alpha = 0.3) +
      geom_smooth(method = lm, se = FALSE) +
      ggtitle("NFL Players Weight versus Rating") +
      ylab("Ratings (Per Lineups.com)\n") +
      xlab("\nWeight in Lbs")  +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot11 <- renderPlotly({
    Final_1 <- subset(Final, Pos %in% input$positions2)
    Final_2 <- subset(Final_1, Team_name %in% input$teams)
    ggplot(Final_2,aes(x = Height_inches, y = Rating, color = Pos)) +
      geom_point(position = "jitter", alpha = 0.3) +
      geom_smooth(method = lm, se = FALSE) +
      ggtitle("NFL Players Height versus Rating") +
      ylab("Ratings (Per Lineups.com)\n") +
      xlab("\nHeight in Inches")  +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot12 <- renderPlotly({
    Final_1 <- subset(Final, Pos %in% input$positions2)
    Final_2 <- subset(Final_1, Team_name %in% input$teams)
    ggplot(Final_2,aes(x = Exp, y = Height_inches)) +
      geom_point(aes(color = Pos),position = "jitter", alpha = 0.3) +
      geom_smooth(method = lm, se = FALSE) +
      ggtitle("NFL Players Experience versus Height") +
      ylab("Height in Inches\n") +
      xlab("\nYears of Experience in NFL")  +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot13 <- renderPlotly({
    Final_1 <- subset(Final, Pos %in% input$positions2)
    Final_2 <- subset(Final_1, Team_name %in% input$teams)
    ggplot(Final_2,aes(x = Exp, y = Weight)) +
      geom_point(aes(color = Pos),position = "jitter", alpha = 0.3) +
      geom_smooth(method = lm, se = FALSE) +
      ggtitle("NFL Players Experience versus Weight") +
      ylab("Weight in Lbs\n") +
      xlab("\nYears of Experience in NFL")  +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot14 <- renderPlotly({
    Final_1 <- subset(Final, Pos %in% input$positions2)
    Final_2 <- subset(Final_1, Team_name %in% input$teams)
    ggplot(Final_2,aes(x = Height_inches, y = Draft_pick)) +
      geom_point(aes(color = Pos),position = "jitter", alpha = 1) +
      geom_smooth(method = lm, se = FALSE) +
      ggtitle("NFL Players Height versus Draft Position") +
      ylab("Draft Pick Number (Lower Picks are Better)\n") +
      xlab("\nHeight in Inches")  +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot15 <- renderPlotly({
    Final_1 <- subset(Final, Pos %in% input$positions2)
    Final_2 <- subset(Final_1, Team_name %in% input$teams)
    ggplot(Final_2,aes(x = Weight, y = Draft_pick)) +
      geom_point(aes(color = Pos),position = "jitter", alpha = 1) +
      geom_smooth(method = lm, se = FALSE) +
      ggtitle("NFL Players Weight versus Draft Pick") +
      ylab("Draft Pick Number (Lower Picks are Better)\n") +
      xlab("\nWeight in Lbs")  +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$plot16 <- renderPlotly({
    Final_3 <- subset(Final_Depth, Pos %in% input$positions2)
    Final_4 <- subset(Final_3, Team_name %in% input$teams)
    ggplot(na.omit(Final_4), aes(x = Height_inches, y = factor(Draft_round))) + 
      geom_jitter(aes(color=factor(Depth)),alpha=0.5) +     
      ggtitle("NFL Players Height versus Draft Round") +
      labs(x = "\nHeight in Inches", y = "Draft Round\n", color = "Depth Chart\nPosition") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot17 <- renderPlotly({
    Final_3 <- subset(Final_Depth, Pos %in% input$positions2)
    Final_4 <- subset(Final_3, Team_name %in% input$teams)
    ggplot(na.omit(Final_4), aes(x = Weight, y = factor(Draft_round))) + 
      geom_jitter(aes(color=factor(Depth)),alpha=0.5) +     
      ggtitle("NFL Players Weight versus Draft Round") +
      labs(x = "\nWeight in Lbs", y = "Draft Round\n", color = "Depth Chart\nPosition") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot18 <- renderPlot({
    ggplot(OL_Final) +
      geom_histogram(aes(x = Weight, y = ..count..),
                     binwidth = 5, fill = "grey", color = "black") +
      ggtitle("Offensive Lineman Weights\n(5lb Bins)") +
      ylab("\nCount") +
      xlab("Weights\n") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      coord_flip() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      geom_vline(xintercept = mean(OL_Final$Weight), color="red") +
      geom_vline(xintercept = mean(OL_Final$Weight) + sd(OL_Final$Weight), color="blue") +
      geom_vline(xintercept = mean(OL_Final$Weight) - sd(OL_Final$Weight), color="blue") 
  })
  
  output$plot19 <- renderPlot({
    ggplot(OL_Final) +
      geom_histogram(aes(x = Height_inches, y = ..count..),
                     binwidth = 1, fill = "grey", color = "black") +
      ggtitle("Offensive Lineman Heights\n(1 inch Bins)") +
      ylab("\nCount") +
      xlab("Heights\n") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      coord_flip() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      geom_vline(xintercept = mean(OL_Final$Height_inches), color="red") +
      geom_vline(xintercept = mean(OL_Final$Height_inches) + sd(OL_Final$Height_inches), color="blue") +
      geom_vline(xintercept = mean(OL_Final$Height_inches) - sd(OL_Final$Height_inches), color="blue")  
  })
  
  output$plot20 <- renderPlot({
    OL_Height_QQ <- qqnorm(OL_Final$Height_inches_uniform, main="Normal Q-Q Plot of OLine Height\n(Adjusted)", ylab = "Sample Quantiles - Height in Inches"); qqline(OL_Final$Height_inches_uniform)
    OL_Height_QQ
  })
  
  output$plot21 <- renderPlot({
    OL_Weight_QQ <- qqnorm(OL_Final$Weight); qqline(OL_Final$Weight)
    OL_Weight_QQ
  })
  
  output$plot22 <- renderPlot({
    plot(Chisq_OL,
         main = "Chi Squared Difference by Weight Bins for Offensive Linemen\n(Biggest Outlier is Heaviest Bin)",
         xlab="Weight Bins",
         ylab = "Chi-squared Values",
         pch= 21, bg = 73)
  })
  
  output$print1 <- renderPrint({
    Final_1 <- subset(Final, Pos %in% input$positions)
    round(shapiro.test(Final_1$Height_inches_uniform)$p.value,4)
  }) 
  
  output$print2 <- renderPrint({
    Final_1 <- subset(Final, Pos %in% input$positions)
    round(shapiro.test(Final_1$Weight)$p.value,4)
  }) 
  
  output$print3 <- renderPrint({
    round(shapiro.test(OL_Final$Weight)$p.value,4)
  }) 
  
  output$print4 <- renderPrint({
    round(shapiro.test(OL_Final$Height_inches_uniform)$p.value,4)
  }) 
  
  
  output$userguide <- renderUI({
    url <- a("Webpage", href="https://www.lineups.com/nfl/players")
    
    
    tagList("All data scraped and sourced from Lineups.com", url)
  })
  
  output$row1<- renderImage({
    Leg<-"Pics/lineups.png"
    list(src=Leg)
  },deleteFile = FALSE)  
  
  output$row2<- renderImage({
    Leg2<-"Pics/MarcusCannon.jpg"
    list(src=Leg2)
  },deleteFile = FALSE)  
  
  output$row3<- renderImage({
    Leg3<-"Pics/ZachBanner.jpg"
    list(src=Leg3)
  },deleteFile = FALSE)  
}

# Run the application 
shinyApp(ui = ui, server = server)
