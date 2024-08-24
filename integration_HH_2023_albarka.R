
# Title     :  Albarka Integration at household level 
# R File    :  integration_HH_2023_albarka
# R Project :  Measuring_Albarka_integration
# Dataset   :  DATA_HH_2023_1_   (Excel file from Annual survey in 2023)


#### LOAD NECESSARY PACKAGES #############################################

pacman::p_load(magrittr, pacman, tidyverse, rio)

#### LOAD DATASET AND DATA OF INTERST ####################################

                                          # First import dataset in environment
df <- DATA_HH_2023_1_ %>%                 # Load dataset & named as df 
  as_tibble() %>%                         # df put as a tibble
  select(A07,A23:A24_Aucun,C01:C09,D06_Score_Total_M_nage) %>% # Selecting columns
  print( n = Inf)                         # Show  all the dataframe

# Print and checking column names
print(colnames(df))

# Checking the size of the dataframe
dim(df)

#### PREPARE DATA ########################################################  

# Checking for missing values in each column
colSums(is.na(df))

# Remove rows with missing value and checking changes  
df%<>%                                                                
  drop_na()                  

colSums(is.na(df))
dim(df)

# Transform A24 from char to a factor type column
df%<>%                              
  mutate(A24=factor(A24))%>%        
  print()                          


#### % OF HHS BY TOTAL NUMBER OF ACTIVITY PARTICIPATED ##################

# Creating  a new column  for total activities in which HHs participate
df$tot_activity <- df$`A24_Distribution du cash inconditionnel`+
  df$`A24_Santé et Nutrition_GSAN_MtM`+
  df$A24_CFW+
  df$A24_Cooperative+
  df$A24_AVEC+
  df$A24_CCJ+
  df$A24_GAPRU+
  df$A24_CV+
  df$`A24_COGES-EAU`+
  df$`A24_Ecole des Maris`

# Bar Chart of % of HHs by total number of activities participating 
g <- df %>%
  ggplot(aes(x = tot_activity, y = ..prop.. * 100)) 

g + geom_bar(aes(group = 1)) +
  xlab("Number of activities") +
  ylab("Percentage of Housholds") +
  ggtitle("Distribution of Households by Number of Activities Participated In")+
  theme(plot.title = element_text(hjust = 0.5))  # center the title

# Calculate an print the mean of total activity in which a hh participates
mean <- mean(df$tot_activity)
print(mean)

#### % OF HHS WHO PARTICIPATED TO NO MORE THAN 2 ACTIVITES #################

# creating a column with  dichotomous vaiable for no more than 2 activites
df%<>%
  mutate(
    atleast_2 = case_when(
      tot_activity <= 2  ~ "yes",
      TRUE ~ "no" # All other values
    ))

# Getting the percentage of "yes" responses for at least_2 variabe
percentage_yes <- mean(df$atleast_2 == "yes") * 100
print(paste("the percentage of HHs with no more than 2 activitis is :",
      percentage_yes))

#### % OF HHS BY TOP 5 COMBINAISON OF PARTICIPATION IN ACTIVITIES ########

#  looking at the combiaison of activities HH particpated (Check levels of A24)  
df%>%
  pull(A24)%>%
  levels

# Lump everything but the top five in "lumps_A24"

df%<>%                                
  mutate(                        # Create a new variable that keep the 
    lump_A24 = fct_lump(      # the top 5 value variable and put all
      A24,n = 5))           # other value variable  into  # other 
  
# Check levels   
df%>%
  pull(lump_A24)%>%
  levels 

# Bar Chart
g <-   df %>%
  ggplot (aes(y =lump_A24))
g + geom_bar()


# Vertical Bar Chart with Percentages
g <- df %>%
  ggplot(aes(y = lump_A24, x = ..prop.. * 100)) 

g + geom_bar(aes(group = 1)) +
  xlab("Percentage")+
  ylab ("Acvitiy participate in")


# Vertical Bar Chart with Percentages and Proportion Labels
g <- df %>%
  ggplot(aes(y = lump_A24, x = ..prop.. * 100)) 

g + geom_bar(aes(group = 1), stat = "count") +
  geom_text(
    aes(label = scales::percent(..prop.., accuracy = 0.1), 
        x = ..prop.. * 100, 
        group = 1), 
    stat = "count", 
    vjust = -0.5
  ) +
  xlab("Percentage") +
  ylab ("Acvitiy participate in")+
  ggtitle("Top 5 Most Activity Combinations: Percentage of households")+
  theme(plot.title = element_text(hjust = 0.5))  # center the title


#### % OF HHS WITH AT LEAST ONE WOMAN IN GSAN #####################


# Create a new variable and convert 1 to "yes" and 0 to "no", and then factor it
df$gsan_factor <- factor(df$`A24_Santé et Nutrition_GSAN_MtM`,
                         levels = c(0, 1), labels = c("no", "yes"))

# Calculate the counts for each category
df_count <- df %>%
  count(gsan_factor) %>%
  mutate(prop = n / sum(n) * 100)  # Calculate the proportion of each category

# Create the pie chart
ggplot(df_count, aes(x = "", y = prop, fill = gsan_factor )) +
  geom_bar(stat = "identity", width = 1) +       # Create bar chart
  coord_polar(theta = "y") +                     # bar chart to pie chart
  labs(x = NULL, y = NULL, fill = "gsan membership") +  # Remove axis labels
  theme_void() +                               # Remove background & grid lines
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5)) # Add percentage labels

#### % OF HHS WITH AT LEAST ONE MEMBER IN COOPERATIVE AG AND ELEVAGE ###########

# Create a new variable and convert 1 to "yes" and 0 to "no", and then factor it
df$coop_factor <- factor(df$A24_Cooperative,
                         levels = c(0, 1), labels = c("no", "yes"))

# Calculate the counts for each category
df_count <- df %>%
  count(coop_factor) %>%
  mutate(prop = n / sum(n) * 100)  # Calculate the proportion of each category

# Create the pie chart
ggplot(df_count, aes(x = "", y = prop, fill = coop_factor )) +
  geom_bar(stat = "identity", width = 1) +       # Create bar chart
  coord_polar(theta = "y") +                     # bar chart to pie chart
  labs(x = NULL, y = NULL, fill = "Coop membership") +   # Remove axis labels
  theme_void() +                                 # Removebackground & grid lines
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5)) # Add percentage labels

#### % OF HHS WITH HH MEMBER(s) IN COOPERATIVE AND GSAN ###########

# Creation of variable Coop_gsan
df%<>%
  
  mutate(
    coop_gsan = case_when(
      gsan_factor == "yes" & coop_factor == "yes"  ~ "yes",
      TRUE ~ "no"))
  

df_count <- df %>%
  count(coop_gsan) %>%
  mutate(prop = n / sum(n) * 100)  # Calculate the proportion of each category

# Create the pie chart
ggplot(df_count, aes(x = "", y = prop, fill = coop_gsan )) +
  geom_bar(stat = "identity", width = 1) +       # Create bar chart
  coord_polar(theta = "y") +                     # Convert bar chart to pie chart
  labs(x = NULL, y = NULL, fill = "coop_gsan") + # Remove axis labels
  theme_void() +                                 # Remove background & grid lines
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5)) # Add percentage labels

####  COMPUTING FOOD CONSUMPTION SCORE #######################################

# creating weighted frequency of food group consumption 
df%<>%
  mutate(g1_w = C01 * 2,
         g2_w = C02 * 3,
         g3_w = C03 * 1,
         g4_w = C04 * 1,
         g5_w = C05 * 4,
         g6_w = C06 * 4,
         g7_w = C07 * 0.5,
         g8_w = C08 * 0.5,
         g9_w = C09 * 0
         )
# Calculating the FCS
df%<>%
  mutate(FCS = g1_w + g2_w + g3_w + g4_w + g5_w + g6_w + g7_w +  g8_w + g9_w )  
    
# Getting the summary statistics of FCS
summary(df$FCS)


#### LOOKING AT THE CORRELATION BETWEEN FCS AND NUMBER OF ACTIVITIES ##########

d<- df%>%
  select(FCS, tot_activity)%>%
  print()

# Compute the coefficient of correlation
correlation<- cor(d$tot_activity,d$FCS)
correlation


#### LOOKING AT FCS MEAN DIFFERENCE BETWEEN COOP_gSAN & GSAN MEMBERSHIP########

## FCS Mean  calculation for both group

# Calculate the mean FCS of those of hhs who participated in both gsan & coop
mean_FCS_both <- mean(df$FCS[df$coop_gsan == "yes"])

# Calculate the mean FCS of hhs who only participated in gsan
mean_FCS_only_one <- mean(df$FCS[df$coop_gsan == "no" & df$gsan_factor == "yes"])

# Print the results
print(paste("Mean FCS of hhs who participated in both is:", mean_FCS_both))
print(paste("Mean FCS of hhs who only participated in gsan is :", mean_FCS_only_one))


##  FCS Mean difference  test (Welsh T-test)

# Filter the data for hhs who participated in both and only in gsan
both_activities <- df[df$coop_gsan == "yes", "FCS"]
only_one <- df[df$coop_gsan == "no" & df$gsan_factor == "yes", "FCS"]

# Perform a t-test
t_test_result <- t.test(both_activities, only_one)

# Print the results
print(t_test_result)


#### LOOKING AT THE RELATIONSHIP BETWEEN FCS & PATICIPATION IN COOP & GSAN ####

# Convert categorical variables to factors
df$coop_gsan <- as.factor(df$coop_gsan)

# Run a linear regression model
linear_model <- lm(FCS ~ coop_gsan, data = df)

# Summary of the model
summary(linear_model)



#### % OF HHS WITH HH MEMBER(s) UCT (soeley or combined) ######################

# Create uct variable and convert 1 to "yes" and 0 to "no", and then factor it

df$uct_factor <- factor(df$`A24_Distribution du cash inconditionnel`,
                         levels = c(0, 1), labels = c("no", "yes"))


df_count <- df %>%
  count(uct_factor) %>%
  mutate(prop = n / sum(n) * 100)  # Calculate the proportion of each category

# Create the pie chart
ggplot(df_count, aes(x = "", y = prop, fill = uct_factor )) +
  geom_bar(stat = "identity", width = 1) +       # Create bar chart
  coord_polar(theta = "y") +                     # Convert bar chart to pie chart
  labs(x = NULL, y = NULL, fill = "uct") + # Remove axis labels
  theme_void() +                                 # Remove background & grid lines
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5)) # Add percentage labels


# FCS histogram and density plot together

g <- df %>%
  ggplot(aes( x= FCS,fill = uct_factor)) # Specify what to graph

g + geom_histogram(aes(y = ..density..), bins =15)  +
  geom_density(alpha = 1, color = "purple") +
  theme(legend.position = "none") +
  ggtitle("Distribution & density plot of Food Consumption Score:UTC vs no-UTC
          ") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_grid(uct_factor ~.) 
  
  

#### FCS MEAN DIFFERENCE BETWEEN UTC(SOLELY OR COMBINED) VERSUS NON UTC ##### 

t_test_result <- t.test(FCS ~ uct_factor, data = df)
print(t_test_result)


#### LOOKING AT THE RELATIONSHIP BETWEEN FCS & PATICIPATIONIN UTC ############

# Run a linear regression model
linear_model <- lm(FCS ~ uct_factor, data = df)

# Summary of the model
summary(linear_model)


#  Confirming  percentage for only UCT

result <- df %>%
  summarise(percentage = sum(
    `A24_Distribution du cash inconditionnel` == 1 &
      `A24_Santé et Nutrition_GSAN_MtM` == 0 &
      A24_CFW == 0 &
      A24_Cooperative == 0 &
      A24_AVEC == 0 &
      A24_CCJ == 0 &
      A24_GAPRU == 0 & 
      A24_CV == 0 & 
      `A24_COGES-EAU` == 0 &
      `A24_Ecole des Maris` == 0
  ) / n() * 100)

# Display the result
print(result)

# Creating a column for those with only UCT

df<- df%<>% 
  mutate(only_uct =ifelse(
    `A24_Distribution du cash inconditionnel` == 1 &
      `A24_Santé et Nutrition_GSAN_MtM` == 0 &
      A24_CFW == 0 &
      A24_Cooperative == 0 &
      A24_AVEC == 0 &
      A24_CCJ == 0 &
      A24_GAPRU == 0 & 
      A24_CV == 0 & 
      `A24_COGES-EAU` == 0 &
      `A24_Ecole des Maris` == 0,
    "yes",
    "no"
      ) )  %>%
  mutate(only_uct = factor(only_uct))
  
# chart 
  
df_count <- df %>%
  count(only_uct) %>%
  mutate(prop = n / sum(n) * 100)  # Calculate the proportion of each category

# Create the pie chart
ggplot(df_count, aes(x = "", y = prop, fill = only_uct )) +
  geom_bar(stat = "identity", width = 1) +       # Create bar chart
  coord_polar(theta = "y") +                     # Convert bar chart to pie chart
  labs(x = NULL, y = NULL, fill = " Only uct") + # Remove axis labels
  theme_void() +                                 # Remove background & grid lines
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5)) # Add percentage labels

#### % OF HHS PARTICIPATING IN COOP OR GSAN ##################################

# creating a new column
df<- df%<>% 
  mutate(Coop_or_gsan =ifelse(
      `A24_Santé et Nutrition_GSAN_MtM` == 1 |
      A24_Cooperative == 1 ,
    "yes",
    "no"
  ) )  %>%
  mutate(coop_or_gsan_factor = factor(Coop_or_gsan))

# chart 

df_count <- df %>%
  count(coop_or_gsan_factor) %>%
  mutate(prop = n / sum(n) * 100)  # Calculate the proportion of each category

# Create the pie chart
ggplot(df_count, aes(x = "", y = prop, fill = coop_or_gsan_factor )) +
  geom_bar(stat = "identity", width = 1) +       
  coord_polar(theta = "y") +                     
  labs(x = NULL, y = NULL, fill = " Coop or GSAN") + 
  theme_void() +                                 
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5)) 


#### % OF HHS PARTICIPATING UCT and  'Coop or GSAN' #########################

f<- df%<>% 
  mutate(utc_Coop_or_gsan =ifelse(
    uct_factor == "yes" |coop_or_gsan_factor == "yes",
    "yes",
    "no"
  ) )  %>%
  mutate(utc_coop_or_gsan_factor = factor(utc_Coop_or_gsan))

# chart 

df_count <- df %>%
  count(utc_coop_or_gsan_factor) %>%
  mutate(prop = n / sum(n) * 100)  # Calculate the proportion of each category

# Create the pie chart
ggplot(df_count, aes(x = "", y = prop, fill = utc_coop_or_gsan_factor )) +
  geom_bar(stat = "identity", width = 1) +       
  coord_polar(theta = "y") +                     
  labs(x = NULL, y = NULL, fill = "UTC and 'coop & gsan' ") + 
  theme_void() +                                 
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5)) 

#### rCsI MEAN DIFFERENCE BETWEEN UTC(SOLELY OR COMBINED) VERSUS NON UTC #####

# renaming variable for ease

df %<>%
  rename(rcsi = D06_Score_Total_M_nage) 

df3 <- df %>%                 
  select(rcsi) %>%   # Selecting columns
  print( n = Inf)

t_test_result <- t.test(rcsi ~ uct_factor, data = df)
print(t_test_result)


# Getting the summary statistics of rcsu
summary(df3$rcsi)


# rCSI histogram and density plot together


g2 <- df3 %>%
  ggplot(aes( x= rcsi)) # Specify what to graph

g2 + geom_histogram(aes(y = ..density..), bins =15)  +
  geom_density(alpha = 1, color = "purple") +
  theme(legend.position = "none") 
  



# rCSI histogram and density plot together

g <- df %>%
  ggplot(aes( x= rcsi,fill = uct_factor)) # Specify what to graph

g + geom_histogram(aes(y = ..density..), bins =15)  +
  geom_density(alpha = 1, color = "purple") +
  theme(legend.position = "none") +
  ggtitle("Distribution & density plot of Coping strategy index :UTC vs no-UTC
          ") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_grid(uct_factor ~.) 

# Scatterplot (rcsi vs FCS), jittered, colored by UCT 

g <- df %>%
  ggplot(aes(rcsi,
             FCS,
             color = uct_factor ))

g + geom_jitter(alpha =0.5) 


# Compute the coefficient of correlation
correlation<- cor(df$rcsi,df$FCS)
correlation


#### LOOKING AT THE RELATIONSHIP BETWEEN FCS, UTC,GSAn ############

# Run a linear regression model
linear_model <- lm(FCS ~ rcsi + uct_factor + coop_gsan, data = df)

# Summary of the model
summary(linear_model)
