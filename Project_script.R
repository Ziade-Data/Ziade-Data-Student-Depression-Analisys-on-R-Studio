library(tidyverse)
library(readxl)
library(janitor)
library(ggplot2)
library(here)
library(tidyr)
library(skimr)

Student_Depression_Dataset <- read_csv("C:/Users/Daniela/Desktop/Student Depression Proyect/Student Depression Dataset.csv")

#GENDER TABLE 
gender_table <- table(Student_Depression_Clean$Gender)
gender_table_df <- as.data.frame(gender_table)
colnames(gender_table_df) <- c(x="Gender", y="Count")
print(gender_table_df)

#GENDER BAR GRAPH
ggplot(data = Student_Depression_Clean)+
  geom_bar(mapping = aes(x=Gender, fill = Gender), color ="Black")+
  geom_text(
    stat = "count",
    aes(x = Gender, label = after_stat(count)),
    vjust = -0.5,  
    size = 4      
  ) +
  labs(
    title = "GENDER DISTRIBUTION FEMALE VS MALE",
    x= "Gender", 
    y= "Participants amount"
  )+
  theme_minimal()+
  scale_fill_manual(values = c("#A358B6", "#2DCAB0"))+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1
    )
  )


ggplot(data = Student_Depression_Dataset)+
  geom_histogram(mapping = aes(x="Age"), bins = 6)

#AGE DENSITY GRAPHIC 
ggplot(data = Student_Depression_Clean) +
  geom_density(aes(x=Age),
               fill = "#2DCAB0",
               alpha = 1
               )

#AGE HISTOGRAM 
ggplot(data = Student_Depression_Clean)+
  geom_histogram(
    aes(x=Age),
    bins = 40,
    fill = "#2DCAB0"
  )+
  labs(
    title = "AGE DISTRIBUTION",
    X= "Age",
    y= "Frecuency"
  )+
  scale_x_continuous(
    breaks = seq(from= 15, to = 60, by = 5)  
  ) +
  scale_y_continuous(
    breaks = seq(from= 0, to = 2400, by = 200)  
  ) +
  theme_minimal()+
  scale_fill_manual(values = c("", "#2DCAB0"))+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1
    )
  )

    


#AGE VIOLING GRAPH
ggplot(data = Student_Depression_Clean)+
  geom_violin(
    aes(x="", y=Age),
    fill = "#2DCAB0"
  )

#AGE BOXPLOT GRAPH 
ggplot(data = Student_Depression_Clean)+
  geom_boxplot(
    aes(x="", y=Age),
    fill = "#2DCAB0"
  )

#AGE TABLE 
Age_table_df <- as.data.frame(Age_table)

colnames(Age_table_df) <- c(x="EDAD", y="COUNT")

Age_table_df

Student_Depression_Clean <- drop_na(Student_Depression_Clean)

skim_without_charts(Student_Depression_Clean)



#CLEAN CITY ROW FROM WRONG VALUES
Student_Depression_Clean <- Student_Depression_Clean[Student_Depression_Clean$City != "3.0", ]

Student_Depression_Clean <- Student_Depression_Clean[Student_Depression_Clean$City != "Less than 5 Kalyan", ]

Student_Depression_Clean <- Student_Depression_Clean[Student_Depression_Clean$City != "ME", ]

Student_Depression_Clean <- Student_Depression_Clean[Student_Depression_Clean$City != "City", ]

Student_Depression_Clean <- Student_Depression_Clean[Student_Depression_Clean$City != "3.0", ]



city_count <- table(Student_Depression_Clean$City)
city_count_df <- as.data.frame(city_count)
city_count_df

profession_table <- table(Student_Depression_Clean$Profession)
profession_table_df <- as.data.frame(profession_table)
profession_table_df

academic_pressure_table <- table(Student_Depression_Clean$`Academic Pressure`)
academic_pressure_table_df <- as.data.frame(academic_pressure_table)
colnames(academic_pressure_table_df) <- c(x="Academic Pressure", y="Count")
academic_pressure_table_df


#ACADEMIC PREASSURE BAR GRAPH 
ggplot(data = Student_Depression_Clean)+
  geom_bar(
    aes(x=`Academic Pressure`),
    fill = "#2DCAB0"
  )+
  geom_text(
    stat = "count",
    aes(x = `Academic Pressure`, label = after_stat(count)),
    vjust = -0.5,  
    size = 4      
  )+
  scale_x_continuous(
    breaks = seq(from=0, to=5, by=1)
  )+
  labs(
    title = "ACADEMIC PREASSURE"
  )+
  theme_bw()+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1
    )
  )

#work PREASSURE TABLE 

work_preassure_table <- table(Student_Depression_Clean$`Work Pressure`)

work_preassure_table_df <- as.data.frame(work_preassure_table)

colnames(work_preassure_table_df) <- c(x= "Work Preassure", y= "Count")

work_preassure_table_df


Student_Depression_Clean <- Student_Depression_Clean %>% 
  select(-"Work Pressure")


job_satisfation_table <- table(Student_Depression_Clean$`Job Satisfaction`)

job_satisfation_table_df <- as.data.frame(job_satisfation_table)

colnames(job_satisfation_table_df) <- c(x="Job Satis", y="Count")


Student_Depression_Clean <- Student_Depression_Clean %>% 
  select(-"Job Satisfaction")


#CGPA 

CGPA_table <- table(Student_Depression_Clean$CGPA)

CGPA_table_df <- as.data.frame(CGPA_table)

colnames(CGPA_table_df) <- c(x="CGPA SCORE", y="Count")

CGPA_table_df


#CGPA HISTOGRAM 
ggplot(data = Student_Depression_Clean) +
  geom_histogram(
    aes(x = CGPA), 
    bins = 30,
    fill = "#2DCAB0",
    color = "black"
    )+
  scale_x_continuous(
    breaks= seq(from=5, to=(max(Student_Depression_Clean$CGPA)), by=0.5))+
  scale_y_continuous(
    breaks = seq(from=0, to=3000, by=500)
  )+
  labs(
    title = "CGPA DISTRIBUTION AMONG STUDENTS"
  )+
  theme_bw()+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1))


#STUDY SATISFACTION

study_satisfaction_table <- table(Student_Depression_Clean$`Study Satisfaction`)

study_satisfaction_table_df <- as.data.frame(study_satisfaction_table)

colnames(study_satisfaction_table_df) <- c(x="STUDY SATISFACTION", y="COUNT")

study_satisfaction_table_df


#STUDY SATISFACTION BAR GRAPH 
ggplot(data = Student_Depression_Clean)+
  geom_bar(
    aes(x=`Study Satisfaction`),
    fill = "#AD34DA",
    color = "black"
  )+
  geom_text(
    stat = "count",
    aes(x = `Study Satisfaction`, label = after_stat(count)),
    vjust = -0.5,  
    size = 4      
  )+
  scale_x_continuous(
    breaks = seq(from=0, to=5, by=1)
  )+
  labs(
    title = "STUDY SATISFACTION"
  )+
  theme_bw()+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1
    )
  )



#SLEEP DURATION 

sleep_duration_table <- table(Student_Depression_Clean$`Sleep Duration`)

sleep_duration_table_df <- as.data.frame(sleep_duration_table)

colnames(sleep_duration_table_df) <- c(x="SLEEP DURATION", y="COUNT")

sleep_duration_table_df

#SLEEP DURATION BAR GRAPH 
ggplot(data = Student_Depression_Clean)+
  geom_bar(
    aes(x=`Sleep Duration`),
    fill = "#AD31DA",
    color = "black"
  )+
  geom_text(
    stat = "count",
    aes(x = `Sleep Duration`, label = after_stat(count)),
    vjust = -0.5,  
    size = 4      
  )+
  labs(
    title = "SLEEP DURATION"
  )+
  theme_bw()+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1
    )
  )


#DIETARY HABITS

dietary_habits_table <- table(Student_Depression_Clean$`Dietary Habits`)

dietary_habits_table_df <- as.data.frame(dietary_habits_table)

colnames(dietary_habits_table_df) <- c(x="DIETARY HABITS", y="COUNT")

dietary_habits_table_df

#Reorganizing the categories to be displayed how I prefer
Student_Depression_Clean$`Dietary Habits` <- factor(
  Student_Depression_Clean$`Dietary Habits`,
  levels = c("Unhealthy", "Moderate", "Healthy", "Others")
)

#DIETARY HABITS BAR GRAPH 
ggplot(data = Student_Depression_Clean)+
  geom_bar(
    aes(x=`Dietary Habits`),
    fill = "#AD31DA",
    color = "black"
  )+
  geom_text(
    stat = "count",
    aes(x = `Dietary Habits`, label = after_stat(count)),
    vjust = -0.5,  
    size = 4      
  )+
    labs(
    title = "DIETARY HABITS"
  )+
  theme_bw()+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1
    )
  )


#DEGREE

degree_table <- table(Student_Depression_Clean$Degree)

degree_table_df <- as.data.frame(degree_table)

colnames(degree_table_df) <- c(x="DEGREE", y="COUNT")

degree_table_df


#Have you ever had suicidal thoughts ?


suicidal_thoughts_table <- table(Student_Depression_Clean$`Have you ever had suicidal thoughts ?`)

suicidal_thoughts_table_df <- as.data.frame(suicidal_thoughts_table)

colnames(suicidal_thoughts_table_df) <- c(x="Have you ever had suicidal thoughts ?", y="COUNT")

suicidal_thoughts_table_df

ggplot(data = Student_Depression_Clean)+
  geom_bar(mapping = aes(x=`Have you ever had suicidal thoughts ?`),
           fill = "#AD31DA",
           color = "black"
           )+
    geom_text(
      stat = "count",
      aes(x = `Have you ever had suicidal thoughts ?`, label = after_stat(count)),
      vjust = -0.5,  
      size = 4      
    )+
      labs(
        title = "SUICIDAL THOUGHTS ON STUDENTS"
      )+
      theme_bw()+
      theme(
        plot.title = element_text(
          family = "serif",       
          face = "bold",         
          size = 16,              
          hjust = 0.5,            
          vjust = 1
        )
      )


#WORK/STUDY HOURS 

work_study_hours_table <- table(Student_Depression_Clean$`Work/Study Hours`)

work_study_hours_table_df <- as.data.frame(work_study_hours_table)

colnames(work_study_hours_table_df) <- c(x="Work/Study Hours", y="COUNT")

work_study_hours_table_df


ggplot(data = Student_Depression_Clean) +
  geom_histogram(
    aes(x = `Work/Study Hours`), 
    bins = 12,
    fill = "#8BE4AE",
    color = "black"
  )+
  scale_x_continuous(
    breaks= seq(from=0, to=12, by=1))+
  scale_y_continuous(
    breaks = seq(from=0, to=4500, by=500)
  )+
  labs(
    title = "STUDY/WORK HOURS SPENT BY STUDENTS"
  )+
  theme_bw()+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1))


#FINANCIAL STRESS


financial_stress_table <- table(Student_Depression_Clean$`Financial Stress`)

financial_stress_table_df <- as.data.frame(financial_stress_table)

colnames(financial_stress_table_df) <- c(x="Financial Stress", y="COUNT")

financial_stress_table_df


#FINANCIAL STRESS BAR GRAPH 
ggplot(data = Student_Depression_Clean)+
  geom_bar(
    aes(x=`Financial Stress`),
    fill = "#AD34DA",
    color = "black"
  )+
  geom_text(
    stat = "count",
    aes(x = `Financial Stress`, label = after_stat(count)),
    vjust = -0.5,  
    size = 3      
  )+
  scale_x_continuous(
    breaks = seq(from=1, to=5, by=1)
  )+
  labs(
    title = "FINANCIAL STRESS"
  )+
  theme_bw()+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1
    )
  )


#FAMILY HISTORY OF MENTAL ILLNES 

family_history_of_mental_illness_table <- table(Student_Depression_Clean$`Family History of Mental Illness`)

family_history_of_mental_illness_table_df <- as.data.frame(family_history_of_mental_illness_table)

colnames(family_history_of_mental_illness_table_df) <- c(x="Family History of Mental Illness", y="COUNT")

family_history_of_mental_illness_table_df


#FAMILY HISTORY OF MENTAL ILLNES BAR GRAPH 
ggplot(data = Student_Depression_Clean)+
  geom_bar(mapping = aes(x=`Family History of Mental Illness`),
           fill = "#AD31DA",
           color = "black"
  )+
  geom_text(
    stat = "count",
    aes(x = `Family History of Mental Illness`, label = after_stat(count)),
    vjust = -0.5,  
    size = 3      
  )+
  labs(
    title = "FAMILY HISTORY OF MENTAL ILLNES ",
    y=""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1
    )
  )


#DEPRESSION 

#Transforming 0 and 1 values into "Yes" and "No"
Student_Depression_Clean$Depression <- factor(Student_Depression_Clean$Depression, 
                                              levels = c(0, 1), 
                                              labels = c("No", "Yes"))

#Student depression Bar chart
ggplot(data = Student_Depression_Clean)+
  geom_bar(aes(x=Depression), fill = "#AD31DA",color = "black")+
  geom_text(
    stat = "count",
    aes(x=Depression, label = after_stat(count)),
    vjust = 1,
    size = 3,
    hjust = 0.5
  )+
  labs(
    title = "DEPRESSION AMONG STUDENTS",
    x = "",
    y=""
  )+
  theme_bw()+
  scale_x_discrete()+
  theme(
    plot.title = element_text(
      family = "serif",       
      face = "bold",         
      size = 16,              
      hjust = 0.5,            
      vjust = 1
    )
  )

  
  


#AGE AGAINST DEPRESSION 

ggplot(data = Student_Depression_Clean)+
  geom_histogram(aes(x=Age, fill = Depression, color = "black"),
                 bins = 60)+
  labs(
    title = "STUDENT DEPRESSION BY AGE RANGE",
    y=""
  )+
  theme_bw()


#DEPRESSION AGAINST GENDER 

ggplot(data = Student_Depression_Clean)+
  geom_bar(aes(x=Depression, fill = Gender),
           position = "dodge")+
  geom_text(stat = "count",
            aes(x=Depression, label = after_stat(count), group = Gender),
            vjust = 0.5,
            size =3,
  )+
  labs(
    title = "GENDER VS DEPRESSION"
  )+
  theme_bw()



#barra dogde
ggplot(data = Student_Depression_Clean) +
  geom_bar(aes(x = Depression, fill = Gender), position = "dodge") +
  labs(title = "Depresión por género", x = "Depresión", y = "Cantidad") +
  theme_minimal()

#Bar stack
ggplot(data = Student_Depression_Clean) +
  geom_bar(aes(x = Depression, fill = Gender), position = "fill") +
  labs(title = "Proporción de depresión por género", x = "Depresión", y = "Proporción") +
  theme_minimal() 


#Points graph
ggplot(data = Student_Depression_Clean) +
  geom_point(aes(x = Age, y = Depression, color = Gender)) +
  labs(title = "Relación entre edad y puntaje de depresión", x = "Edad", y = "Puntaje de depresión") +
  theme_minimal()



#DISTRIBUTION OF DEPRESSION BY ACADEMIC PREASURE"
ggplot(data = Student_Depression_Clean) +
  geom_boxplot(aes(x = Depression, y = `Academic Pressure`, fill = Gender)) +
  labs(title = "DISTRIBUTION OF DEPRESSION BY ACADEMIC PREASURE", x = "Depresión", y = "Academic preasure") +
  theme_bw()


ggplot(data = Student_Depression_Clean) +
  geom_violin(aes(x = Depression, y = CGPA, fill = Gender)) +
  labs(title = "DENSITY OF DEPRESSION BY CGPA SCORE", x = "Depression", y = "CGPA score") +
  theme_minimal()


#ACADEMIC PRESSURE BY CGPA
ggplot(data = Student_Depression_Clean) +
  geom_point(aes(x = `Academic Pressure`, y = CGPA)) +
  labs(title = "ACADEMIC PRESSURE BY CGPA", x = "Academic Pressure", y = "CGPA Score") +
  theme_bw()


#STUDY SATISFACTION BY ACADEMIC PRESSURE
ggplot(data = Student_Depression_Clean) +
  geom_violin(aes(x = `Academic Pressure`, y = `Study Satisfaction`)) +
  labs(title = "STUDY SATISFACTION BY ACADEMIC PRESSURE", x = "Academic Pressure", y = "Study Satisfaction") +
  theme_bw()


#DISTRIBUTION OF DEPRESSION BY STUDY SATISFACTION"
ggplot(data = Student_Depression_Clean) +
  geom_bar(aes(x = `Study Satisfaction`, fill = Depression)) +
  scale_x_continuous(breaks = seq(from = 0, to = 5, by = 1))+
  labs(title = "DISTRIBUTION OF DEPRESSION BY STUDY SATISFACTION", x = "Study Satisfaction", y = "") +
  theme_bw()



#DISTRIBUTION OF DEPRESSION BY SLEEP DURATION "
ggplot(data = Student_Depression_Clean) +
  geom_bar(aes(x = `Sleep Duration`, fill = Depression)) +
  labs(title = "DISTRIBUTION OF DEPRESSION BY SLEEP DURATION", x = "Sleep Duration", y = "") +
  theme_bw()


#DISTRIBUTION OF DEPRESSION BY DIETARY HABITS"
ggplot(data = Student_Depression_Clean) +
  geom_bar(aes(x = `Dietary Habits`, fill = Depression)) +
  scale_fill_manual(values = c("No" = "#4DE4CA", "Sí" = "#B750CB")) +
  labs(title = "DISTRIBUTION OF DEPRESSION BY DIETARY HABITS", x = "Dietary Habits", y = "") +
  theme_bw()



#DISTRIBUTION OF DIETARY HABITS BY SLEEP DURATION"
ggplot(data = Student_Depression_Clean) +
  geom_bar(aes(x = `Dietary Habits`, fill = `Sleep Duration`)) +
  scale_fill_manual(values = c("Less than 5 hours" = "#E95261", "5-6 hours" = "#FA4759", "More than 8 hours" = "#39C6D5", "7-8 hours" = "#46DAEB")) +
  labs(title = "DISTRIBUTION OF DIETARY HABITS BY SLEEP DURATION", x = "Dietary Habits", y = "") +
  theme_bw()



#DISTRIBUTION OF SUICIDAL THOUGHTS AND DEPRESSION"
ggplot(data = Student_Depression_Clean) +
  geom_bar(aes(x = Depression, fill = `Have you ever had suicidal thoughts ?`)) +
  scale_fill_manual(values = c("No" = "#4DE4CA", "Yes" = "#B750CB")) +
  labs(title = "DISTRIBUTION OF SUICIDAL THOUGHTS AND DEPRESSION", x = "Suicidal thoughts", y = "") +
  theme_bw()


#DISTRIBUTION OF DEPRESSION AND FAMILY HISTORY OF MENTAL ILLNESS"
ggplot(data = Student_Depression_Clean) +
  geom_bar(aes(x = Depression, fill = `Family History of Mental Illness`)) +
  scale_fill_manual(values = c("No" = "#4DE4CA", "Yes" = "#B750CB")) +
  labs(title = "DISTRIBUTION OF DEPRESSION AND FAMILY HISTORY OF MENTAL ILLNESS", x = "Depression", y = "") +
  theme_bw()


#WORK/STUDY HOURS AND DEPRESSION
ggplot(data = Student_Depression_Clean)+
  geom_histogram(aes(x=`Work/Study Hours`, fill = Depression),
                 bins = 12)+
  labs(
    title = "WORK/STUDY HOURS AND DEPRESSION",
    y=""
  )+
  theme_bw()


#DISTRIBUTION OF DEPRESSION AND FAMILY HISTORY OF MENTAL ILLNESS"
ggplot(data = Student_Depression_Clean) +
  geom_bar(aes(x = `Financial Stress`, fill = Depression)) +
  labs(title = "DISTRIBUTION OF DEPRESSION AND FAMILY HISTORY OF MENTAL ILLNESS", x = "Depression", y = "") +
  theme_bw()




profession_table_df <- profession_table_df %>%
  arrange(desc(COUNT))

CGPA_table <- table(Student_Depression_Clean$CGPA)

CGPA_table_df <- as.data.frame(CGPA_table)

colnames(CGPA_table_df) <- c(x="CGPA SCORE", y="Count")

CGPA_table_df$`CGPA SCORE` <- as.integer(CGPA_table_df$`CGPA SCORE`)

CGPA_table_df <- CGPA_table_df %>%
  filter(`CGPA SCORE` != 0)

CGPA_table_df



ggplot(data = Student_Depression_Clean)+
  geom_histogram(aes(x=Age, fill = Depression),
                 bins = 60)+
  labs(
    title = "STUDENT DEPRESSION BY AGE RANGE",
    y=""
  )+
  theme_bw()
