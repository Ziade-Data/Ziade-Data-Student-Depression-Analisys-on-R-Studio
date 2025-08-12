# Depression in Students: Data Analysis Project

## Overview
This project focuses on analyzing data related to depression among students. The goal is to explore, clean, and analyze the dataset to uncover insights and patterns that could help in understanding the factors associated with depression in this demographic.

## Project Structure
The project is organized as follows:

- **Data**: Contains the dataset used for the analysis. (Taken from Kaggle)
- **Report**: Full project and code used for the analysis. 
- **Script**: Includes R script for data cleaning, exploration, and analysis.
- **Output**: Stores the generated plots, and tables.
- **README.md**: This file, providing an overview of the project.

## Dataset
The dataset used in this project contains information on various factors related to depression in students. It includes variables such as:

- **Demographics**: Age, Gender, etc.
- **Academic Performance**: CGPA, Academic Pressure, Study Satisfaction, Degree and work/study hours
- **Mental Health Indicators**: Depression scores,  suicidal thoughts, Family History of Mental Illness, Dietary Habits and Sleep Duration

## Data Cleaning and Preparation
The data cleaning process involved:

1. **Handling Missing Values**: Identifying and imputing missing data.
2. **Outlier Detection**: Identifying and addressing outliers.
3. **Data Transformation**: Normalizing and scaling data where necessary.

## Exploratory Data Analysis (EDA)
The EDA phase included:

- **Univariate Analysis**: Exploring individual variables.
- **Bivariate Analysis**: Examining relationships between pairs of variables.
- **Multivariate Analysis**: Investigating interactions among multiple variables.

## Key Findings
Some of the key findings from the analysis include:

- **Correlation between Dietary and Sleep habits and Depression**: As expected, people with healthy dietary habits report the less amount of depression, while, students with moderate and specially unhealthy habits, are the ones with the most amount of cases of depression. This combined with sleeping habits, could give us some insight about habits and it correlation with depression. 
- **Gender Differences**: Female students tend to report higher levels of depression compared to male students.
- **Impact of academic pressure **: Both female and male depression are spotted on academic pressures between 3 and 5. And likewise, most of the students who don't report on having depression, are the ones with the lowest scores for academic pressure. 

## Visualizations
The project includes various visualizations to better understand the data:

- **Histograms**: For distribution of depression scores.
- **Scatter Plots**: To visualize relationships between variables.
- **Box Plots**: For comparing depression levels across different groups.
- **Bar Graphs**: For few variable categories. 

## Conclusion
This analysis provides valuable insights into the factors associated with depression among students. The findings could be used to inform interventions and support programs aimed at improving mental health in educational settings.

## How to Reproduce the Analysis
To reproduce the analysis:

1. Clone the repository.
2. Open the R Markdown file in RStudio.
3. Install the necessary R packages.
4. Run the R Markdown file to generate the analysis and reports.

## Dependencies
The following R packages are required to run the analysis:

- `tidyverse`
- `ggplot2`
- `dplyr`
- `caret`
- `rmarkdown`
- `skimr`
- `here`
- `janitor`
- `readxl`

## Contact
For any questions or further information, please contact me at [daniela.ziade@hotmail.com].
