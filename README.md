# Introduction 
Welcome to the California COVID Assessment Tool (CalCAT).  This repository contains an application written in Shiny and for use with any US state to assist in assessing the many different models available for understanding COVID-19 transmission and spread. It brings together several data sources that are publically available, and can be supplemented with your own data to improve the assessment. 

# Getting Started
_ATTN: This application is designed to run with Rstudio. If you need to download and install Rstudio go [here](https://rstudio.com/) to get started._ 
1. Enter your state's name in line 47 of the `cal_cat_data_routine.rmd` where it says ```state_name <- "California"```
``Ctrl+Alt+R``
This routine will download data for your state from a number of different modeling groups. Yuo can find out more about them in the Technical notes app the application. 
2. You may also modify the data path for the application (line 56), but it defaults to a subdirectory named for your state, for example `data/CA`. 
3. Once the data routine markdown has run, you can run the app by opening the `global.R` file 

# Build and Test
TODO: Describe and show how to build your code and run the tests. 

# Contribute
TODO: Explain how other users and developers can contribute to make your code better. 

FILL THIS OUT
If you want to learn more about creating good readme files then refer the following [guidelines](https://docs.microsoft.com/en-us/azure/devops/repos/git/create-a-readme?view=azure-devops). You can also seek inspiration from the below readme files:
- [Rstudio](https://rstudio.com/)
- [Shiny](https://shiny.rstudio.com/)



__A gift from California with love.__
_“Together, all things are possible.”_
                -- Cesar Chavez 
