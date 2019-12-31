# Development steps for DECA shiny app

# Single J2SR-style plot with country selectable from drop-down list - done
# Toggle switch for vertical summary line - done
# Toggle switch for regression analysis - done
# Select number of PCs and p-value threshold - done
# Make sure that countries for which no data will be shown aren't included in drop-down list
# (explained here: https://www.davidsolito.com/post/conditional-drop-down-in-shiny/)

# Double-check that everything included in lit reviews is available
# How to get nicer fonts (ggplot)?
# Maybe this: https://stackoverflow.com/questions/42347968/how-to-get-r-shiny-font-rendering-right-on-linux
# How to make sure irrelevant options aren't displayed? (e.g., num pcs when no predictions)
# Can I get mouse-overs of absolute values from the plot?
# (I think so: https://stackoverflow.com/questions/27965931/tooltip-when-you-mouseover-a-ggplot-on-shiny)
# how to re-size plots to reflect number of rows?
# get reasonable output when plots are rendered without a country selection

# Single scatterplot with multiple selectable countries
# Add mouse-over to view names of other countries?
# Add ability to select from a list of standard plots

# Figure out how to optimize for efficient data loading (at startup instead of when first plot is rendered)
# Design workflow that allows people to quickly generate all plots for desk review

library(shiny)

setwd('/home/craig/Projects/DECA')
source('utils.R')
#source('plots.R')
setwd('/home/craig/Projects/DECA/shiny')

plot_list <- c('Infrastructure','Access and use','Digital literacy',
               'Digital literacy gender gaps','Affordability','Digital society',
               'Censorship and civil liberties','Freedom on the Net',
               'Privacy and surveillance','Information integrity',
               'Cybersecurity','EIU Global Microscope',
               'GSMA Mobile Money Regulatory Index','Findex',
               'Findex barriers to access','Findex access gaps',
               'IMF Financial Access Survey','Digital trade and e-commerce')

# if I want to filter the country list based on the plot type chosen, I need to be able to generate a list of viable countries for each plot
# this means countries where data exists for at least one variable in the plot
# the best way to do this is probably to change my approach -- instead of a separate function for each plot, I have a
# master data table with all of the variables included in any plot and a named list of the variables required for each plot.

shinyUI(fluidPage(
  
  titlePanel("J2SR-style test plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput('plot_type','Select a plot:',choices=c('Select one' = '',plot_list)),
      selectizeInput('country','Select a country:',choices=c('Select one' = '',sort(all_countries))),
      checkboxInput('score','Show summary score?',TRUE),
      sliderInput('shade','Fraction to shade:',0,1,0.5),
      checkboxInput('pred','Show predictions?',FALSE),
      numericInput('pcs','Number of principal components:',5,min=1,max=30)
    ),
    
    mainPanel(
       plotOutput("dotPlot")
    )
  )
))
