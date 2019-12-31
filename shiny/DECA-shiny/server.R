#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(Cairo)

setwd('/home/craig/Projects/DECA')
source('plots.R')
setwd('/home/craig/Projects/DECA/shiny')

plot_list <- c('Infrastructure','Access and use','Digital literacy',
               'Digital literacy gender gaps','Affordability','Digital society',
               'Censorship and civil liberties','Freedom on the Net',
               'Privacy and surveillance','Information integrity',
               'Cybersecurity','EIU Global Microscope',
               'GSMA Mobile Money Regulatory Index','Findex',
               'Findex barriers to access','Findex access gaps',
               'IMF Financial Access Survey','Digital trade and e-commerce')


shinyServer(function(input, output) {
  output$dotPlot <- renderPlot({
    this_plot <- switch(match(input$plot_type,plot_list),
                        infra_plot,access_plot,dig_lit_plot,dig_lit_gap_plot,
                        afford_plot,society_plot,censorship_plot,fotn_plot,privacy_plot,
                        info_plot,cyber_plot,eiu_plot,mmri_plot,findex_plot,barrier_plot,
                        findex_gaps_plot,imf_plot,trade_plot) 
    this_plot(input$country,
              shade_fraction=input$shade,
              overall_score=ifelse(input$score,'PC1','none'),
              show_pred=input$pred,
              num_pcs=input$pcs)
    })
  
})
