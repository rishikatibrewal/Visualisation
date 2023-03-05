library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
library(readr)

police <- read.csv("https://raw.githubusercontent.com/rishikatibrewal/Visualisation/main/Innocent%20Deaths%20caused%20by%20Police_%20Corrected.csv",header=T,nrows=3000)
colnames(police)[colnames(police) == 'Date.of.injury.resulting.in.death.DD.MM.YYYY.'] ='Date.of.injury'
police$Year=format(as.Date(police$Date.of.injury,"%d-%m-%Y"),"%Y")
police$Age.groups=cut(police$Age,breaks=seq(0,110,by=5))
state_freq = table(police$State)
state_freq = as.data.frame(state_freq)
colnames(state_freq) <- c("State","Frequency")
df_ca=filter(police,police$State=="CA")
df_mo=filter(police,police$State=="MO")
df_00=filter(police,police$Year=="2000")
df_01=filter(police,police$Year=="2001")
df_m=filter(police,police$Gender=="Male")
df_f=filter(police,police$Gender=="Female")
df_2=filter(police,police$Age.groups=="(20,25]")
df_3=filter(police,police$Age.groups=="(25,30]")
df_w=filter(police,police$Race=="European-American/White")
df_b=filter(police,police$Race=="African-American/Black")

ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title="Component 2"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Univariate",tabName = "Problem_1"),
            menuItem("Bivariate",tabName = "Problem_2"),
            menuItem("State Wise Study",tabName = "Problem_3"),
            menuItem("Year Wise Study",tabName = "Problem_4"),
            menuItem("Gender Wise Study",tabName = "Problem_5"),
            menuItem("Age Group Wise Study",tabName = "Problem_6"),
            menuItem("Race Wise Study",tabName = "Problem_7")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                "Problem_1",
                tabsetPanel(
                    tabPanel(
                        "Interactive Map",
                        fluidRow(plotlyOutput("map_usa"))
                    ),
                    tabPanel(
                        "Bar Plot",
                        fluidRow(
                            sidebarPanel(
                                selectInput("var", 
                                            "Categorical Variable:",
                                            c("Age Groups" = "Age.groups",
                                              "Race" = "Race",
                                              "Year"="Year",
                                              "State" = "State",
                                              "Gender"="Gender",
                                              "Highest Level of Force"="Highest.level.of.force",
                                              "Intended Use of Force"="Intended.use.of.force",
                                              "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                ),
                                actionButton("go","Plot Bar Graph")
                            ),
                            plotOutput("bar")
                        )
                    ),
                    tabPanel(
                        "Pie Chart",
                        fluidRow(
                            sidebarPanel(
                                selectInput("var2", 
                                            "Categorical Variable:",
                                            c("Race" = "Race",
                                              "Year"="Year",
                                              "Gender"="Gender",
                                              "Intended Use of Force"="Intended.use.of.force",
                                              "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                ),
                                actionButton("go2","Plot Pie Chart")
                            ),
                           plotOutput("pie")
                        )
                    ),
                    tabPanel(
                        "Table",
                        fluidRow(
                            sidebarPanel(
                                selectInput("var3", 
                                            "Categorical Variable:",
                                            c("Age Groups" = "Age.groups",
                                              "Race" = "Race",
                                              "Year"="Year",
                                              "State" = "State",
                                              "Gender"="Gender",
                                              "Highest Level of Force"="Highest.level.of.force",
                                              "Intended Use of Force"="Intended.use.of.force",
                                              "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                ),
                                actionButton("go3","View Data Table")
                            ),
                            box(h4("Table"),tableOutput("tab"),align="center",collapsible=T,background="purple")
                        )
                    )
                )
            ),
            tabItem(
                "Problem_2",
                        fluidRow(
                            sidebarPanel(
                                selectInput("var4", 
                                            "Categorical Variable:",
                                            c("Age Groups" = "Age.groups",
                                              "Race" = "Race",
                                              "Year"="Year",
                                              "State" = "State",
                                              "Gender"="Gender",
                                              "Highest Level of Force"="Highest.level.of.force",
                                              "Intended Use of Force"="Intended.use.of.force",
                                              "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                ),
                                selectInput("var5",
                                            "Categorical Variable:",
                                            c("Age Groups" = "Age.groups",
                                              "Race" = "Race",
                                              "Year"="Year",
                                              "State" = "State",
                                              "Gender"="Gender",
                                              "Highest Level of Force"="Highest.level.of.force",
                                              "Intended Use of Force"="Intended.use.of.force",
                                              "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                ),
                                actionButton("go4","Plot Graph")
                            ),
                            plotOutput("bibar")
                        )
                ),
            tabItem(
                "Problem_3",
                tabsetPanel(
                    tabPanel(
                        "California",
                        fluidRow(
                            sidebarPanel(
                                selectInput("var6", 
                                            "Categorical Variable:",
                                            c("Age Groups" = "Age.groups",
                                              "Race" = "Race",
                                              "Year"="Year",
                                              "Gender"="Gender",
                                              "Highest Level of Force"="Highest.level.of.force",
                                              "Intended Use of Force"="Intended.use.of.force",
                                              "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                ),
                                actionButton("go5","Plot Bar Graph")
                            ),
                            plotOutput("cabar")
                        )
                    ),
                    tabPanel("Missouri",
                        fluidRow(
                            sidebarPanel(
                                selectInput("var7", 
                                            "Categorical Variable:",
                                            c("Age Groups" = "Age.groups",
                                              "Race" = "Race",
                                              "Year"="Year",
                                              "Gender"="Gender",
                                              "Highest Level of Force"="Highest.level.of.force",
                                              "Intended Use of Force"="Intended.use.of.force",
                                              "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                ),
                                actionButton("go6","Plot Bar Graph")
                            ),
                            plotOutput("mobar")
                        )
                    ))
                ),
                tabItem(
                    "Problem_4",
                    tabsetPanel(
                        tabPanel(
                            "2000",
                            fluidRow(
                                sidebarPanel(
                                    selectInput("var8", 
                                                "Categorical Variable:",
                                                c("Age Groups" = "Age.groups",
                                                  "Race" = "Race",
                                                  "State"="State",
                                                  "Gender"="Gender",
                                                  "Highest Level of Force"="Highest.level.of.force",
                                                  "Intended Use of Force"="Intended.use.of.force",
                                                  "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                    ),
                                    actionButton("go7","Plot Bar Graph")
                                ),
                                plotOutput("bar00")
                            )
                        ),
                        tabPanel("2001",
                                 fluidRow(
                                     sidebarPanel(
                                         selectInput("var9", 
                                                     "Categorical Variable:",
                                                     c("Age Groups" = "Age.groups",
                                                       "Race" = "Race",
                                                       "State"="State",
                                                       "Gender"="Gender",
                                                       "Highest Level of Force"="Highest.level.of.force",
                                                       "Intended Use of Force"="Intended.use.of.force",
                                                       "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                         ),
                                         actionButton("go8","Plot Bar Graph")
                                     ),
                                    plotOutput("bar01")
                                 )
                        )
                )
            ),
            tabItem(
                "Problem_5",
                tabsetPanel(
                    tabPanel(
                        "Male",
                        fluidRow(
                            sidebarPanel(
                                selectInput("var10", 
                                            "Categorical Variable:",
                                            c("Age Groups" = "Age.groups",
                                              "Race" = "Race",
                                              "State"="State",
                                              "Year"="Year",
                                              "Highest Level of Force"="Highest.level.of.force",
                                              "Intended Use of Force"="Intended.use.of.force",
                                              "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                ),
                                actionButton("go9","Plot Bar Graph")
                            ),
                            plotOutput("barm")
                        )
                    ),
                    tabPanel("Female",
                             fluidRow(
                                 sidebarPanel(
                                     selectInput("var11", 
                                                 "Categorical Variable:",
                                                 c("Age Groups" = "Age.groups",
                                                   "Race" = "Race",
                                                   "State"="State",
                                                   "Year"="Year",
                                                   "Highest Level of Force"="Highest.level.of.force",
                                                   "Intended Use of Force"="Intended.use.of.force",
                                                   "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                     ),
                                     actionButton("go10","Plot Bar Graph")
                                 ),
                                 plotOutput("barf")
                             )
                    )
                )
            ),
            tabItem(
                "Problem_6",
                tabsetPanel(
                    tabPanel(
                        "(20,25]",
                        fluidRow(
                            sidebarPanel(
                                selectInput("var12", 
                                            "Categorical Variable:",
                                            c("Gender" = "Gender",
                                              "Race" = "Race",
                                              "State"="State",
                                              "Year"="Year",
                                              "Highest Level of Force"="Highest.level.of.force",
                                              "Intended Use of Force"="Intended.use.of.force",
                                              "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                ),
                                actionButton("go11","Plot Bar Graph")
                            ),
                            plotOutput("bar2")
                        )
                    ),
                    tabPanel("(25,30]",
                             fluidRow(
                                 sidebarPanel(
                                     selectInput("var13",
                                                 "Categorical Variable:",
                                                 c("Gender" = "Gender",
                                                   "Race" = "Race",
                                                   "State"="State",
                                                   "Year"="Year",
                                                   "Highest Level of Force"="Highest.level.of.force",
                                                   "Intended Use of Force"="Intended.use.of.force",
                                                   "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                     ),
                                     actionButton("go12","Plot Bar Graph")
                                 ),
                                 plotOutput("bar3")
                             )
                    )
                )
            ),
            tabItem(
                "Problem_7",
                tabsetPanel(
                    tabPanel(
                        "European-American/White",
                        fluidRow(
                            sidebarPanel(
                                selectInput("var14", 
                                            "Categorical Variable:",
                                            c("Age Groups"="Age.groups",
                                              "Gender" = "Gender",
                                              "State"="State",
                                              "Year"="Year",
                                              "Highest Level of Force"="Highest.level.of.force",
                                              "Intended Use of Force"="Intended.use.of.force",
                                              "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                ),
                                actionButton("go13","Plot Bar Graph")
                            ),
                            plotOutput("barw")
                        )
                    ),
                    tabPanel("African-American/Black",
                             fluidRow(
                                 sidebarPanel(
                                     selectInput("var15",
                                                 "Categorical Variable:",
                                                 c("Age Groups"="Age.groups",
                                                   "Gender" = "Gender",
                                                   "State"="State",
                                                   "Year"="Year",
                                                   "Highest Level of Force"="Highest.level.of.force",
                                                   "Intended Use of Force"="Intended.use.of.force",
                                                   "Foreknowledge of Mental Illness"="Foreknowledge.of.mental.illness")
                                     ),
                                     actionButton("go14","Plot Bar Graph")
                                 ),
                                plotOutput("barb")
                             )
                    )
                )
            )
        )
    )
)
server <- function(input,output){
    output$map_usa <- renderPlotly({
        map_state = state_freq %>%
            select(State, Frequency) %>%
            mutate(hover = paste0(State,",", Frequency))
        map = plot_geo(map_state,
                       locationmode = 'USA-states') %>%
            add_trace(locations = ~State,
                      z = ~Frequency,
                      zmin = min(state_freq$Frequency),
                      zmax = max(state_freq$Frequency),
                      color = ~Frequency,
                      text = ~hover,
                      hoverinfo = 'text') %>%
            layout(geo = list(scope = 'usa'), font=list(),
                   title = "Innocent Deaths Caused in each state of the USA")
        isolate({map})})
    observeEvent(input$go,{
        u_data <- eventReactive(input$go,{
            switch(input$var,
                   "Age.groups"=police$Age.groups,
                   "Race" = police$Race,
                   "Year"=police$Year,
                   "State" = police$State,
                   "Gender"=police$Gender,
                   "Highest.level.of.force"=police$Highest.level.of.force,
                   "Intended.use.of.force"=police$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=police$Foreknowledge.of.mental.illness)
        })
        output$bar <- renderPlot({
                isolate({ggplot(police,aes(x=u_data(),fill=u_data()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var))+xlab(paste(input$var))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
        })})
    })
    observeEvent(input$go2,{
        u_data2 <- eventReactive(input$go2,{
            switch(input$var2,
                   "Race" = police$Race,
                   "Year"=police$Year,
                   "Gender"=police$Gender,
                   "Intended.use.of.force"=police$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=police$Foreknowledge.of.mental.illness)
        })
        output$pie <- renderPlot({
            isolate({pie(table(u_data2()),lty=1,clockwise="TRUE",cex=0.7,radius=1.01,main=paste("Pie chart of",input$var2))
                })})
    })
    observeEvent(input$go3,{
        output$tab <- renderTable(bordered=T,{
            tabl=table(police[input$var3])
            isolate({tabl})       
        })
    })
    observeEvent(input$go4,{
        bi_data <- eventReactive(input$go4,{
            switch(input$var4,
                   "Age.groups"=police$Age.groups,
                   "Race" = police$Race,
                   "Year"=police$Year,
                   "State" = police$State,
                   "Gender"=police$Gender,
                   "Highest.level.of.force"=police$Highest.level.of.force,
                   "Intended.use.of.force"=police$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=police$Foreknowledge.of.mental.illness)
        })
        bi_data2 <- eventReactive(input$go4,{
            switch(input$var5,
                   "Age.groups"=police$Age.groups,
                   "Race" = police$Race,
                   "Year"=police$Year,
                   "State" = police$State,
                   "Gender"=police$Gender,
                   "Highest.level.of.force"=police$Highest.level.of.force,
                   "Intended.use.of.force"=police$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=police$Foreknowledge.of.mental.illness)
        })
        output$bibar <- renderPlot({
            if(bi_data()!=bi_data2()){
                isolate({ggplot(police,aes(x=bi_data(),fill=bi_data2()))+geom_bar()+coord_flip()+ggtitle(paste("Graph of",input$var4,"for each",input$var5))+xlab(paste(input$var4))+scale_x_discrete(guide=guide_axis(n.dodge=1.5)) +ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })}
        })
    })
    observeEvent(input$go5,{
        u_data3 <- eventReactive(input$go5,{
            switch(input$var6,
                   "Age.groups"=df_ca$Age.groups,
                   "Race" = df_ca$Race,
                   "Year"=df_ca$Year,
                   "Gender"=df_ca$Gender,
                   "Highest.level.of.force"=df_ca$Highest.level.of.force,
                   "Intended.use.of.force"=df_ca$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=df_ca$Foreknowledge.of.mental.illness)
        })
        output$cabar <- renderPlot({
            isolate({ggplot(df_ca,aes(x=u_data3(),fill=u_data3()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var6))+xlab(paste(input$var6))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })})
    })
    observeEvent(input$go6,{
        u_data4 <- eventReactive(input$go6,{
            switch(input$var7,
                   "Age.groups"=df_mo$Age.groups,
                   "Race" = df_mo$Race,
                   "Year"=df_mo$Year,
                   "Gender"=df_mo$Gender,
                   "Highest.level.of.force"=df_mo$Highest.level.of.force,
                   "Intended.use.of.force"=df_mo$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=df_mo$Foreknowledge.of.mental.illness)
        })
        output$mobar <- renderPlot({
            isolate({ggplot(df_mo,aes(x=u_data4(),fill=u_data4()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var7))+xlab(paste(input$var7))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })})
    })
    observeEvent(input$go7,{
        u_data5 <- eventReactive(input$go7,{
            switch(input$var8,
                   "Age.groups"=df_00$Age.groups,
                   "Race" = df_00$Race,
                   "State"=df_00$State,
                   "Gender"=df_00$Gender,
                   "Highest.level.of.force"=df_00$Highest.level.of.force,
                   "Intended.use.of.force"=df_00$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=df_00$Foreknowledge.of.mental.illness)
        })
        output$bar00 <- renderPlot({
            isolate({ggplot(df_00,aes(x=u_data5(),fill=u_data5()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var8))+xlab(paste(input$var8))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })})
    })
    observeEvent(input$go8,{
        u_data6 <- eventReactive(input$go8,{
            switch(input$var9,
                   "Age.groups"=df_01$Age.groups,
                   "Race" = df_01$Race,
                   "State"=df_01$State,
                   "Gender"=df_01$Gender,
                   "Highest.level.of.force"=df_01$Highest.level.of.force,
                   "Intended.use.of.force"=df_01$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=df_01$Foreknowledge.of.mental.illness)
        })
        output$bar01 <- renderPlot({
            isolate({ggplot(df_01,aes(x=u_data6(),fill=u_data6()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var9))+xlab(paste(input$var9))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })})
    })
    observeEvent(input$go9,{
        u_data7 <- eventReactive(input$go9,{
            switch(input$var10,
                   "Age.groups"=df_m$Age.groups,
                   "Race" = df_m$Race,
                   "State"=df_m$State,
                   "Year"=df_m$Year,
                   "Highest.level.of.force"=df_m$Highest.level.of.force,
                   "Intended.use.of.force"=df_m$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=df_m$Foreknowledge.of.mental.illness)
        })
        output$barm <- renderPlot({
            isolate({ggplot(df_m,aes(x=u_data7(),fill=u_data7()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var10))+xlab(paste(input$var10))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })})
    })
    observeEvent(input$go10,{
        u_data8 <- eventReactive(input$go10,{
            switch(input$var11,
                   "Age.groups"=df_f$Age.groups,
                   "Race" = df_f$Race,
                   "State"=df_f$State,
                   "Year"=df_f$Year,
                   "Highest.level.of.force"=df_f$Highest.level.of.force,
                   "Intended.use.of.force"=df_f$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=df_f$Foreknowledge.of.mental.illness)
        })
        output$barf <- renderPlot({
            isolate({ggplot(df_f,aes(x=u_data8(),fill=u_data8()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var11))+xlab(paste(input$var11))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })})
    })
    observeEvent(input$go11,{
        u_data10 <- eventReactive(input$go11,{
            switch(input$var12,
                   "Gender"=df_2$Gender,
                   "Race" = df_2$Race,
                   "State"=df_2$State,
                   "Year"=df_2$Year,
                   "Highest.level.of.force"=df_2$Highest.level.of.force,
                   "Intended.use.of.force"=df_2$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=df_2$Foreknowledge.of.mental.illness)
        })
        output$bar2 <- renderPlot({
            isolate({ggplot(df_2,aes(x=u_data10(),fill=u_data10()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var12))+xlab(paste(input$var12))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })})
    })
    observeEvent(input$go12,{
        u_data11 <- eventReactive(input$go12,{
            switch(input$var13,
                   "Gender"=df_3$Gender,
                   "Race" = df_3$Race,
                   "State"=df_3$State,
                   "Year"=df_3$Year,
                   "Highest.level.of.force"=df_3$Highest.level.of.force,
                   "Intended.use.of.force"=df_3$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=df_3$Foreknowledge.of.mental.illness)
        })
        output$bar3 <- renderPlot({
            isolate({ggplot(df_3,aes(x=u_data11(),fill=u_data11()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var13))+xlab(paste(input$var13))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })})
    })
    observeEvent(input$go13,{
        u_data12 <- eventReactive(input$go13,{
            switch(input$var14,
                   "Gender"=df_w$Gender,
                   "Age.groups" = df_w$Age.groups,
                   "State"=df_w$State,
                   "Year"=df_w$Year,
                   "Highest.level.of.force"=df_w$Highest.level.of.force,
                   "Intended.use.of.force"=df_w$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=df_w$Foreknowledge.of.mental.illness)
        })
        output$barw <- renderPlot({
            isolate({ggplot(df_w,aes(x=u_data12(),fill=u_data12()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var14))+xlab(paste(input$var14))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })})
    })
    observeEvent(input$go14,{
        u_data13 <- eventReactive(input$go14,{
            switch(input$var15,
                   "Gender"=df_b$Gender,
                   "Age.groups" = df_b$Age.groups,
                   "State"=df_b$State,
                   "Year"=df_b$Year,
                   "Highest.level.of.force"=df_b$Highest.level.of.force,
                   "Intended.use.of.force"=df_b$Intended.use.of.force,
                   "Foreknowledge.of.mental.illness"=df_b$Foreknowledge.of.mental.illness)
        })
        output$barb <- renderPlot({
            isolate({ggplot(df_b,aes(x=u_data13(),fill=u_data13()))+geom_bar(show.legend="FALSE")+coord_flip()+ggtitle(paste("Graph of",input$var15))+xlab(paste(input$var15))+scale_x_discrete(guide=guide_axis(n.dodge=1.5))+ylab("Frequency")+theme_light()+scale_fill_viridis_d(direction = -1)
            })})
    })
 
}
shinyApp(ui = ui, server = server)
