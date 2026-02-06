# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Boston Marathon 2023 Analysis App


library(shiny)
library(tidyverse)
library(ggplot2)
library(bslib)
library(scales)
library(DT)
library(thematic)
library(shinyWidgets)
library(plotly)
library(sf)
library(caret)

thematic_shiny(font = "auto")

# Load the data
file_path <- "../data_raw/boston_marathon_2023.csv"
marathon_data <- read.csv(file_path)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "materia"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "gender", "Select Gender",
        choices = c("All", "M", "W"),
        selected = "All",
        multiple = FALSE
      ),
      pickerInput(
        "age_group", "Select Age Group",
        choices = unique(marathon_data$age_group),
        multiple = TRUE,
        selected = unique(marathon_data$age_group),
        options = pickerOptions(
          liveSearch = TRUE,
          actionsBox = TRUE
        )
      ),
      sliderInput(
        "time", "Finish Time Range (Minutes)",
        min = min(marathon_data$finish_net_minutes, na.rm = TRUE),
        max = max(marathon_data$finish_net_minutes, na.rm = TRUE),
        value = c(min(marathon_data$finish_net_minutes, na.rm = TRUE),
                  max(marathon_data$finish_net_minutes, na.rm = TRUE))
      ),
      downloadButton('downFile', label = "Download Filtered Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          h3("Boston Marathon 2023 Data"),
          dataTableOutput("summary_table")
        ),
        tabPanel(
          "Visualizations",
          plotlyOutput("barplot_counts"),
          plotlyOutput("scatterplot_performance")
        ),
        tabPanel(
          "Statistical Analysis",
          h3("Statistical Analysis"),
          p("Select a statistical analysis to display below:"),
          tabsetPanel(
            tabPanel(
              "K-Means Clustering",
              plotlyOutput("kmeans_plot")
            ),
            tabPanel(
              "Principal Component Analysis (PCA)",
              plotlyOutput("pca_plot")
            )
          )
        ),
          tabPanel(
            "Literature Review",
            h3("Literature Review"),
            p("Oftentimes lower Bib numbers are given to the fastest runners, and many marathons abide by this (Francis, 2019). It would be interesting to look at the Bibs and their times. Therefore, the faster you are, the lower the bib number. More men than women run in races (Rock, 2024). For example, in the United States, roughly 55 percent of men complete marathons vs 45 percent of women. Oftentimes, male runners complete the races faster than female athletes. For example, in the Chicago and New York City Marathons, men ran on average 17.2 percent faster in the Chicago marathon and 18.8 percent faster in the NYC Marathon (Nesburg et al., 2023)."),
            p("The analysis of marathon performance has drawn considerable attention in sports science, particularly in exploring how factors like age, gender, and pacing strategies affect race outcomes. This literature review synthesizes findings from key studies to provide context for analyzing the 2023 Boston Marathon data."),
            p("Hunter et al. (2011) examined the **age of peak performance** among elite male and female marathon runners across various marathons. Their findings indicated that both sexes generally reach peak performance at similar ages, though with slight variations. This study provides foundational insights into how age influences marathon performance, supporting the analysis of age-based trends in the Boston Marathon dataset."),
            p("Further investigating **pacing strategies** by age and gender, Nikolaidis and Knechtle (2018) studied marathon pacing differences and found that women tend to pace more consistently than men, while men often exhibit greater speed fluctuations, especially in later race segments. These pacing tendencies are linked to physiological factors such as muscle glycogen depletion and neuromuscular fatigue, which impact men more significantly. This research provides a basis for understanding gender-based pacing strategies within the Boston Marathon data, particularly how they might influence finish times across demographic groups."),
            p("Cuk, Nikolaidis, and Knechtle (2020) explored **sex differences in pacing** across half-marathon and marathon distances, comparing speed variations between men and women. Their findings suggest that women maintain more consistent pacing across both race types, while men exhibit greater speed changes, especially in the marathon. These differences may be due to physiological and psychological factors, such as men’s greater tendency for 'risky' pacing early in races. This study offers valuable insights for understanding how gender and distance influence pacing, applicable to the Boston Marathon analysis when comparing marathon completion times across demographics."),
            p("In summary, these studies illustrate the complex interplay of age, gender, and pacing strategies in marathon performance. By incorporating these findings, the Boston Marathon analysis can provide a nuanced perspective on how demographic factors shape race outcomes."),
            p("### References"),
            p("1. Francis, A. (2019, March 20). Boston Marathon bib numbers, wave assignments released - Canadian Running Magazine. Canadian Running Magazine."),
            p("2. Hunter, S. K., Stevens, A. A., Magennis, K., Skelton, K. W., & Fauth, M. (2011). 'Is there a sex difference in the age of elite marathon runners?' *Medicine & Science in Sports & Exercise*."),
            p("3. Nikolaidis, P. T., & Knechtle, B. (2018). 'Age and gender differences in pacing during marathon racing.' *Journal of Strength and Conditioning Research*."),
            p("4. Nesburg, R. A., Mason, A. P., Fitzsimmons, B., & Hunter, S. K. (2023). Sex Differences in Marathon Running: Physiology and Participation. Exercise, Sport, and Movement, 1(3), e00010. https://doi.org/10.1249/ESM.0000000000000010"),
            p("5. Rock, B. (2024, August 18). Analyzing the Changing Demographics of Marathon Finishers from 2010 to 2019. Medium; Runner’s Life."),
            p("6. Cuk, I., Nikolaidis, P. T., & Knechtle, B. (2020). 'Sex differences in pacing during half-marathon and marathon races.' *Research in Sports Medicine*.")
          ),
          tabPanel(
            "Ethics Review",
            h3("Ethics Review"),
            p("Throughout the project, we will ensure data privacy and adhere to ethical guidelines for data analysis and visualization, such as the American Statistical Association's Code of Conduct. This includes handling any identifiable information with care and maintaining data integrity Link: https://www.amstat.org/meetings/code-of-conduct.")
          ),
          tabPanel(
            "Questions of Interest",
            h3("Questions of Interest"),
            p("As Data scientist we can’t simply waste away the findings of our projects. Data is used to help answer real world problems and this project is no exception to that notion. For our work, we play the role of two different groups of people interested in the marathon. The sports analyst (think of organizations such as ESPN or NCAA) and the marathon organizers. Sports will often come to the Boston marathon in order to gauge the next generation of runners or possibly even recruit them. Joan Benoit (Maine resident) had won the Boston marathon in 1979 and 1983. She then competed in the 84’ olympics and ended up winning a gold medal. Another more recent example, Peres Jepchirchir who was originally from Kenya participated in and won the 2022 Boston marathon. She then went to the olympics and won a gold medal in the women’s marathon. We see how important this event truly is in gauging out potential recruits and so as a sports analyst there are a few variables that are important to us. Performance focused variables such as overall placement, finish time in min, sec, and hrs, and even half time for pacing observation are all important variables to keep in mind. But one of the most surprising aspects of this project was how bib number was actually one of the chief variables for us to look at. An immense amount of data can be extracted when taking this variable and comparing it with the others. The reason the bib number is so important is because of how the BAA (Boston Athletic Association) prioritizes bib number."), 
            p("When you want to sign up for the Boston Marathon, you need to first register for the qualifying runs. When you participate in these qualifiers you must receive a qualifying time in order to be eligible for the race:"),
            tags$div(
              style = "overflow-x:auto;",
              tags$table(
                style = "width:100%; text-align:left; border-collapse:collapse; margin-top:20px;",
                tags$thead(
                  tags$tr(
                    tags$th("Age Group", style = "border-bottom:1px solid black; padding:5px;"),
                    tags$th("Men", style = "border-bottom:1px solid black; padding:5px;"),
                    tags$th("Women", style = "border-bottom:1px solid black; padding:5px;"),
                    tags$th("Non-Binary", style = "border-bottom:1px solid black; padding:5px;")
                  )
                ),
                tags$tbody(
                  tags$tr(tags$td("18-34"), tags$td("3hrs 00min 00sec"), tags$td("3hrs 30min 00sec"), tags$td("3hrs 30min 00sec")),
                  tags$tr(tags$td("35-39"), tags$td("3hrs 05min 00sec"), tags$td("3hrs 35min 00sec"), tags$td("3hrs 35min 00sec")),
                  tags$tr(tags$td("40-44"), tags$td("3hrs 10min 00sec"), tags$td("3hrs 40min 00sec"), tags$td("3hrs 40min 00sec")),
                  tags$tr(tags$td("45-49"), tags$td("3hrs 20min 00sec"), tags$td("3hrs 50min 00sec"), tags$td("3hrs 50min 00sec")),
                  tags$tr(tags$td("50-54"), tags$td("3hrs 25min 00sec"), tags$td("3hrs 55min 00sec"), tags$td("3hrs 55min 00sec")),
                  tags$tr(tags$td("55-59"), tags$td("3hrs 35min 00sec"), tags$td("4hrs 05min 00sec"), tags$td("4hrs 05min 00sec")),
                  tags$tr(tags$td("60-64"), tags$td("3hrs 50min 00sec"), tags$td("4hrs 20min 00sec"), tags$td("4hrs 20min 00sec")),
                  tags$tr(tags$td("65-69"), tags$td("4hrs 05min 00sec"), tags$td("4hrs 35min 00sec"), tags$td("4hrs 35min 00sec")),
                  tags$tr(tags$td("70-74"), tags$td("4hrs 20min 00sec"), tags$td("4hrs 50min 00sec"), tags$td("4hrs 50min 00sec")),
                  tags$tr(tags$td("75-79"), tags$td("4hrs 35min 00sec"), tags$td("5hrs 05min 00sec"), tags$td("5hrs 05min 00sec")),
                  tags$tr(tags$td("80+"), tags$td("4hrs 50min 00sec"), tags$td("5hrs 20min 00sec"), tags$td("5hrs 20min 00sec"))
                )
              )
            ),
            p("This is also done to ensure there isn’t any congestion in the race since there are so many runners. The specifics of who gets placed into each wave are similar as well:"),
            p("Charity runners are an exception to this as they only need to submit some sort of timed running but they do not need to participate in the actual qualifying races. Once all the times are gathered, they distribute the bib numbers by giving the most elite runners with the best times from the qualifiers the lowest bib number. The runners who did qualify but didn’t have nearly the best times are given high bib numbers. Then these runners are divided into 4 different waves with each wave starting 25 min ahead of each other:"),
            tags$div(
              style = "overflow-x:auto;",
              tags$table(
                style = "width:100%; text-align:left; border-collapse:collapse; margin-top:20px;",
                tags$thead(
                  tags$tr(
                    tags$th("Wave", style = "border-bottom:1px solid black; padding:5px;"),
                    tags$th("Start Time", style = "border-bottom:1px solid black; padding:5px;")
                  )
                ),
                tags$tbody(
                  tags$tr(tags$td("Elite Wave (Men)"), tags$td("9:30 a.m.")),
                  tags$tr(tags$td("Elite Wave (Women)"), tags$td("9:50 a.m.")),
                  tags$tr(tags$td("Wave 1"), tags$td("10:00 a.m.")),
                  tags$tr(tags$td("Wave 2"), tags$td("10:25 a.m.")),
                  tags$tr(tags$td("Wave 3"), tags$td("10:50 a.m.")),
                  tags$tr(tags$td("Wave 4"), tags$td("11:15 a.m."))
                )
              )
            ),
            p("This is also done to ensure there isn’t any congestion in the race since there are so many runners. The specifics of who gets placed into each wave are similar as well:"),
            tags$ul(
              tags$li(tags$b("Elite Wave:"), " Bib numbers 1-100 are assigned to the professional runners"),
              tags$li(tags$b("Wave 1:"), " Bib numbers 101 through 7,700, with qualifying times faster than 3:07:27"),
              tags$li(tags$b("Wave 2:"), " Bib numbers 8,000 through 15,600, with qualifying times between 3:07:27 and 3:27:17"),
              tags$li(tags$b("Wave 3:"), " Bib numbers 16,000 through 23,600, with qualifying times between 3:27:17 and 3:56:54"),
              tags$li(tags$b("Wave 4:"), " Bib numbers 24,000 through 32,500, with qualifying times greater than 3:56:54")
            ),
            p("Now, with all this being said, how do we use this knowledge to derive some conclusions on our data and answer questions:"),
            p("We built a linear regression model comparing finish times (in min) and bib numbers:
There are a couple of things to note here: The p-value is <2.2e-16 which tells us that the bib number and finish time have a statistically significant relationship here. R^2 is 0.4774 which means almost half the variance is explained by the bib number. F-statistic is 7524 with the earlier p-value being < 2e-16 means this model is significant"),
            p("The coefficient being positive for bib numbers does support the idea that higher bib numbers lead to slower finish times however, there seems to be a bit more variability then we would like since we want a strong conclusion here. We need a stronger model."),
            p("Creating a regression model that compares bib numbers and overall placement we see the bib_number coefficient at 0.6904 which tells us for every single increase in the bib number, the place you will finish in the race increases by 0.6904. Again looking at the p-value tells us that this relationship is significant with the R^2 value accounting for over 60% of the variance in overall placement. Here we see a much stronger model here that we can base a conclusion off of which does make sense when looking at the variables. While finish time does tell us the actual performance of the runner with their time, there is a significant amount of variability that could go into it. (Weather conditions, morning routine, or even fatigue). Overall placement however is not effected by this so it build a stronger model."),
            p("Using these models and graphs we can comfortably say that as a sports analysts, I would want to use the models and graphs that highlight bib numbers as a mechanism to potentially recruit and observe the racers."),
            p("Now about the marathon organizers. Their goals are a bit different compared to the sports analyzers. They would want to focus on safety, fair competition, and reach. Variables such as age, gender, half-times, and bib-number are all important here:"),
            p("Looking at the regression model on age and finish times we see:"),
            tags$ul(
              tags$li("Age group 40-44 take 4.73 more minutes to finish than the youngest group"),
              tags$li("Age group 45-49 take 11.03 more minutes to finish"),
              tags$li("Age group 50-54 take 21.36 more minutes to finish"),
              tags$li("Age group 55-59 take 28.97 more minutes to finish"),
              tags$li("Age group 60-64 take 38.7 more minutes to finish"),
              tags$li("Age group 65-69 take 52.14 more minutes to finish"),
              tags$li("Age group 70-74 take 70.8 more minutes to finish"),
              tags$li("Age group 75-79 take 93.23 more minutes to finish"),
              tags$li("Age group 80+ take 91.71 more minutes to finish")
            ),
            p("Here we see older runners taking up to an extra hour and a half to finish the race compared to younger runners. As a marathon organizers it would be in the best interest to update the aid stations with refreshments and even have the runners that finished stay back in support for the older runners."),
            p("Creating a table for the number of male runners compared to female runners there is not a huge gap in the numbers, but there are more male runners than female in all age range categories. This could tell us that we need to advertise more to female runners and hopefully equate these numbers."),
            p("Bib numbers being so important to sports analysts, it's no question it will also provide great information for the sports management team. Firstly, they provide a way to quickly identify racers in case of an emergency. They also help again to create the waves and qualifying times for next year's marathon. As stated before when looking at finish times and bib numbers it seems that there is a good amount of variability here so maybe it may be important to investigate aspects such as the weather or terrain."),
            p("The last variables we interpreted as marathon organizers are the variables that encompass the split times of the runners. Split times are very important as they serve as the metric to decide where to put aid stations, water stops, rest areas, etc:"),
            p("The average halftime for each age group comes out as:"),
            tags$table(
              tags$thead(
                tags$tr(
                  tags$th("Age Group"),
                  tags$th("Half-Times")
                )
              ),
              tags$tbody(
                tags$tr(tags$td("18-39"), tags$td("1:41")),
                tags$tr(tags$td("40-44"), tags$td("1:43")),
                tags$tr(tags$td("45-49"), tags$td("1:46")),
                tags$tr(tags$td("50-54"), tags$td("1:50")),
                tags$tr(tags$td("55-59"), tags$td("1:53")),
                tags$tr(tags$td("60-64"), tags$td("1:57")),
                tags$tr(tags$td("65-69"), tags$td("2:20")),
                tags$tr(tags$td("70-74"), tags$td("2:10")),
                tags$tr(tags$td("75-79"), tags$td("2:19")),
                tags$tr(tags$td("80+"), tags$td("2:20"))
              )
            ),
            p("We see that in the ranges: 1:40 to 2:20 are where the half times are so aid station placement should vary here. This is because older runners may need aid more immediately and frequently while younger runners may need it a little towards the end."),
            p("These variables will no doubt help with making the marathon as smooth and inclusive as possible.")
          
        )
      )
    )
  )
)



server <- function(input, output, session) {
  marathon_data <- reactive({
    df <- read.csv("../data_raw/boston_marathon_2023.csv")
    return(df)
  })
  
  filtered_data <- reactive({
    df <- marathon_data()
    if("finish_net_minutes" %in% names(df)) {
      df <- df %>%
        filter(
          age_group %in% input$age_group,
          finish_net_minutes >= input$time[1], finish_net_minutes <= input$time[2]
        )
    } else {
      stop("Column 'finish_net_minutes' not found.")
    }
    return(df)
  })
  
  clean_marathon_data <- reactive({
    marathon_data() %>%
      filter(!is.na(as.numeric(as.character(bib_number)))) %>%
      mutate(bib_number_numeric = as.numeric(as.character(bib_number)))
  })
  
  observe({
    df <- marathon_data()
    gender_data <- df
    if (input$gender != "All") {
      gender_data <- gender_data %>% filter(gender == input$gender)
    }
    updatePickerInput(
      session,
      "age_group",
      choices = unique(gender_data$age_group),
      selected = unique(gender_data$age_group)
    )
  })
  
  output$summary_table <- renderDataTable({
    filtered_data() %>%
      group_by(age_group, gender) %>%
      summarise(
        Total_Runners = n(),
        Avg_Finish_Time = mean(finish_net_minutes, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$barplot_counts <- renderPlotly({
    plot <- ggplot(filtered_data(), aes(x = age_group, fill = gender)) +
      geom_bar() +
      labs(
        title = "Number of Runners by Age Group and Gender",
        x = "Age Group",
        y = "Count"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(plot)
  })
  
  output$scatterplot_performance <- renderPlotly({
    plot <- ggplot(filtered_data(), aes(x = age_group, y = finish_net_minutes, color = gender)) +
      geom_point(alpha = 0.5) +
      labs(
        title = "Performance by Age Group and Gender",
        x = "Age Group",
        y = "Finish Time (Minutes)"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot)
  })
  
  output$kmeans_plot <- renderPlotly({
    set.seed(123)
    kmeans_model <- kmeans(clean_marathon_data()[, c("finish_net_minutes", "bib_number_numeric")], centers = 3, nstart = 25)
    cluster_data <- clean_marathon_data() %>% mutate(cluster = as.factor(kmeans_model$cluster))
    
    plot <- ggplot(cluster_data, aes(x = bib_number_numeric, y = finish_net_minutes, color = cluster)) +
      geom_point(alpha = 0.6) +
      labs(
        title = "K-Means Clustering of Marathon Finishers",
        x = "Bib Number",
        y = "Finish Time (Minutes)",
        color = "Cluster"
      ) +
      theme_minimal()
    
    ggplotly(plot)
  })
  
  output$pca_plot <- renderPlotly({
    pca_data <- clean_marathon_data()[, c("finish_net_minutes", "place_overall", "bib_number_numeric")]
    pca_result <- prcomp(pca_data, scale. = TRUE)
    explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
    
    explained_variance_df <- data.frame(
      Principal_Component = paste0("PC", 1:length(explained_variance)),
      Variance_Explained = explained_variance
    )
    
    plot <- ggplot(explained_variance_df, aes(x = Principal_Component, y = Variance_Explained)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(
        title = "Explained Variance by Principal Component",
        x = "Principal Component",
        y = "Proportion of Variance Explained"
      ) +
      theme_minimal()
    
    ggplotly(plot)
  })
  
  output$statistical_analysis <- renderUI({
    tabsetPanel(
      tabPanel(
        "K-Means Clustering",
        plotlyOutput("kmeans_plot")
      ),
      tabPanel(
        "Principal Component Analysis (PCA)",
        plotlyOutput("pca_plot")
      )
    )
  })
  
  output$downFile <- downloadHandler(
    filename = "filtered_marathon_data.csv",
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
