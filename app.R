library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(rstatix)
library(DT)
library(stringr)
library(RVAideMemoire)
#Data cleaning and importing

dat <- readxl::read_excel("DATA2x02_survey_2025_Responses.xlsx")


old_names = colnames(dat)
new_names <- c(
  "timestamp",
  "target_grade",
  "assignment_preference",
  "trimester_or_semester",
  "age",
  "tendency_yes_or_no",
  "pay_rent",
  "stall_choice",
  "weetbix_count",
  "weekly_food_spend",
  "living_arrangements",
  "weekly_alcohol",
  "believe_in_aliens",
  "height",
  "commute",
  "daily_anxiety_frequency",
  "weekly_study_hours",
  "work_status",
  "social_media",
  "gender",
  "average_daily_sleep",
  "usual_bedtime",
  "sleep_schedule",
  "sibling_count",
  "allergy_count",
  "diet_style",
  "random_number",
  "favourite_number",
  "favourite_letter",
  "drivers_license",
  "relationship_status",
  "daily_short_video_time",
  "computer_os",
  "steak_preference",
  "dominant_hand",
  "enrolled_unit",
  "weekly_exercise_hours",
  "weekly_paid_work_hours",
  "assignments_on_time",
  "used_r_before",
  "team_role_type",
  "university_year",
  "favourite_anime",
  "fluent_languages",
  "readable_languages",
  "country_of_birth",
  "wam",
  "shoe_size",
  "books_read_highschool",
  "daily_water_intake_l",
  "perceived_old_age",
  "study_music_preference"
)

colnames(dat) = new_names


# Trim white space & convert empty/blank strings to NA
dat <- dat %>% mutate(across(where(is.character), ~na_if(trimws(.), "")))

# Step 2: Simple cleanups
dat <- dat %>%
  mutate(
    wam = as.numeric(wam),
    wam = ifelse(!is.na(wam) & wam <= 20, NA, wam),
    wam_bins = cut(wam, breaks = c(0,50,65,75,85,100.1), labels = c('FL','PS','CR','DI','HD'), right = FALSE),
    target_grade = ifelse(tolower(as.character(target_grade)) == "fail", NA, target_grade),
    pay_rent = ifelse(pay_rent %in% c("Yes","No"), pay_rent, NA),
    weekly_paid_work_hours = as.numeric(weekly_paid_work_hours),
    weekly_paid_work_hours = ifelse(!is.na(weekly_paid_work_hours) & weekly_paid_work_hours > 60, NA, weekly_paid_work_hours),
    age = ifelse(age < 15 | age > 100, NA, age),
    average_daily_sleep = ifelse(average_daily_sleep < 2 | average_daily_sleep > 16, NA, average_daily_sleep),
    daily_anxiety_frequency = ifelse(daily_anxiety_frequency < 0 | daily_anxiety_frequency > 10, NA, daily_anxiety_frequency),
    daily_water_intake_l = ifelse(daily_water_intake_l < 0 | daily_water_intake_l > 10, NA, daily_water_intake_l),
    books_read_highschool = ifelse(books_read_highschool < 0, NA, books_read_highschool)
    )
  
dat <- dat %>%
  mutate(
    
    gender = case_when(
      str_to_lower(gender) %in% c("male", "m") ~ "Male",
      str_to_lower(gender) %in% c("female", "f") ~ "Female",
      TRUE ~ NA_character_
    ),
    
    drivers_license = case_when(
      str_to_lower(drivers_license) %in% c("yes", "y") ~ "Yes",
      str_to_lower(drivers_license) %in% c("no", "n") ~ "No",
      TRUE ~ NA_character_
    ),
    
    believe_in_aliens = case_when(
      str_to_lower(believe_in_aliens) %in% c("yes") ~ "Yes",
      str_to_lower(believe_in_aliens) %in% c("no") ~ "No",
      TRUE ~ NA_character_
    ),
    
    assignments_on_time = case_when(
      str_to_lower(assignments_on_time) %in% c("always","yes","y") ~ "Yes",
      str_to_lower(assignments_on_time) %in% c("no","sometimes","rarely") ~ "No",
      TRUE ~ NA_character_
    ),
    
    used_r_before = case_when(
      str_to_lower(used_r_before) %in% c("yes","y") ~ "Yes",
      str_to_lower(used_r_before) %in% c("no","n") ~ "No",
      TRUE ~ NA_character_
    ),
    
    # Categorical cleanups
    steak_preference = case_when(
      str_detect(str_to_lower(steak_preference), "rare") ~ "Rare",
      str_detect(str_to_lower(steak_preference), "medium") ~ "Medium",
      str_detect(str_to_lower(steak_preference), "well") ~ "Well-done",
      TRUE ~ NA_character_
    ),
    
    dominant_hand = case_when(
      str_to_lower(dominant_hand) %in% c("left","l") ~ "Left",
      str_to_lower(dominant_hand) %in% c("right","r") ~ "Right",
      TRUE ~ NA_character_
    ),
    
    # Relationship status -> collapse into "Single" / "In relationship"
    relationship_status = case_when(
      str_detect(str_to_lower(relationship_status), "single") ~ "Single",
      str_detect(str_to_lower(relationship_status), "dating|relationship|partner|married") ~ "In Relationship",
      TRUE ~ NA_character_
    ),
    
    # Team role type: collapse into a few categories (Leader / Follower / Other)
    team_role_type = case_when(
      str_detect(str_to_lower(team_role_type), "leader|manager") ~ "Leader",
      str_detect(str_to_lower(team_role_type), "follower|member") ~ "Follower",
      TRUE ~ "Other"
    ),
    
    # Living arrangements: collapse
    living_arrangements = case_when(
      str_detect(str_to_lower(living_arrangements), "parents|home") ~ "With Parents",
      str_detect(str_to_lower(living_arrangements), "alone|independent|flat") ~ "Alone",
      str_detect(str_to_lower(living_arrangements), "share|roommate|friends") ~ "Shared",
      TRUE ~ NA_character_
    )
  )

categorical_vars <- c(
  "gender",
  "pay_rent",
  "drivers_license",
  "believe_in_aliens",
  "assignments_on_time",
  "used_r_before",
  "dominant_hand",
  "relationship_status",
  "team_role_type"
)

numeric_vars <- c(
  "age",
  'usual_bedtime',
  "wam",
  "weekly_paid_work_hours",
  "weekly_study_hours",
  "average_daily_sleep",
  "daily_anxiety_frequency",
  "sibling_count",
  "books_read_highschool",
  "daily_water_intake_l",
  "perceived_old_age"
)

for(col in numeric_vars){
  dat[[col]] <- suppressWarnings(as.numeric(as.character(dat[[col]])))
}

# Force categorical columns
for(col in categorical_vars){
  dat[[col]] <- as.factor(dat[[col]])
}


# __ UI __

ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "dataset_tab", icon = icon("table")),
      menuItem("Plots", tabName = "plots_tab", icon = icon("chart-bar")),
      menuItem("Tests", tabName = "tests_tab", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dataset_tab',
              fluidRow(
                box(title = "dataset", width = 12, DT::dataTableOutput("data_table"))
              )),
      
      tabItem(tabName = "plots_tab",
              fluidRow(
                box(
                  title = 'Plot Options', width = 4, status = 'primary', solidHeader = T,
                  selectInput('plot_type', 'Plot Type', 
                              choices = c("Histogram", "Scatter", "Box", "Column")),
                  uiOutput('plot_vars_ui')
                ),
                
                box(title = 'Plot Output', width = 8, status = 'primary', solidHeader = T,
                    plotOutput('plot_output', height = '500px'))
              )
              ),
      
      tabItem(tabName = 'tests_tab',
              fluidRow(
                box(title = 'Test Options', width = 4, status = 'warning', solidHeader = T,
                    selectInput('test_type', 'Choose Test', choices = c("t-test", "Chi-square")),
                    
                    conditionalPanel(
                      condition = "input.test_type == 't-test'",
                      selectInput("t_var", "Select Numerical Variable", choices = numeric_vars),
                      checkboxInput("var_equal", "Assume equal variances?", value = FALSE),
                      uiOutput('t_group_ui'),
                      numericInput('alpha', 'Significance Level', value = 0.05, min = 0.001, max = 0.2, step = 0.01),
                      radioButtons('alternative', 'Alternative Hypothesis', choices = c("Two-sided" = "two.sided",
                                                                                        "Greater" = "greater",
                                                                                        "Less" = "less")),
                      radioButtons('t_method', 'Test Method', choices = c("Normal t-test" = "t",
                                                                          "Welch" = "welch",
                                                                          "Permutation" = "perm"))
                      ),
                  conditionalPanel(
                    condition = "input.test_type == 'Chi-square'",
                    selectInput("chi_var1", "Categorical Variable 1", choices = categorical_vars),
                    selectInput("chi_var2", "Categorical Variable 2", choices = categorical_vars),
                    numericInput("alpha_chi", "Significance Level", value = 0.05, min = 0.001, max = 0.2, step = 0.01)
                  )
              ),
              
              box(title = 'Test Results', width = 8, status = 'warning', solidHeader = T,
                  uiOutput('test_results_ui'), plotOutput('test_plot', height = '400px'))
              )
        
))))

    
server <- function(input, output, session) {
      # __ DATASET __
  output$data_table <- DT::renderDataTable({
        DT::datatable(dat, options = list(pageLength = 10))
      })
      
  # __ PLOTS __
  output$plot_vars_ui <- renderUI({
    req(input$plot_type)
    if (input$plot_type == "Histogram") {
      selectInput("hist_var", "Numerical Variable", choices = numeric_vars)
    } else if (input$plot_type == "Scatter") {
      tagList(
        selectInput("x_var", "X Variable", choices = numeric_vars),
        selectInput("y_var", "Y Variable", choices = numeric_vars),
        selectInput("color_var", "Color (Optional)", choices = c("None", categorical_vars)),
        selectInput("size_var", "Size (Optional)", choices = c("None", numeric_vars))
      )
    } else if (input$plot_type == "Box") {
      tagList(
        selectInput("box_num", "Numerical Variable", choices = numeric_vars),
        selectInput("box_cat", "Categorical Variable (Optional)", choices = c("None", categorical_vars))
      )
    } else if (input$plot_type == "Column") {
      tagList(
        selectInput("col_num", "Numerical Variable", choices = numeric_vars),
        selectInput("col_cat", "Categorical Variable", choices = categorical_vars)
      )
    }
  })
  
  output$plot_output <- renderPlot({
    req(input$plot_type)
    if (input$plot_type == "Histogram") {
      ggplot(dat, aes_string(x = input$hist_var)) +
        geom_bar(fill = "blue") + theme_minimal()
    } 
    else if (input$plot_type == "Scatter") {
      p <- ggplot(dat, aes_string(x = input$x_var, y = input$y_var))
      if (input$color_var != "None") p <- p + aes_string(color = input$color_var)
      if (input$size_var != "None") p <- p + aes_string(size = input$size_var)
      p + geom_point() + theme_minimal()
    } 
    else if (input$plot_type == "Box") {
      if (input$box_cat != "None") {
        ggplot(dat, aes_string(x = input$box_cat, y = input$box_num)) + geom_boxplot(fill="lightgreen") + theme_minimal()
      } else {
        ggplot(dat, aes_string(y = input$box_num)) + geom_boxplot(fill="lightgreen") + theme_minimal()
      }
    } 
    else if (input$plot_type == "Column") {
      ggplot(dat, aes_string(x = input$col_cat, y = input$col_num)) + geom_col(fill="salmon") + theme_minimal()
    }
  })
  
  # __ Tests __
  
  output$t_group_ui <- renderUI({
    req(input$t_var)
    selectInput('group_var', 'Select Categorical Grouping Variable', choices = categorical_vars)
  })
  
  output$test_results_ui <- renderUI({
    
    if (input$test_type == "t-test") {
      
      df <- dat %>% 
        select(all_of(c(input$t_var, input$group_var))) %>% 
        filter(!is.na(.data[[input$t_var]]), !is.na(.data[[input$group_var]]))
      
      df[[input$t_var]] <- as.numeric(df[[input$t_var]])
      df[[input$group_var]] <- as.factor(df[[input$group_var]])
      
      group_levels <- levels(df[[input$group_var]])
      if(length(group_levels) != 2) return(tags$p("Grouping variable must have exactly 2 levels"))
      
      # Normality + variance messages
      shapiro_p <- shapiro.test(df[[input$t_var]])$p.value
      normality_msg <- if (shapiro_p > 0.05) {
        "Normality assumption OK (Shapiro-Wilk p > 0.05)"
      } else {
        "Normality assumption may be violated (Shapiro-Wilk p < 0.05)"
      }
      
      var_msg <- ""
      if (input$var_equal) {
        lev_p <- car::leveneTest(df[[input$t_var]] ~ df[[input$group_var]])$`Pr(>F)`[1]
        var_msg <- if (lev_p > 0.05) {
          "Equal variances assumption OK (Levene's test p > 0.05)"
        } else {
          "Equal variances assumption may be violated (Levene's test p < 0.05)"
        }
      }
      
      group1 <- df[[input$t_var]][df[[input$group_var]] == group_levels[1]]
      group2 <- df[[input$t_var]][df[[input$group_var]] == group_levels[2]]
      
      if(input$t_method == "perm") {
        
        t0 <- t.test(group1, group2, alternative = input$alternative)$statistic
        
        set.seed(42)
        B <- 3000
        t_null <- numeric(B)
        combined <- c(group1, group2)
        n1 <- length(group1)
        
        for(i in 1:B) {
          permuted <- sample(combined)
          t_null[i] <- t.test(permuted[1:n1], permuted[(n1+1):length(permuted)],
                              alternative = input$alternative)$statistic
        }
        
        if(input$alternative == "two.sided") p_val <- mean(abs(t_null) >= abs(t0))
        if(input$alternative == "greater") p_val <- mean(t_null >= t0)
        if(input$alternative == "less") p_val <- mean(t_null <= t0)
        
        t_stat <- t0
        df_val <- NA
        
      } else {
        t_res <- t.test(df[[input$t_var]] ~ df[[input$group_var]],
                        alternative = input$alternative,
                        var.equal = ifelse(input$t_method == "t", TRUE, FALSE))
        t_stat <- unname(t_res$statistic)
        df_val <- unname(t_res$parameter)
        p_val <- t_res$p.value
      }
      
      conclusion <- ifelse(p_val < input$alpha, 
                           paste("p <", input$alpha, "→ Reject H0"), 
                           paste("p >=", input$alpha, "→ Fail to reject H0"))
      tagList(
        h3("T-test Results"),
        tags$b("Hypotheses:"),
        tags$ul(
          tags$li(paste("H0: Mean", input$t_var, "is equal across groups of", input$group_var)),
          tags$li(paste("HA:", switch(input$alternative, 
                                      "two.sided" = "Means differ",
                                      "greater"   = paste(group_levels[1], ">", group_levels[2]),
                                      "less"      = paste(group_levels[1], "<", group_levels[2]))))
        ),
        tags$b("Assumptions:"),
        tags$ul(
          tags$li(normality_msg),
          if (var_msg != "") tags$li(var_msg)
        ),
        tags$b("Results:"),
        tags$ul(
          tags$li(paste("Test statistic =", round(t_stat, 3))),
          if (!is.na(df_val)) tags$li(paste("df =", round(df_val, 2))),
          tags$li(paste("p-value =", signif(p_val, 3))),
          tags$li(paste("Conclusion:", conclusion))
        )
      )
      
    } else if (input$test_type == "Chi-square") {
      
      df <- dat %>%
        select(all_of(c(input$chi_var1, input$chi_var2))) %>%
        na.omit()
      
      df[[input$chi_var1]] <- as.factor(df[[input$chi_var1]])
      df[[input$chi_var2]] <- as.factor(df[[input$chi_var2]])
      
      tab <- table(df[[input$chi_var1]], df[[input$chi_var2]])
      
      chi_res <- chisq.test(tab, correct = FALSE)
      chi_stat <- unname(chi_res$statistic)
      chi_df <- unname(chi_res$parameter)
      chi_p <- chi_res$p.value
      expected <- chi_res$expected
      
      conclusion <- ifelse(chi_p < input$alpha, 
                           paste("p <", input$alpha, "→ Reject H0"), 
                           paste("p >=", input$alpha, "→ Fail to reject H0"))
      
      min_expected <- min(expected)
      assumption_msg <- if(min_expected >= 5) {
        "All expected counts >= 5, Chi-square assumption OK"
      } else {
        paste("Some expected counts < 5 (min =", round(min_expected,2), 
              "), Chi-square assumption may be violated")
      }
      
      tagList(
        h3("Chi-square Test Results"),
        tags$b("Hypotheses:"),
        tags$ul(
          tags$li(paste("H0: Variables", input$chi_var1, "and", input$chi_var2, "are independent")),
          tags$li(paste("HA: Variables", input$chi_var1, "and", input$chi_var2, "are associated"))
        ),
        tags$b("Assumptions:"),
        tags$ul(
          tags$li(assumption_msg)
        ),
        tags$b("Results:"),
        tags$ul(
          tags$li(paste("Chi-square statistic =", round(chi_stat,3))),
          tags$li(paste("df =", chi_df)),
          tags$li(paste("p-value =", signif(chi_p, 3))),
          tags$li(paste("Conclusion:", conclusion))
        )
      )
      
    }
    
  })
  
  
  
  output$test_plot <- renderPlot({
    req(input$test_type)
    if (input$test_type == "Chi-square") {
      req(input$chi_var1, input$chi_var2)
      df <- dat %>% select(all_of(c(input$chi_var1, input$chi_var2))) %>% na.omit()
      tab <- table(df[[input$chi_var1]], df[[input$chi_var2]])
      # bar plot of counts
      ggplot(df, aes_string(x = input$chi_var1, fill = input$chi_var2)) +
        geom_bar(position = "dodge") +
        theme_minimal() +
        labs(title = paste("Counts of", input$chi_var1, "by", input$chi_var2))
    } else if (input$test_type == "t-test") {
      req(input$t_var, input$group_var)
      df <- dat %>% select(all_of(c(input$t_var, input$group_var))) %>% na.omit()
      group_levels <- unique(df[[input$group_var]])
      if (length(group_levels) != 2) {
        plot.new()
        text(0.5, 0.5, "Boxplot not available: grouping variable must have exactly 2 levels")
      } else {
        ggplot(df, aes_string(x = input$group_var, y = input$t_var, fill = input$group_var)) +
          geom_boxplot(alpha = 0.7) +
          geom_jitter(width = 0.15, alpha = 0.4) +
          theme_minimal() +
          labs(title = paste("Boxplot of", input$t_var, "by", input$group_var))
      }
    }
  })
  
}
shinyApp(ui, server)
    