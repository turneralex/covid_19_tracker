library(shiny)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(rjson)

covid_19_raw <- fromJSON(file = "https://interactive.guim.co.uk/docsdata/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE.json")

covid_19_raw <- covid_19_raw %>% 
    pluck("sheets")

covid_19_df <- covid_19_raw %>% 
    pluck("updates") %>% 
    map_dfr(
        ~ as.data.frame(
            .x, 
            stringsAsFactors = F
        )
    ) %>% 
    as_tibble() %>% 
    select(State, Date, Cumulative.case.count, Cumulative.deaths, Recovered..cumulative.)

colnames(covid_19_df) <- colnames(covid_19_df) %>% 
    tolower() %>% 
    str_replace_all("[.]", "_")

colnames(covid_19_df)[colnames(covid_19_df) == "cumulative_case_count"] <- "cumulative_cases"

colnames(covid_19_df)[colnames(covid_19_df) == "recovered__cumulative_"] <- "cumulative_recoveries"

covid_19_df <- covid_19_df %>% 
    mutate(
        state = state %>% factor(),
        date = date %>% dmy(),
        cumulative_cases = cumulative_cases %>% as.integer(),
        cumulative_deaths = cumulative_deaths %>% as.integer(),
        cumulative_recoveries = cumulative_recoveries %>% as.integer()
    ) %>% 
    complete(state, date) %>% 
    arrange(state, date) %>% 
    group_by(state) %>% 
    fill(
        cumulative_cases, 
        cumulative_deaths,
        cumulative_recoveries
    ) %>% 
    group_by(state, date) %>% 
    summarise(
        cumulative_cases = cumulative_cases %>% max(),
        cumulative_deaths = cumulative_deaths %>% max(),
        cumulative_recoveries = cumulative_recoveries %>% max()
    ) %>% 
    mutate(
        cumulative_cases = if_else(
            cumulative_cases %>% is.na(),
            as.integer(0),
            cumulative_cases
        ),
        cumulative_deaths = if_else(
            cumulative_deaths %>% is.na(),
            as.integer(0),
            cumulative_deaths
        ),
        cumulative_recoveries = if_else(
            cumulative_recoveries %>% is.na(),
            as.integer(0),
            cumulative_recoveries
        ),
        # fix for erroneous SA data where on 17/04/2020 cumulative_recoveries == cumulative_cases
        # despite cumulative_deaths being > 0
        # needs to be done twice as the above fill() call duplicates the issue across 2 days
        cumulative_recoveries = if_else(
            (cumulative_deaths > 0) & (cumulative_recoveries >= cumulative_cases),
            lag(cumulative_recoveries),
            cumulative_recoveries
        ),
        cumulative_recoveries = if_else(
            (cumulative_deaths > 0) & (cumulative_recoveries >= cumulative_cases),
            lag(cumulative_recoveries),
            cumulative_recoveries
        ),
        cumulative_cases = if_else(
            lag(cumulative_cases, default = 0) > cumulative_cases,
            lag(cumulative_cases),
            cumulative_cases
        ),
        cumulative_active_cases = cumulative_cases - cumulative_deaths - cumulative_recoveries,
        cases_change = cumulative_cases - lag(cumulative_cases, default = 0),
        cases_growth = (((cumulative_cases / lag(cumulative_cases, default = 0)) - 1) * 100) %>% 
            round(2),
        month = date %>% month(label = T)
    ) %>% 
    filter(cumulative_cases >= 10) %>% 
    ungroup() %>% 
    select(date, month, state, cumulative_cases:cases_growth) 

covid_19_total_df <- covid_19_df %>%
    group_by(date) %>% 
    summarise(
        cumulative_cases = cumulative_cases %>% sum(),
        cumulative_deaths = cumulative_deaths %>% sum(),
        cumulative_recoveries = cumulative_recoveries %>% sum()
    ) %>% 
    mutate(
        cumulative_active_cases = cumulative_cases - cumulative_deaths - cumulative_recoveries,
        cases_change = cumulative_cases - lag(cumulative_cases, default = 0),
        cases_growth = (((cumulative_cases / lag(cumulative_cases, default = 0)) - 1) * 100) %>% 
            round(2)
    ) %>% 
    mutate(
        state = "NATIONAL",
        month = date %>% month(label = T)
    ) %>% 
    filter(cumulative_cases >= 10) %>%
    select(date, month, state, cumulative_cases:cases_growth) 

covid_19_combined_df <- covid_19_df %>% 
    bind_rows(covid_19_total_df)

shinyServer(function(input, output) {
    
    state_df <- reactive({
        if (input$state == "NATIONAL") {
            covid_19_df
        } else {
            covid_19_df %>% 
                filter(state == input$state)
        }
    })
    
    total_df <- reactive({
        covid_19_combined_df %>% 
            filter(state == input$state) %>% 
            select(date, cumulative_deaths:cumulative_active_cases) %>%
            pivot_longer(
                cols = cumulative_deaths:cumulative_active_cases,
                names_to = "variable",
                values_to = "value"
            ) %>%
            group_by(date, variable) %>% 
            summarise(value = value %>% sum()) %>% 
            mutate(
                variable = variable %>%
                    fct_relevel("cumulative_active_cases", "cumulative_recoveries", "cumulative_deaths") %>% 
                    fct_relabel(
                        ~ str_replace_all(.x, "[_]", " ") %>% 
                            str_to_title()
                    )
            ) 
    })
    
    change_df <- reactive({
        covid_19_combined_df %>%
            filter(state == input$state & cases_growth != Inf) %>% 
            slice(-1) %>% 
            select(date, cases_change, cases_growth) %>%
            pivot_longer(
                cols = -date,
                names_to = "variable",
                values_to = "value"
            ) %>%
            mutate(
                variable = if_else(
                    variable == "cases_change",
                    "Case Change",
                    "Case Growth"
                )
            ) %>% 
            group_by(date, variable) %>% 
            summarise(value = value %>% sum()) 
    })

    output$covid_19_plot <- renderPlot({
        p1 <- state_df() %>% 
        ggplot(aes(date, cumulative_cases, colour = state)) +
            geom_line() +
            geom_point() +
            geom_label_repel(data = state_df() %>% 
                                 group_by(state) %>% 
                                 slice(n()),
                             aes(date + 2,
                                 label = cumulative_cases,
                                 fill = state),
                             colour = "white",
                             show.legend = F) +
            scale_x_date(date_breaks = "1 week") +
            scale_colour_brewer(palette = "Dark2") +
            scale_fill_brewer(palette = "Dark2") +
            labs(title = "Cumulative Total Confirmed Cases by State",
                 subtitle = "Each state begins after at least 10 cases are recorded",
                 x = "Date",
                 y = "Cumulative Total Cases",
                 colour = "State",
                 caption = "Data Source (via The Guardian): https://interactive.guim.co.uk/docsdata/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE.json") +
            theme(plot.caption = element_text(size = 10))
        
        p2 <- total_df() %>% 
            ggplot(aes(date, value, fill = variable)) +
            geom_col() +
            geom_label(data = total_df() %>% 
                           group_by(variable) %>% 
                           slice(n()),
                       aes(date + 2,
                           label = value,
                           fill = variable),
                       position = position_stack(),
                       colour = "white",
                       show.legend = F) +
            scale_x_date(date_breaks = "1 week") +
            scale_fill_brewer(palette = "Accent") +
            labs(title = "Cumulative Total Confirmed Cases inc. Deaths & Recoveries*",
                 subtitle = "Begins after at least 10 cases are recorded",
                 x = "Date",
                 y = "Cumulative Totals",
                 fill = "Case Type",
                 caption = "*Data not always updated for recoveries") +
            theme(legend.position = "bottom",
                  plot.caption = element_text(size = 10))
        
        p3 <- change_df() %>% 
            ggplot(aes(date, value, fill = variable)) +
            geom_col() +
            geom_label(data = change_df() %>% 
                           group_by(variable) %>% 
                           slice(n()),
                       aes(date + 2,
                           label = value,
                           fill = variable),
                       colour = "white",
                       show.legend = F) +
            scale_x_date(date_breaks = "1 week") +
            scale_fill_brewer(palette = "Set1") +
            facet_grid(variable ~ ., scales = "free_y") +
            labs(title = "New Confirmed Cases vs. Previous Day",
                 subtitle = "First day is excluded due to extreme growth from a low base in some states",
                 x = "Date",
                 y = "Value") +
            theme(legend.position = "none")
        
        cowplot::plot_grid(p2, p3, ncol = 1) %>% 
            cowplot::plot_grid(p1, .)
    })
    
})
