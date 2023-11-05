library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(viridis)
library(leaflet)
library(plotly)
library(tidyr)
library(mapview)
library(forcats)
library(leafpop)
# Define a custom popup function

# 
# # load in Shapefile
# district_shapes <- st_read( "Geo FIles/Districts_261/District_261.shp")%>% 
#     st_make_valid()  %>% 
#     mutate(District  = ifelse(District  =="Cape Cape Metropolitan Area (CCMA)", 
#                               "Cape Coast Metropolitan Area (CCMA)", 
#                               District )) 
# 
# write_sf(district_shapes, "shp_files/district_shapes.shp")
white_to_green = colorRampPalette(c('darkred', 'white', 'darkgreen'))

custom_colors <- c('score_internet' = "#40bac9", 
                   'score_education' = "#6d1785", 
                   'score_electricity' = "#dce622")

data_for_app <- readRDS("data_for_app.rds")
district_shapes <- read_sf("district_shapes_simple.json")

ui <- fluidPage(
    

    theme = bs_theme(
        bg = "#101010",
        fg = "#FFF",
        primary = "#E69F00",
        secondary = "#0072B2",
        success = "#009E73",
        base_font = font_google("Azeret Mono"),
        code_font = font_google("Azeret Mono")
    ),
    titlePanel("Ranking Calculator"),
    sidebarLayout(
        sidebarPanel(
            #style = "overflow-y: scroll; max-height: 1000px;", 
            sliderInput("score_within_10", "Score for population within 10km of school:", min = 0, max = 100, value = 50),
            sliderInput("score_within_5", "Score for population within 5km of school:", min = 0, max = 100, value = 50),
            sliderInput("score_enrollment", "Score for School Enrollment rate (3-14 years)", min = 0, max = 100, value = 50),
            sliderInput("score_electricity_nightlight", "Score % of Population living in areas with (based on nightlight satallite data)", min = 0, max = 100, value = 50),
            sliderInput("score_electricty_census", "Score Electricity Census:", min = 0, max = 100, value = 50),
            sliderInput("score_own_laptop", "Score Own Laptop:", min = 0, max = 100, value = 10),
            sliderInput("score_own_smartphoe", "Score Own Smartphone:", min = 0, max = 100, value = 10),
            sliderInput("score_used_internet", "Score used Internet:", min = 0, max = 100, value = 50),
            sliderInput("rank_multiplier_ookla", "rank multiplier ookla:", min = 0, max = 1, value = 100/261/5),
            sliderInput("rank_multiplier_mlab", "rank multiplier mlab:", min = 0, max = 1, value = 100/261/5),
            checkboxInput("deflate", "Deflate Scores", value = TRUE)
        ),
        mainPanel(
            leaflet::leafletOutput("rankMap", height = "500px"),
            p("Rank of districts"),
            div(style = "overflow-y: scroll; max-height: 700px;", 
                plotlyOutput("rankBar", height = "3500px")
            )

        )
    )
)


server <- function(input, output, session) {
    
    # Create a reactive object for the ranks
    ranks <- reactive({
       data <-  data_for_app %>%
            mutate(score_education = proportion_within_school_10k_n * input$score_within_10 + 
                       proportion_within_school_5k_n * input$score_within_5 +
                       (percentage_3_14_years_currently_enrolled/100) * input$score_enrollment,
                   score_electricity = proportion_with_elec_access * input$score_electricity_nightlight + 
                       (electric_census_use/100) * input$score_electricty_census,
                   score_internet = ookla_rank * input$rank_multiplier_ookla +
                       mlab_rank * input$rank_multiplier_mlab +
                       proportion_owns_laptop *  input$score_own_laptop +
                       proportion_owns_smartphone * input$score_own_smartphoe + 
                       (proportion_used_internet_census) * input$score_used_internet
                   
            ) %>%
            mutate(score_final = (score_education + score_electricity + score_internet)) 
        
        if (input$deflate) {
            data <- data %>%
                mutate(score_final_deflated = score_final * (1 - prop_illit_eng_literate_local))
        } else {
            data <- data %>%
                mutate(score_final_deflated = score_final)
        }
        
        data <- data %>%
            mutate(rank_score = rank(score_final_deflated),
                   rank_elec = rank(score_electricity ),
                   rank_internet = rank(score_internet ),
                   rank_education = rank(score_education  )) %>%
            arrange(desc(rank_score))
        
        return(data)
    })
    
    
    output$rankMap <-  leaflet::renderLeaflet({
        # Assuming all_data_district_levels_df and district_shapes are loaded in the global environment
        joined_data <- district_shapes %>%
            inner_join(ranks(), by = join_by(District))
        
        
        map1 <- mapview(joined_data, zcol = "score_final_deflated", legend = TRUE, layer.name = "Score Layer")
        map2 <- mapview(joined_data, zcol = "rank_score", legend = TRUE, layer.name = "Rank Layer")
        
        map_total <- map1 + map2
       
        map_total@map
        
    })
    
        output$rankMap <-  leaflet::renderLeaflet({
        # Assuming all_data_district_levels_df and district_shapes are loaded in the global environment
        joined_data <- district_shapes %>%
            inner_join(ranks(), by = join_by(District)) %>% 
            mutate(score_final_deflated = round(score_final_deflated, 1))
        
        map1 <- mapview(joined_data, zcol = "score_final_deflated", 
                        legend = TRUE, layer.name = "Score Layer", 
                        map.types = c("CartoDB.DarkMatter"),
                        popup = popupTable(joined_data, feature.id = FALSE, zcol= c("score_final_deflated")),
                        alpha.regions = 0.9,
                        
                        col.regions = white_to_green)
        map2 <- mapview(joined_data, zcol = "rank_score", 
                        map.types = c("CartoDB.DarkMatter"),
                        alpha.regions = 0.9,
                        popup = popupTable(joined_data, feature.id = FALSE, zcol= c("rank_score")),
                        
                        legend = TRUE, layer.name = "Rank Layer")
        
        map_total <- map1| map2
       
        map_total@map
        
    })
        

        output$rankBar <- renderPlotly({
            ranked_data <- ranks()  %>%
                mutate(rank_score = rank(-score_final_deflated),
                       rank_elec = rank(-score_electricity ),
                       rank_internet = rank(-score_internet ),
                       rank_education = rank(-score_education  )) %>%
                arrange(desc(rank_score)) %>% 
                select(District   ,  rank_score,rank_elec, rank_internet, rank_education, score_internet , score_education,score_electricity,score_final_deflated ) %>% 
                pivot_longer(cols = c(score_internet , score_education,score_electricity))%>%
                arrange(desc(score_final_deflated )) %>% 
                mutate(District = as.factor(District)) %>% 
                mutate(District = fct_reorder(District, score_final_deflated)) %>% 
                mutate(rank = case_when(name == "score_internet"  ~ rank_internet,
                                        name == "score_education"  ~ rank_education ,
                                        name == "score_electricity"  ~ rank_elec ))%>% 
                mutate(type = case_when(name == "score_internet"  ~ "internet rank",
                                        name == "score_education"  ~"education rank" ,
                                        name == "score_electricity"  ~ 'electricity rank' ))  %>% 
                mutate(type = case_when(name == "score_internet"  ~ "internet rank",
                                        name == "score_education"  ~"education rank" ,
                                        name == "score_electricity"  ~ 'electricity rank' )) 
            
            hover_text <- paste(
                "District: ", ranked_data$District, "<br>",
                "Rank: ", ranked_data$rank_score, "<br>",
                "Score: ", round(ranked_data$score_final_deflated, 2),
                sep = ""
            )
            
            plot_ly(
                data = ranked_data,
                y = ~District,
                x = ~value,
                color =~name,  # Convert name to factor with specified order
                colors = custom_colors,  # Specify custom colors
                type = 'bar',
                orientation = 'h',
                #hovertemplate = "<b><i>District: %{label}</i></b> <br> <b>%{x}</b>"
                hoverinfo = 'text',
                hovertext = ~paste0("<b>",District, "</b>\n", gsub( "_", " ", name), ': ', round(value, 2), '\n', type, ": ", rank,   "\noverall rank: ",rank_score )
            ) %>%
                layout(
                    #title = "District Rankings",
                    title = "",
                    legend = list(
                        orientation = "h",
                        x = 0.5,
                        y = 1.02,
                        xanchor = "center",
                        yanchor = "top"
                    ),
                    xaxis = list(title = ""),
                    yaxis = list(title = "", 
                                 ticklabelmode = 'show',
                                 tickvals = ~District,
                                 autorange = 'reversed'
                                 
                                 ),
                    font = list(color = 'white',
                                family = 'Azeret Mono',
                                size = 14),
                    plot_bgcolor = "rgba(0,0,0,0)",
                    paper_bgcolor = "rgba(0,0,0,0)",
                    barmode = 'stack'  # This makes the bar chart stacked
                )%>% config(displayModeBar = F)
            
        })

}

shinyApp(ui = ui, server = server)
