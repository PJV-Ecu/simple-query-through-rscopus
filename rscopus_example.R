#https://cran.r-project.org/web/packages/rscopus/vignettes/api_key.html
#https://cran.r-project.org/web/packages/rscopus/vignettes/multi_author.html
#https://github.com/HBLL-Collection-Development/scopus-api/blob/master/README.md
#https://www.ilovephd.com/list-of-subject-areas-covered-by-scopus-database/

library(pacman)

p_load("rscopus", "dplyr", "tidyr", "ggplot2", "ggthemes", "stringr")

if (have_api_key()) {
    make_query <- function(univ_name) {
        paste0("AFFIL(\"", univ_name, "\") AND PUBYEAR = 2024") #opción: AND ACCESSTYPE(OA)
    }
    
    univ <- c("Instituto Nacional de Investigación en Salud Pública", "Instituto de Investigación Geológico y Energético", "Instituto Nacional de Meteorología e Hidrología", "Instituto Nacional de Patrimonio Cultural", "Instituto Nacional de Investigaciones Agropecuarias", "Instituto Nacional de Biodiversidad")
    #i <- 1
    #subj_area <- subject_areas()[i] # Selects the first subject area
    #print(subj_area)
    
    results_df <- data.frame(University = character(), Total_Results = numeric(), stringsAsFactors = FALSE)
    
    for (university in univ) {
        query <- make_query(university)
        print(paste("Searching for:", university))
        completeArticle <- scopus_search(
            query = query,
            view = "STANDARD",
            count = 25
        )
        if (!is.null(completeArticle)) {
            total_results <- completeArticle$total_results
            total_results <- as.numeric(total_results)
            print(paste("Total results for", university, ":", total_results))
            print(names(completeArticle))
            results_df <- rbind(results_df, data.frame(University = university, Total_Results = total_results, stringsAsFactors = FALSE))
        } else {
            print(paste("No results found for", university))
            results_df <- rbind(results_df, data.frame(University = university, Total_Results = NA, stringsAsFactors = FALSE))
        }
    }
    
    # Create the bar plot
    plot <- ggplot(results_df, aes(y = reorder(University, Total_Results), x = Total_Results, colour = University,
                                   xend = 0, yend = University)) +
        geom_segment(linewidth = 3) +
        geom_point(size = 5) +
        geom_vline(xintercept = 65, linetype = 2, colour = "grey20") +
        theme_solarized_2() +
        labs(
            title = "Publicaciones en Agricultura y Ciencias Biológicas",
            x = "Universidad",
            y = "Publicaciones totales SCOPUS"
        ) +
        theme(legend.position = "none")
    
    print(plot)
    
    ggsave("plot3.png", plot, width = 13.14, height = 8.12)
    
} else {
    print("API key not found. Please set your API key using set_api_key().")
}

ggsave("plot2.png", plot, width = 8.12, height = 13.14)