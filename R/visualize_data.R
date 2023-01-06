#' @title Plot Urban Water Data
#' @description This function generates a simple visualization of Urban Water Data.
#' @param selected_category The category that the user would like plotted.
#' Coverage accross reporting requirements varies by category.
#' Can be \code{"supply"}, \code{"supply total"}, \code{"demand"}, \code{"demand total"}, \code{"losses"}
#' @param mode The mode to run model in. Can be \code{"seed"}, \code{"simulate"}, \code{"calibrate"}
#' @param pwsid The Public Water Sysetm Identification Number (PWSID) corresponding
#' to the agency whose data you would like to visualize.
#' @examples
#' plot_data(selected_category = "demand", pwsid = "CA3610001")
#' @export

plot_data <- function(selected_category = c("supply", "supply total", "demand",
                                            "demand total", "losses"),
                      pwsid) {
  if (show_subcategories == FALSE) {
  filtered_data <- expected_data |>  # TODO update to be data pull function
    filter(category == selected_category) |> # TODO can add filters for PWSID here to just show one agency (makes more sense that way)
    group_by(report_name) |>
    summarise(volume_af = sum(volume_af, na.rm = T))

  plot <- plot_ly(filtered_data, y = ~report_name, x = ~volume_af, fill = ~report_name,
                  type = "bar", marker = list(color = wesanderson::wes_palette("Royal2"))) |>
    layout(xaxis = list(title = paste0("Volume (Acre Feet) ", selected_category, " across reports for ", pwsid)),
           yaxis = list(title = ""))
  }
  if (show_subcategories == TRUE) {
    filtered_data <- expected_data |>  # TODO update to be data pull function
      filter(category == selected_category,
             # supplier_name == "Santa Fe Irrigation District"
             ) |> # TODO can add filters for PWSID here to just show one agency (makes more sense that way)
      group_by(report_name, use_type) |>
      summarise(volume_af = sum(volume_af, na.rm = T))

    colors <- c("#9A8822", "#899DA4", "#C93312", "#F8AFA8", "#DC863B", "#FDDDA0", "#74A089", "#E1BD6D", "#FAEFD1", "#DC863B")
    plot <- ggplotly(ggplot(filter(filtered_data, !is.na(use_type)), aes(y = report_name, x = volume_af, fill = use_type)) +
                       geom_col() +
                       labs(x = paste0("Volume (Acre Feet) ", selected_category, " across reports for ", pwsid), y = "") +
                       theme_minimal() +
                       theme(text = element_text(size=14),
                             legend.title = element_blank(),
                             legend.text = element_text(size = 10)) +
                       scale_fill_manual(values = colors))
  }



return(plot)
}
