#' @title Plot Urban Water Data
#' @description This function generates a simple visualization of Urban Water Data.
#' @param data Data to plot. Must be in the format outputted from \code{pull_data()} function
#' @param category_selection The category that the user would like plotted.
#' Coverage across reporting requirements varies by category.
#' Can be \code{"supply"}, \code{"supply total"}, \code{"demand"}, \code{"demand total"}, \code{"losses"}
#' @param pwsid_selection The Public Water System Identification Number (PWSID) corresponding
#' to the agency whose data you would like to visualize.
#' @param show_subcategories Defaults to FALSE. If set to TRUE shows subcategories for use types.
#' @examples
#' \dontrun{
#' pull_data(2020) |>
#'     plot_data(category_selection = "demand", pwsid = "CA3610001")
#' }
#' @export
plot_data <- function(data,
                      category_selection = c("supply", "supply total", "demand",
                                            "demand total", "losses"),
                      pwsid_selection,
                      show_subcategories = FALSE) {
  if (show_subcategories == FALSE) {
  filtered_data <- data |>
    filter(category == category_selection,
           pwsid == pwsid_selection) |>
    group_by(report_name) |>
    summarise(volume_af = sum(volume_af, na.rm = T))

  plot <- plot_ly(filtered_data, y = ~report_name, x = ~volume_af, fill = ~report_name,
                  type = "bar", marker = list(color = wesanderson::wes_palette("Royal2"))) |>
    layout(xaxis = list(title = paste0("Volume (Acre Feet) ", category_selection, " across reports for ", pwsid_selection)),
           yaxis = list(title = ""))
  }
  if (show_subcategories == TRUE) {
    filtered_data <- data |>
      filter(category == category_selection,
             pwsid == pwsid_selection) |>
      group_by(report_name, use_group) |>
      summarise(volume_af = sum(volume_af, na.rm = T))

    plot <- ggplotly(ggplot(filter(filtered_data, !is.na(use_group)), aes(y = report_name, x = volume_af, fill = use_group)) +
                       geom_col() +
                       labs(x = paste0("Volume (Acre Feet) ", category_selection, " across reports for ", pwsid_selection), y = "") +
                       theme_minimal() +
                       theme(text = element_text(size=14),
                             legend.title = element_blank(),
                             legend.text = element_text(size = 10)) +
                       scale_fill_manual(values = wesanderson::wes_palette("Royal2", 23, type = "continuous")))
  }
return(plot)
}
