box::use(
  htmlwidgets[JS],
  plotly[config, ggplotly, layout, plotly_build],
)

#' Render ggplot as interactive Plotly chart with download button
#'
#' @param g ggplot object
#' @param data Data to embed for download
#' @param csv_name Custom CSV name for download
#' @return Plotly object with integrated download button
build_interactive_plot <- function(g, data, csv_name = "plot_data.csv") {
  #  Build base plot with plotly
  p <- ggplotly(g, tooltip = "text") |>
    layout(legend = list(orientation = "h")) |>
    config(modeBarButtonsToRemove =
             list("zoom2d", "pan2d",
                  "select2d", "lasso2d",
                  "zoomIn2d", "zoomOut2d",
                  "autoScale2d", "hoverClosestCartesian",
                  "hoverCompareCartesian"),
           displayModeBar = TRUE)

  p <- plotly_build(p)

  #  Embed data in the widget for download
  p$x$layout$meta <- list(csv_name = csv_name, full_data = data)

  #  Custom download button
  download_btn <- list(
    name = "download_plot_data",
    icon = list(
      path = "M6 2h8a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2zm0 2v4h8V4zM8 14h4v-4H8v4z",
      transform = "matrix(1 0 0 1 0 0) scale(1)"
    ),
    title = "Download dataset as a CSV",
    click = JS("
      function(gd) {
        var meta = gd.layout.meta || {};
        var fullData = meta.full_data;
        var csvName = meta.csv_name || 'plot_data.csv';

        var csv = '';
        if (fullData && fullData.length > 0) {
          var keys = Object.keys(fullData[0]);
          csv += keys.join(',') + '\\n';
          fullData.forEach(function(row) {
            csv += keys.map(k => row[k]).join(',') + '\\n';
          });
        } else {
          var rows = [];
          gd.data.forEach(function(trace, t) {
            var name = trace.name || ('Trace ' + (t + 1));
            (trace.x || []).forEach(function(xVal, i) {
              rows.push([name, xVal, trace.y[i]].join(','));
            });
          });
          csv = 'trace,x,y\\n' + rows.join('\\n');
        }

        var blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
        var url = URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = csvName;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
      }
    ")
  )

  p <- p |>
    config(
      modeBarButtonsToAdd = list(download_btn)
    )
}