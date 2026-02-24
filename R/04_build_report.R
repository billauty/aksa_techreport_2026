library(officer)
library(flextable)
library(ggplot2)

create_accessible_yearbook <- function(test_content_objects, intro_docx) {
  
  # Open the base Quarto introduction
  doc <- read_docx(intro_docx) |> 
    cursor_end() |> 
    body_add_break()
  
  # Loop through each generated test package
  for (content in test_content_objects) {
    
    # 1. Heading
    doc <- body_add_par(doc, content$test_name, style = "heading 1")
    
    # 2. Add Flextable natively (NO images, NO XML hacks)
    doc <- body_add_flextable(doc, content$item_stats_table)
    doc <- body_add_par(doc, "") 
    
    # 3. Add ggplot natively WITH Alt Text
    temp_img <- tempfile(fileext = ".png")
    ggsave(temp_img, plot = content$item_fit_plot, width = 6.5, height = 4.5, dpi = 300)
    
    accessible_image <- external_img(src = temp_img, alt_text = content$fit_plot_alt)
    doc <- body_add_fpar(doc, fpar(accessible_image))
    doc <- body_add_par(doc, "")
    doc <- body_add_break(doc) # Put each test on a new page
  }
  
  out_path <- "reports/Final_Technical_Report_2026.docx"
  print(doc, target = out_path)
  return(out_path)
}