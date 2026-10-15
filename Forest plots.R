#install.packages("readxl")
library(readxl)
#install.packages('stringr')
library(stringr)
#remotes::install_github(repo = "EBPI-Biostatistics/biostatUZH", build_vignettes = FALSE)
library(biostatUZH)

data=read_excel("Sensitivity and censoring summary.xlsx", sheet = "Analysis results")
library(tidyverse)
library(dplyr)

# Split type of sensitivity analyses strings into a character vector
#split_cats <- strsplit(data$`Type of sensitivity analysis`, ", ")
#all_cats <- sort(unique(unlist(split_cats)))

all_cats <- sort(unique(unlist(data$`Type of sensitivity analysis`)))


# Group and split the data frame creating one object per NCT
split_df_grouped <- data %>%
  group_by(NCT) %>%
  group_split()







#Create a forest plot with all the sensitivity anlyses for each study

library(forestplot)
w <- c()
n <- c()
pdf_files <- c()

for (i in seq_along(split_df_grouped)) {
  NCT1 <- split_df_grouped[[i]]
  NCT_name <- unique(NCT1$NCT)
  #print(paste("Processing NCT:", NCT_name))
  
  # Coerce columns to character first inside the loop (in case they are factors)
  NCT1$HR <- as.character(NCT1$HR)
  NCT1$`CI Lower bound` <- as.character(NCT1$`CI Lower bound`)
  NCT1$`CI upper bound` <- as.character(NCT1$`CI upper bound`)
  
  # Find rows with NA HR in original data
  x <- c()
  for (j in 1:nrow(NCT1)) {
    if (is.na(NCT1$HR[j])) {
      #print(paste(NCT1$Analysis[j], "No HR value"))
      x <- c(x, j)
      n <- c(n, NCT_name)
    }
  }
  
  # Safely coerce to numeric inside the loop on cleaned character columns
  HR <- suppressWarnings(as.numeric(NCT1$HR))
  CI_lower <- suppressWarnings(as.numeric(NCT1$`CI Lower bound`))
  CI_upper <- suppressWarnings(as.numeric(NCT1$`CI upper bound`))
  
  for (k in 1:nrow(NCT1)) {
    if (is.na(HR[k]) & !is.na(NCT1$HR[k])) {
      #print(paste(NCT_name, NCT1$Analysis[k], "has no HR, but reported as:", NCT1$HR[k]))
      x <- c(x, k)
      w <- c(w, NCT_name)
    }
  }
  
  if (length(x) > 0) {
    NCT1 <- NCT1[-x, ]
    HR <- HR[-x]
    CI_lower <- CI_lower[-x]
    CI_upper <- CI_upper[-x]
  }
  
  if (nrow(NCT1) == 0) {
    #cat(" -", NCT_name, ": No valid data for plotting\n")
    next
  }
  
  labeltext <- rbind(
    c("Analysis", "HR (95% CI)"),
    cbind(NCT1$Analysis, sprintf("%.3f (%.3f–%.3f)", HR, CI_lower, CI_upper))
  )
  
  mean  <- c(NA, HR)
  lower <- c(NA, CI_lower)
  upper <- c(NA, CI_upper)
  
  pdf_file <- paste0("forestplot_", NCT_name, ".pdf")
  
  # Calculate number of rows: +1 for header
  n_rows <- nrow(NCT1) + 1
  
  # Dynamically set PDF height in inches (approx. 1.1 inches per row)
  row_height_in <- 1.1
  margin_in <- 2
  pdf_height <- row_height_in * n_rows + margin_in
  
  pdf_width <- 11  # Wide enough for labels and CI
  
  # Save the plot as PDF
  pdf(pdf_file, width = pdf_width, height = pdf_height)
  #grid::grid.newpage()  # Ensure grid graphics page is ready
  
  print(
    forestplot(
      labeltext,
      mean = mean,
      lower = lower,
      upper = upper,
      zero = 1,
      boxsize = 0.2,
      line.margin = 0.5,
      xlog = TRUE,
      title = NCT_name,
      col = fpColors(box = "blue", line = "black", zero = "gray50"),
      lwd.zero = 3,
      txt_gp = fpTxtGp(
        xlab = gpar(fontsize = 14),
        ticks = gpar(fontsize = 20)
      ),
      new_page = FALSE,
      graphwidth = unit(3, "in")
    )
  )
  
  dev.off()
  pdf_files <- c(pdf_files, pdf_file)
}

library(qpdf)

# List your PDF files
pdf_files <- list.files(pattern = "^forestplot_.*\\.pdf$")

# Combine them into one PDF
pdf_combine(input = pdf_files, output = "combined_forestplots.pdf")