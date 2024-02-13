

make_afl_photo <- function(starting_alpha){

imgpaths <- list.files(path="D:/data/player_photos/",
                       pattern = glue("^2022_"), full.names = T)

# Initialize a vector to store the data
data_vec <- numeric(length(imgpaths))

# Set the starting value
current_val <- starting_alpha

# Loop 450 times to generate the data
for (i in 1:length(imgpaths)) {
  # Store the current value in the vector
  data_vec[i] <- current_val

  # Reduce the current value by length %
  current_val <- current_val
}

## Shuffle order

imgpaths_shuffle <- imgpaths %>%
  as_tibble() %>%
  mutate(id = row_number()) %>%
  sample_n(nrow(.)) %>%
  select(value)%>%
  unlist()

imgpaths_shuffle <- cbind(imgpaths_shuffle,data_vec) %>%
  as.tibble() %>%
  mutate(alpha_vec = as.numeric(data_vec)) %>%
  select(-data_vec)

png(glue('output/faces/bulk/bulk_AFL_shuffle_{starting_alpha}.png'), width = 2, height = 2, units = 'in', res = 150)
par(mai=c(0,0,0,0))
plot.new()

walk2(.x = imgpaths_shuffle$imgpaths_shuffle,
      .y = imgpaths_shuffle$alpha_vec,
      .f = load_and_raster)

dev.off()

}

make_afl_photo(0.055555)

walk(seq(0.01,0.3,0.01),make_afl_photo)
