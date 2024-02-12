

make_afl_photo <- function(starting_alpha){

imgpaths <- list.files(path="D:/data/player_photos_aflw/",
                       pattern = glue("^2023_"), full.names = T)

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

png(glue('output/faces/aflw/bulk/bulk_AFL_shuffle_{starting_alpha}.png'), width = 2, height = 2, units = 'in', res = 300)
par(mai=c(0,0,0,0))
plot.new()

walk2(.x = imgpaths_shuffle$imgpaths_shuffle,
      .y = imgpaths_shuffle$alpha_vec,
      .f = load_and_raster)

dev.off()

}

make_afl_photo(0.0451)
make_afl_photo(0.0452)
make_afl_photo(0.0453)
make_afl_photo(0.0454)
make_afl_photo(0.0455)
make_afl_photo(0.0456)
make_afl_photo(0.0457)
make_afl_photo(0.0458)
make_afl_photo(0.0459)



walk(seq(0.03,0.09,0.005),make_afl_photo)
