library(data.table)

# Create the dataset
origin <- data.table(
  person_id = c("Ga","P1","Gb", "P1","Gc", "P1","Gd", "P1"),
  start_match = c(100, 100, 125,125, 300,300, 325, 325),
  end_match = c(200,200, 233, 233, 350, 350, 600, 600),
  is_pregnancy = c(1,0,1,0,1,0,1,0)
)

data <- origin[is_pregnancy == 0,]
