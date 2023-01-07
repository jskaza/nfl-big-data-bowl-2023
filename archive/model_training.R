library(keras)

Y <- res$coords
X <- res$opponent_coords

model <- keras_model_sequential() |> 
  layer_lstm(units = 32, input_shape = c(2)) |> 
  layer_dense(units = 2, activation = 'linear')

# Define the loss function and the optimizer
model |> compile(loss = 'mean_squared_error', optimizer = 'adam')