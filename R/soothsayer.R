

# series is some time series object (maybe tsibble), soothsayer is the model
yield_model <- function( series, soothsayer ) {

  # pass a string vs pass a model directly? alternatively form a model object here -
  # though that could get messy
  model <- "ar"

  return(model)
}

# this is a function which generates a soothsayer - either a closure, an S3 or an R6
# thing that can determine what to do with a series.
# hallucination allows us to generate more data from the samples, and enhance the soothsayer that way
soothsayer <- function( training_data,
                        features,
                        base_model = ranger::ranger,
                        hallucinate ) {

  # calculate features from training data
  # calculate all models
  # generate an out of sample forecast,
  # calculate desired metric

  NULL
}
