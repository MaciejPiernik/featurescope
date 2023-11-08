library(keras)

EuclideanLayer <- R6::R6Class("EuclideanLayer",
                           
   inherit = KerasLayer,
   
   public = list(
       
       output_dim = NULL,
       
       kernel = NULL,
       
       initialize = function(output_dim) {
           self$output_dim <- output_dim
       },
       
       build = function(input_shape) {
           self$kernel <- self$add_weight(
               name = 'kernel', 
               shape = list(input_shape[[2]], self$output_dim),
               initializer = initializer_random_uniform(minval = 0, maxval = 1, seed = NULL),
               trainable = TRUE
           )
       },
       
       call = function(x, mask = NULL) {
           k_sqrt(k_dot(x, k_transpose(x)) - 2*k_dot(x, self$kernel) + k_sum(k_square(self$kernel), axis=1))
       },
       
       compute_output_shape = function(input_shape) {
           list(input_shape[[1]], self$output_dim)
       }
   )
)

layer_euclidean <- function(object, output_dim, name = NULL, trainable = TRUE) {
    create_layer(EuclideanLayer, object, list(
        output_dim = as.integer(output_dim),
        name = name,
        trainable = trainable
    ))
}