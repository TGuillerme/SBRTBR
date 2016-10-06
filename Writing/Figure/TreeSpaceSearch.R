# Plots for illustrating the ideal tree search algorithm

image(t(volcano)[ncol(volcano):1,])

image(volcano)
image(volcano*rnorm(dim(volcano)[1]*dim(volcano)[2]))

flat_land <- matrix(110, 87, 61)
volcano_modif <- apply(volcano, c(1,2), function(X) ifelse(X > 140, sample(130:200, 1), X))

sampler <- function(start, end) {
    x1 <- sample(start:end, 1)
    x2 <- sample(x1:end, 1)
    return(c(x1, x2))
}

matrix <- NULL
start <- 94
end <- 195
while(length(matrix) < length(volcano))
{
    sampling <- sampler(start, end)
    matrix <- c(matrix, seq(from = sampling[1], to = sampling[2]))
    start <- matrix[length(matrix)]
    sampling <- sampler(start, end)
    matrix <- c(matrix, seq(from = sampling[2], to = sampling[1]))
    start <- matrix[length(matrix)]
}
matrix_image <- matrix(matrix[1:length(volcano)], 87, 61)


image(cbind(rbind(volcano, flat_land), rbind(flat_land, matrix_image)))

image(matrix_image)