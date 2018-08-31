# Sigmoid activation function
sigmoid <- function(a) {
  1 / (1 + exp(-a))
}

# Binary step activation function
binaryStep <- function(a){
  if(a<0){
    result <- 0
  }
  else
  {
    result <- 1
  }
  return(result)
}

# Function to depict input to neuron
input <- function(bias,weight,x){
  weighted_data <- weight * x
  aggregatedWeight <- sum(weighted_data)
  #aggregatedBias <- sum(bias)
  inputNeuron <- bias + aggregatedWeight
  return(inputNeuron)
}

# Neuron with sigmoid activation function
neuron_sigmoid <- function(b,w,x){
  inputNeuron <- input(b,w,x)
  neuronBySigmoid <- sigmoid(inputNeuron)
  return(neuronBySigmoid)
}

# Neuron with binary step activation function
neuron_binary <- function(b,w,x){
  inputNeuron <- input(b,w,x)
  neuronByBinary <- binaryStep(inputNeuron)
  return(neuronByBinary)
}

# XOR network
xorNetwork <- function(x1,x2){
  x <- rbind(x1+x2)
  h1 <- neuron_binary(-10,20,x)
  h2 <- neuron_binary(30,-20,x)
  h <- rbind(h1,h2)
  output <- neuron_binary(-30,20,h)
  return(output)
}

# Testing neural network with different input sets
xorNetwork(1,1)
xorNetwork(0,1)
xorNetwork(1,0)
xorNetwork(0,0)
