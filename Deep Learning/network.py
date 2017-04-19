class Network(object):

	def __init__(self, sizes):
		self.num_layers = len(sizes)
		self.sizes = sizes
		#	Network initialization code assumes that the first layer of neurons is an input layer 
		#	and omits to set any biases for those neurons
		self.biases = [np.random.randn(y, 1) for y in sizes[1:]]
		self.weights = [np.random.randn(y, x) for x, y in zip(sizes[:-1], sizes[1:])]
						
def sigmoid(z):
	return 1.0/(1.0+np.exp(-z))