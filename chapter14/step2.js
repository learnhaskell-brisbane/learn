var sine = function(x) { 
	return [Math.sin(x), 'sine on you crazy number']
};
var cube = function(x) { 
	return [x * x * x, 'hey cube'] 
};

var compose = function(f, g) {
  return function(x) {
    return f(g(x));
  };
};

var sineOfCube = compose(sine, cube);

// Broken because sine of an array (from cube) and string being lost - bad typing