var sine = function(x) { return Math.sin(x) };
var cube = function(x) { return x * x * x };

var compose = function(f, g) {
  return function(x) {
    return f(g(x));
  };
};

var sineOfCube = compose(sine, cube);
