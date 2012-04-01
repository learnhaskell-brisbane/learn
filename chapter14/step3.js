// functions go Number -> (Number, String)
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

// return
var unit = function(x) {
	return [x, ''];
}

// join - >>=
// x = Number, s = String, apply f (Number -> Number,String) to x, 
// y = Number result, t = String result.
var bind = function(f) {
  return function(tuple) {
    var x  = tuple[0],
        s  = tuple[1],
        fx = f(x),
        y  = fx[0],
        t  = fx[1];

    return [y, s + t];
  };
};

var lift = function(f) {
  return compose(f, unit);
};

function map(f, array) {
  var result = [];
  for (var i = 0; i < array.length; i++)
      result.push(f(array[i]));
  return result;
}

var sineOfCube = compose(bind(sine), bind(cube));
var sineOfCubeUp = compose(sineOfCube, unit);
var sineOfCubeLift = lift(sineOfCube);
// sineOfCube([12, '']);
// sineOfCubeUp(12);
// sineOfCubeLift(12);
// map(sineOfCubeLift, [12,13]);
