//------------------------------------------------------------------------------
// 2012-09-03 Trying to study the fundamentals of mathematics with JavaScript.
//
//------------------------------------------------------------------------------
//
m = {
  utilities : {},
  tests : {},
  operations : {}
}

//------------------------------------------------------------------------------
// Utilities 
//
//------------------------------------------------------------------------------
//
m.utilities.deduplicate_array = function(x) {
  var o = {}, 
      i, 
      l = x.length, 
      r = []
  for(i=0; i<l;i+=1)  o[x[i]] = x[i]
  for(i in o)         r.push(o[i])
  return r
}

//------------------------------------------------------------------------------
// Tests
//
//------------------------------------------------------------------------------
//
m.tests.binary_operation = function(x){return (  
  typeof x == 'function' 
  &&  x.length == 2 
)}

m.tests.set = function(x){return (  
  Object.prototype.toString.call(x) == '[object Array]'
  && x === m.utilities.deduplicate_array(x)
)}

//------------------------------------------------------------------------------
// Operations 
//
//------------------------------------------------------------------------------
//
m.operations.addition       = function(a,b){ return a + b }
m.operations.subtraction    = function(a,b){ return a - b }
m.operations.multiplication = function(a,b){ return a * b }
m.operations.division       = function(a,b){ return a / b }

//------------------------------------------------------------------------------
// Uncomment for shorthand assignment:
// o = m.operations
// t = m.tests
//------------------------------------------------------------------------------
//
//
//console.log(m.tests.binary_operation(m.operations.addition))

//x = [1,2,3]
//for(var i=0,len=x.length; i<len; i++){
//  console.log(x[i])
//}
console.log(m.utilities.deduplicate_array([1,1,3,6,1,3,4]))
