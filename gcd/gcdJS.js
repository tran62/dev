function gcd(a,b) {
    a = Math.abs(a);
    b = Math.abs(b);
  
    if (b > a) {
      var temp = a;
      a = b;
      b = temp; 
    }
  
    while (true) {
      a %= b;
      if (a === 0) { return b; }
      b %= a;
      if (b === 0) { return a; }
    }
  }
  console.log("gcd (57, 171): ",gcd_rec(57,171));


  // Recursive.
  
  function gcd_rec(a, b) {
    return b ? gcd_rec(b, a % b) : Math.abs(a);
  }
  console.log("gcd_rec (57, 190): ",gcd_rec(57,190));


  // Implementation that works on an array of integers.
  
  function GCD(arr) {
    var i, y,
        n = arr.length,
        x = Math.abs(arr[0]);
  
    for (i = 1; i < n; i++) {
      y = Math.abs(arr[i]);
  
      while (x && y) {
        (x > y) ? x %= y : y %= x;
      }
      x += y;
    }
    return x;
  }
  
  //For example:
  console.log("GCD([57,0,-45,-18,90,447]: ",GCD([57,0,-45,-18,90,447])); //=> 3