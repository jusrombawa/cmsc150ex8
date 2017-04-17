Bisection = function(f,a,b,macheps,max,verbose) #function, lower bound, upper bound, machine epsilon, boolean
{
  #0. set default values
  if(missing(max))
  {
    max = 100000;
  }
  if(missing(verbose))
  {
    verbose = TRUE;
  }
  
  count = max;
  cprev = 0;
  c = 0;
    
  while(count > 0)
  {
    if(verbose == TRUE)
    {
      print("Iteration:");
      print(max-count+1);
    }
    
    #1. choose upper and lower then check if f(lower)*f(upper) < 0

    
    check = f(a)*f(b);
    if(check > 0)
    {
      print("Cannot find root within bounds");
      return(NA);
    }
    
    #2. approximate C (root approximate) and don't forget to save cprev
    cprev = c;

    c = (a+b)/2;
    
    #3. evaluate new values for a and b (lower and upper, no particular order)
    fa = f(a);
    fb = f(b);
    fc = f(c);
    
    if(verbose == TRUE)
    {
      print("a");
      print(a);
      print("b");
      print(b);
      print("root approximate");
      print(c);
      
      print("f(a)");
      print(fa);
      print("f(b)");
      print(fb);
      print("f(root approximate)");
      f(c);
    }

    temp = fa*fc;
    
    if(temp == 0)
    {
      print("found root");
      return(c);
    }
    
    if(temp < 0) {
      print("b = c");
      b = C;
      print(b)
    } else {
      print("a = c");
      a = c;
      print(a)
    }
    
    #4. compute approximate error
    ea = abs((c-cprev)/c);
    
    if(verbose == TRUE)
    {
      print("Approximate Relative Error:");
      print(ea);
    }

    #5. check if error less than machine epsilon
    if(ea < macheps)
    {
      return(c);
    }
    
    #6. decrement count then repeat
    count = count-1;
    
  }
  
  #7. ... and if it still somehow doesn't get the result when count is exhausted, return c
  print("count exhausted");
  return(c);
}

#false position method
FalsePosition = function(f,a,b,macheps,max,verbose) #function, lower bound, upper bound, machine epsilon, boolean
{
  #0. set default values
  if(missing(max))
  {
    max = 100000;
  }
  if(missing(verbose))
  {
    verbose = TRUE;
  }
  
  count = max;
  cprev = 0;
  c = 0;
  
  while(count > 0)
  {
    print("Iteration:");
    print(max-count+1);

    #1. choose upper and lower then check if f(lower)*f(upper) < 0
    check = f(a)*f(b);
    if(check > 0)
    {
      print("Cannot find root within bounds");
      return(NA);
    }
    
    #2. approximate C (root approximate) and don't forget to save cprev
    cprev = c;
    
    c = ( b*f(a) - a*f(b) ) / ( f(a) - f(b) );
    
    #3. evaluate new values for a and b (lower and upper, no particular order)
    fa = f(a);
    fb = f(b);
    fc = f(c);
    
    if(verbose == TRUE)
    {
      print("a");
      print(a);
      print("b");
      print(b);
      print("root approximate")
      print(c);
      
      print("f(a)");
      print(fa);
      print("f(b)");
      print(fb);
      print("f(root approximate)");
      f(c);
    }
    
    
    
    temp = fa*fc;
    
    if(temp < 0)
    {
      print("b = c");
      b = C;
    }
    else if(temp > 0)
    {
      print("a = c");
      a = c;
    }
    
    else if(temp == 0)
    {
      print("found root");
      return(c);
    }
    
    #4. compute approximate error
    ea = abs((c-cprev)/c);
    if(verbose == TRUE)
    {
      print("Approximate Relative Error:");
      print(ea);
    }
    
    #5. check if error less than machine epsilon
    if(ea < macheps)
    {
      print("ea < macheps");
      return(c);
    }
    
    #6. decrement count then repeat
    count = count-1;
    
  }
  
  #7. ... and if it still somehow doesn't get the result when count is exhausted, return c
  print("count exhausted");
  return(c);
}


f = function(x) return(x*x - 4);
up = 0;
down = -12;
max = 100000;
verbose = FALSE;
macheps = 0.00001;

solution = Bisection(f,down,up,macheps,max,verbose);
#print(f(2));
print("solution")
print(solution)