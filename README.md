monterl_carlo
=============

An experiment in modeling a monte carlo simulation in erlang
  

## API:  
  
monterl_carlo will simulate market prices on a stock exchange.  
You can start it and it will execute a function on every tick or you can have it graph a series of prices for you.  
  
###initialize    
  
```erlang
monterl_carlo:start_link("MSFT",28.61,5,0.1,0.2,1000).
```
  
where the parameters are:   
1. symbol  
2. initial price  
3. precision of the price (how many decimals)  
4. annual volatility (%)  
5. annual expected return (%)  
6. update interval (ms)  
  
###Start pumping out data:  
  
```erlang
monterl_carlo:start("MSFT",fun(X) -> io:format("~p~n",[X]) end).
```  
  
This will start pumping out the state of the simulation at the interval you specified.  
  
You can stop it by calling:  
  
```erlang
monterl_carlo:stop("MSFT").
```  
  

###Generate graph data  
  
```erlang
monterl_carlo:graph("MSFT",50, bid).
```  
  
Where the parameters are:  
1. symbol  
2. points  
3. type of data  
  
currently the types of data are:
* bid  
* ask  
* both  
* statistics  
  

