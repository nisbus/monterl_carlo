monterl_carlo
=============

An experiment in modeling a monte carlo simulation in erlang
  

## API:  
  
monterl_carlo will simulate market prices on a stock exchange.  
  
You initialize it with:  

```erl
monterl_carlo:start_link("MSFT",28.61,5,0.1,0.2,1000).
```
  
where the parameters are:   
1. symbol  
2. initial price  
3. precision of the price (how many decimals)  
4. annual volatility (%)  
5. annual expected return (%)  
6. update interval (ms)  
  
after that you can start it with:  
  
```erl
monterl_carlo:start("MSFT",fun(X) -> io:format("~p~n",[X]) end).
```  
  
This will start pumping out the state of the simulation at the interval you specified.  
  
You can then stop it by calling:  
  
```erl
monterl_carlo:stop("MSFT").
```  
