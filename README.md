monterl_carlo
=============

An experiment in modeling a monte carlo simulation in erlang
  

## API:  
  
monterl_carlo:start_link("MSFT",28.61,5,0.1,0.2,1000).    

monterl_carlo:start("MSFT",fun(X) -> io:format("~p~n",[X]) end).  
  
monterl_carlo:stop("MSFT").  
  
  
This starts pumping out prices and other statistics to your shell.  

