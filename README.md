monterl_carlo
=============

An experiment in modeling a monte carlo simulation in erlang
  
# API:  
  
monte_carlo:start_link("MSFT",28.61,3,0.1,0.2,1000).  
  
monte_carlo:start("MSFT",fun(X) -> io:format("~p~n",[X]) end).  
  
monte_carlo:stop("MSFT").  