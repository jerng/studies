%  This is an attempt to write a mathematics library in Erlang.
%
%   1.  Don't duplicate the "math" module; possibly this module will contribute 
%       to that module in the future.
%   2.  Try to maintain a separation of functions for statistics and data 
%       processing; possibly these will be spun off into separate libraries.
%   3.  Minimise complexity of input operands; perhaps write other functions
%       explicitly for mapping. 
%   4.  Figure out how to call C/Fortran code from this library.
%   5.  Aim for a monolithic "m_stdlib" (Python-esque); aim for an it-just-works 
%       framework for extensibility, for code that's being considered for the
%       "m_stdlib".
%   6.  Keep the namespace clean. Actually, it would be nice to clean up stdlib.
%   7.  Emphasise code reuse; complex functions return intermediate and final 
%       results. 
%   8.  Emphasise short function calls; minimise name lengths and arities.
%
%
%
%
%

mean_a(List_X)->
  Count_X = length(List_X),
  MeanA_X = lists:foldl( 
    fun(X,Acc)-> X+Acc end, 
    0, 
    List_X
  ) 
  / Count_X,
  { MeanA_X, Count_X}.

variance(List_X)->
  { MeanA_X, Count_X } = mean_a(List_X),
  Variance_X = lists:foldl(
    fun(X,Acc)-> math:pow(X - MeanA_X, 2) + Acc end,
    0,
    List_X
  ),
  { Variance_X, MeanA_X, Count_X }.

std_dev_sam(List_X)->
  { Variance_X, MeanA_X, Count_X } = variance(List_X),
  StdDevSam_X = math:sqrt(Variance_X / (Count_X-1)),
  { StdDevSam_X, Variance_X, MeanA_X, Count_X }.
