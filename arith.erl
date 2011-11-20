-module(arith).
-include_lib("eunit/include/eunit.hrl").

-export([ is_prime/1, gcd/2, coprime/2, phi/1, list_of_prime/2,
	 prime_factors/1, prime_factors_mult/1, phi_imp/1, goldbach/1,
	 goldbach_list/2 ]).
%% P31 (**) Determine whether a given integer number is prime.%%
%% Example:%% * (is-prime 7)
%% T

is_prime(N) when N =< 1 ->
    false;
is_prime(N) ->
    is_prime(N,2).    

is_prime(N,Count) when Count < N -> 
    case N rem Count == 0 of
	true ->
	    false;
	false ->
	    is_prime(N,Count+1)
    end;
is_prime(N,Count) when Count >= N-> 
    true.

%% P32 (**) Determine the greatest common divisor of two positive
%% integer numbers.
%% Use Euclid's algorithm.  Example: *(gcd 36 63) 9

%% In Euclid's Elements (Book VII) we find a way of calculating the
%% gcd of two numbers, without listing the divisors of either
%% number. It is now called Euclid's Algorithm. [An algorithm is a
%% step by step process (or recipe) for doing something.] First, I
%% will describe it using an example. We will find the gcd of 36 and
%% 15. Divide 36 by 15 (the greater by the smaller), getting 2 with a
%% remainder of 6. Then we divide 15 by 6 (the previous remainder) and
%% we get 2 and a remainder of 3. Then we divide 6 by 3 (the previous
%% remainder) and we get 2 with no remainder. The last non-zero
%% remainder (3) is our gcd. Here it is in general:

%%     a/b gives a remainder of r
%%     b/r gives a remainder of s
%%     r/s gives a remainder of t
%%     ...
%%     w/x gives a remainder of y
%%     x/y gives no remainder

%% In this case, y is the gcd of a and b. If the first step produced
%% no remainder, then b (the lesser of the two numbers) is the gcd.
gcd(A,B) when A >= B->
    case (A rem B) >= 1 of
	true ->
	    gcd(B, A rem B);
	false ->
	    B
    end;
gcd(A,B) ->
    gcd(B,A).

%% P33 (*) Determine whether two positive integer numbers are
%% coprime. Two numbers are coprime if their greatest common divisor
%% equals 1.
%% Example:
%% * (coprime 35 64)
%% T
coprime(A,B) when A > 0 andalso B > 0 ->
    case gcd(A,B) == 1 of 
	true ->
	    true;
	false ->
	    false
    end;
coprime(_A,_B) ->
    not_possible.

%% P34 (**) Calculate Euler's totient function phi(m).
%% Euler's so-called totient function phi(m) is defined as the number of
%% positive integers r (1 <= r < m) that are coprime to m.
%% Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case:
%% phi(1) = 1.
%% * (totient-phi 10)
%% 4
%% Find out what the value of phi(m) is if m is a prime
%% number. Euler's totient function plays an important role in one of
%% the most widely used public key cryptography methods (RSA). In this
%% exercise you should use the most primitive method to calculate this
%% function (there are smarter ways that shall discuss later).
phi(M) ->
    length([ X || X <-  lists:seq(1,M-1), coprime(M,X) == true ]).


%% P35 (**) Determine the prime factors of a given positive integer.
%% Construct a flat list containing the prime factors in ascending order.
%% Example:
%%  * (prime-factors 315)
%% What you do in this method is you start with the first
%% prime, which is 2 and ask yourself whether the number is divisible
%% by it. If it is, use short division to divide it by 2, and then
%% continue with the answer you get from the division and ask yourself
%% if that is still divisible by 2. If it is, divide that by
%% 2. Eventually you will get an answer that is not divisible by 2, so
%% then you ask yourself whether it is divisible by 3, and then
%% continue on in this way with 5, 7, and the rest of the primes until
%% you get an answer that is prime. Then the numbers along the side
%% and the final answer will be the prime factorization written in the
%% proper order. Here is how we would factor 60 using this method.
%%  60 = 2 * 2 * 3 * 5

prime_factors(N) when N =< 2 ->
    0;
prime_factors(N) when N > 2->
    P = list_of_prime(2,N),
    prime_factors(N,N,P,[]).

prime_factors(_N,_Next,[],Acc) ->
    Acc;
prime_factors(N,Next,[H|T],Acc) ->
    case lists:foldl(fun(X,Prod) -> X * Prod end,1,Acc) == N of
	true ->
	    lists:reverse(Acc);
	false ->
	    case Next rem H == 0 of
		true ->
		    prime_factors(N,Next div H,[H|T],[H|Acc]);
		false ->
		    prime_factors(N,Next,T,Acc)
	    end
    end.

%% P36 (**) Determine the prime factors of a given positive integer (2).
%% Construct a list containing the prime factors and their multiplicity.
%% Example:
%% * (prime-factors-mult 315) 
%% ((3 2) (5 1) (7 1))
%% Enjoyed implementing it.
prime_factors_mult(N)->
    [H|T] = prime_factors(N),
    prime_factors_mult(H,T,1,[]).

prime_factors_mult(H,[],1,[]) ->
    [[H,1]];
prime_factors_mult(H,[H1|[]],TempAcc,Acc) when H == H1 ->
    lists:reverse([[H,TempAcc+1]|Acc]);
prime_factors_mult(H,[H1|[]],TempAcc,Acc) ->
    lists:reverse([[H1,1] |[[H,TempAcc]|Acc]]);
					       
prime_factors_mult(H,[H1|T1],TempAcc,Acc) when H == H1 ->
    prime_factors_mult(H,T1,TempAcc+1,Acc);
prime_factors_mult(H,[H1|T1],TempAcc,Acc) ->
    prime_factors_mult(H1,T1,1,[[H,TempAcc]|Acc]).

%% P37 (**) Calculate Euler's totient function phi(m) (improved).  
%% See problem P34 for the definition of Euler's totient function. If
%% the list of the prime factors of a number m is known in the form of
%% problem P36 then the function phi(m) can be efficiently calculated
%% as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime
%% factors (and their multiplicities) of a given number m. Then phi(m)
%% can be calculated with the following
%% formula:
%% phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 -
%% 1) * p3 ** (m3 - 1) + ...  
%% Note that a ** b stands for the b'th power of a.
%% phi_imp Result is not equal to phi(N) *****CHECK IT******

phi_imp(N)->
    PFM = prime_factors_mult(N),
    lists:foldl(fun([P,M],Acc) -> ((P-1) * math:pow(P,(M-1))) + Acc end, 0,PFM).

%% P39 A list of prime numbers.  Given a range of integers by its
%% lower and upper limit, construct a list of all prime numbers in
%% that range.
list_of_prime(A,B)->
    [ X || X <- lists:seq(A,B), is_prime(X) == true ]. 

%% P40 (**) Goldbach's conjecture.
%% Goldbach's conjecture says that every positive even number greater
%% than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is
%% one of the most famous facts in number theory that has not been proved
%% to be correct in the general case. It has been numerically confirmed
%% up to very large numbers (much larger than we can go with our Prolog
%% system). Write a predicate to find the two prime numbers that sum up
%% to a given even integer.
%% Example:
%% * (goldbach 28)
%% (5 23)
goldbach(N) when N rem 2 /= 0 ->
    should_be_even_number;
goldbach(N) ->
    P = list_of_prime(2,N),
    goldbach_loop(N,P).

goldbach_loop(_Num,[_H|[]])->
    ok;

goldbach_loop(Num,[H|T])->
    goldbach_loop(Num,H,T,T).

goldbach_loop(Num,_N,[],Tail) ->
    goldbach_loop(Num,Tail);

goldbach_loop(Num,N,[_H|_T],_Tail) when N+N == Num ->
    [N,N];
goldbach_loop(Num,N,[H|_T],_Tail) when N+H == Num ->
    [N,H];
goldbach_loop(Num,N,[_H|T],Tail)-> 
    goldbach_loop(Num,N,T,Tail).


%% P41 (**) A list of Goldbach compositions.
%% Given a range of integers by its lower and upper limit, print a list of all
%% even numbers and their Goldbach composition.
%% Example:
%% * (goldbach-list 9 20)
%% 10 = 3 + 7
%% 12 = 5 + 7
%% 14 = 3 + 11
%% 16 = 3 + 13
%% 18 = 5 + 13
%% 20 = 3 + 17
%% In most cases, if an even number is written as the sum of two prime
%% numbers, one of them is very small. Very rarely, the primes are both
%% bigger than say 50. Try to find out how many such cases there are in the
%% range 2..3000.
%% Example (for a print limit of 50):
%% * (goldbach-list 1 2000 50)
%% 992 = 73 + 919
%% 1382 = 61 + 1321
%% 1856 = 67 + 1789
%% 1928 = 61 + 1867

goldbach_list(_A,0) ->
    should_be_more_than_2;
goldbach_list(0,_A) ->
    should_be_more_than_2;
goldbach_list(A,B) when A > B ->
    goldbach_list(B,A);
goldbach_list(A,B)->
    Nums = [X || X <- lists:seq(A,B), X rem 2 == 0],
    goldbach_list(Nums).
goldbach_list([])->
    ok;
goldbach_list([H|T])->
    [P1,P2]=goldbach(H),
    io:format("~p = ~p + ~p~n",[H,P1,P2]),
    goldbach_list(T).


%% Unit Tests
goldbach_simple_test() ->
    [3,7] = goldbach(10).
goldbach_odd_test() ->
    should_be_even_number = goldbach(7).

is_prime_true_test()->
    true = is_prime(7).
is_prime_false_test()->
    false = is_prime(10).

gcd_simple_test()->
    10 = gcd(10,20).

coprime_true_test()->
    true = coprime(3,7).
coprime_false_test()->
    false = coprime(30,20).

phi_test() ->
    4 = phi(10).
