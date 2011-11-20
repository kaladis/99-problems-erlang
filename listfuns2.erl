-module(listfuns2).
-export([ insert_at/3, range/2, rnd_select/2, lotto_select/2,
	 rnd_permuu/1, comb2/1, sort/1, lsort/1 ]).

%% P21 (*) Insert an element at a given position into a list.
%% Example:
%% (insert-at 'alfa '(a b c d) 2)
%% (A ALFA B C D)
insert_at([],_C,_N)->
    [];
insert_at(_,_,0) ->
    [];
insert_at(L,C,N) ->
    insert_at(L,C,N,1,[]).

insert_at([H|T],C,N,Count,Acc) when Count < N ->
    insert_at(T,C,N,Count+1,[H|Acc]);
insert_at(L,C,N,Count,Acc) when Count =< N ->
    lists:reverse(Acc) ++ [C] ++ L.

%% P22 (*) Create a list containing all integers within a given range.
%% If first argument is smaller than second, produce a list in
%% decreasing order.
%% Example:
%% * (range 4 9)
%%  (4 5 6 7 8 9)
range(Start,End) when Start < End ->
    lists:reverse(range(Start,End,[]));
range(Start,End) when Start == End ->
    [];
range(Start,End) ->
    range(End,Start,[]). % this is a trick; dont reverse it 

range(Start,End,Acc) when Start < End ->
    range(Start+1,End,[Start|Acc]);
range(Start,End,Acc) when Start >= End ->
    [Start|Acc].

%% P23 (**) Extract a given number of randomly selected elements from
%% a list.  The selected items shall be returned in a list.
%% Example:
%% * (rnd-select '(a b c d e f g h) 3)
%%  (E D A)
%% Hint: Use the built-in random number generator and the result of
%% problem P20.
rnd_select([],_N) ->
    [];
rnd_select(_,0) ->
    [];
rnd_select(L,N) ->
    RndElements  = rnd_elements(length(L),N),
    rnd_select(L,RndElements,[]).

rnd_select(_L,[],Acc) ->
    Acc;
rnd_select(L,[H|T],Acc)->
    rnd_select(L,T,[lists:nth(H,L)|Acc]).

rnd_elements(Max,HowMany) when HowMany > Max ->
    [];
rnd_elements(Max,HowMany) ->
    rnd_elements(Max,HowMany,[]).

rnd_elements(_Max,0,Acc) ->
    Acc;
rnd_elements(Max,HowMany,Acc) ->
    R = random:uniform(Max),
    case inlist(R,Acc) of
	true ->
	    rnd_elements(Max,HowMany,Acc);
	false ->
	    rnd_elements(Max,HowMany-1,[R|Acc])
    end.

inlist(_N,[]) ->
    false;
inlist(N,[H|_T]) when H == N ->
    true;
inlist(N,[_H|T]) ->
    inlist(N,T).

%% P24 (*) Lotto: Draw N different random numbers from the set 1..M.
%% The selected numbers shall be returned in a list.
%% Example:
%% * (lotto-select 6 49)
%% (23 1 17 33 21 37)
%% Hint: Combine the solutions of problems P22 and P23.
lotto_select(N,End) ->
    %% when all the elements are Ascci numbers it return a string.
    rnd_select(range(0,End),N).

%% P25 (*) Generate a random permutation of the elements of a list.
%% Example:
%% * (rnd-permu '(a b c d e f))
%%  (B A D C E F)
%%  Hint: Use the solution of problem P23.
rnd_permu([]) ->
    [];
rnd_permu(L) ->
    rnd_select(L,length(L)).


%% P26 (**) Generate the combinations of K distinct objects chosen
%% from the N elements of a list In how many ways can a committee of 3
%% be chosen from a group of 12 people? We all know that there are
%% C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial
%% coefficients). For pure mathematicians, this result may be
%% great. But we want to really generate all the possibilities ina
%% list.
%% Example:
%% * (combination 3 '(a b c d e f))
%% ((A B C) (A B D) (A B E) ...) 
combination([H|T]) ->
    combination(H,T,[]).
combination(_C,[],Acc)->
    Acc;
combination(C,L,Acc)->
    [H|T] = L,
    Acc1 = [[C,X] || X <- L],
    combination(H,T,Acc1 ++ Acc).

%% P28 (**) Sorting a list of lists according to length of sublists a)
%% We suppose that a list contains elements that are lists
%% themselves. The objective is to sort the elements of this list
%% according to their length. E.g.  short lists first, longer lists
%% later, or vice versa.
%% Example:
%%  * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
%% ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))

%% simple sort 
sort([])->
    [];
sort([H|T])->
    sort(H,T,[],[]).
sort(P,[],Left,Right) ->
    sort(Left) ++ [P] ++ sort(Right);
sort(P,[H|T],Left,Right) when H >= P ->
    sort(P,T,Left,[H|Right]);
sort(P,[H|T],Left,Right) ->
    sort(P,T,[H|Left],Right).

%% Length sort
lsort([])->
    [];
lsort([H|T]) ->
    lsort(H,T,[],[]).

lsort(P,[],Left,Right) ->
    lsort(Left) ++ [P] ++ lsort(Right);
lsort(P,[H|T],Left,Right) when length(H) >= length(P) ->
    lsort(P,T,Left,[H|Right]);
lsort(P,[H|T],Left,Right) ->
    lsort(P,T,[H|Left],Right).

%% Example:
%% * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
%% ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
%% b) Again, we suppose that a list contains elements that are lists
%% themselves. But this time the objective is to sort the elements of
%% this list according to their length frequency; i.e., in the
%% default, where sorting is done ascendingly, lists with rare lengths
%% are placed first, others with a more frequent length come later.
%% Example:
%% * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
%% ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
%% Note that in the above example, the first two lists in the result
%% have length 4 and 1, both lengths appear just once. The third and
%% forth list have length 3 which appears twice (there are two list of
%% this length). And finally, the last three lists have length 2. This
%% is the most frequent length.
%% attach_length([],Acc) ->
%%     Acc;
%% attach_length([H|T],Acc) ->
%%     attach_length(T,[ {length(H),H} |Acc]).

%% lfsort([])->
%%     [];
%% lfsort(L) ->
%%     [H|T] = attach_length(L,[]),
%%     lfsort(H,T,[],[]).

%% lfsort({PLen,P},[],Left,Right) ->
%%     lfsort(Left) ++ [{PLen,P}] ++ lfsort(Right);
%% lfsort({PLen,P},[{Len,H}|T],Left,Right ) when Len > PLen ->
%%     lfsort({PLen,P},T,Left,[{Len,H}|Right]);
%% lfsort({PLen,P},[{Len,H}|T],Left,Right) ->
%%     lfsort({PLen,P},T,[{Len,H}|Left],Right).

