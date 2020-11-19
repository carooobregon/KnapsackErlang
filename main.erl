-module(main).
%-export([test/0, solve/2, solve/3, solveConcurrent/3]).
-export([start/0,evaluate/2,rndSolution/1,mutate/2,solve/2, solve/3, solveConcurrent/3,getInstance/1,test/0]).
% Do not forget to include the full name and student ID of the team members.
% ============================================
% Name: Ana Carolina Arellano 
% Student Id: A01650945
% Name: Gabriela Corona Garza
% Student Id: A01282529
% Name: Carolina Obregon
% Student Id: A01251983
% Name: Ricardo Ramirez
% Student Id: A01338143
% ============================================

% Evaluation of solutions
%
% evaluate/2 Returns a tuple with the sum of the weights and profits of the items in the knapsack.
% This function must be implemented by using tail recursion, to avoid problems with large instances.
% ============================================
% Example:
% 	project:evaluate([true, false, false, false, true], {12, [{5, 10}, {4, 7}, {8, 1}, {3, 5}, {7, 10}]}).
% Returns {12, 20}, which means that, the cumulative weight of the items in the knapsack is 12 units,
% and its profit is 20 units.
% ============================================

evaluate(_, {_, []}) -> 0;
evaluate([SolH | SolT], {Capacity, [{W, P} | T]}) -> if
    SolH and (W =< Capacity) -> P + evaluate(SolT, {Capacity - W, T});
    true -> evaluate(SolT, {Capacity, T})
  end.

% Generation of random solutions
%
% rndSolution/1 generates a random solution for instances of n items.
% ============================================
% Example:
% 	project:rndSolution(5).
% Returns a random solution for an instance with five items.
% ============================================
rndSolution(N) -> if
		N > 0 -> [(rand:uniform() > 0.5)] ++ rndSolution(N - 1);
		true -> []
	end.

% Mutation of solutions
%
% mutate/2 mutates a solution with a given probability.
% ============================================
% Example:
% 	project:mutate([true, false, false, false, true], 0.1).
% Returns a new solution which is slightly different from the one given as argument (the elements
% are changed with a probability of 0.1).
% ============================================
mutate([], _) -> [];
mutate([H | T], MRate) -> Temp = rand:uniform(), 
    if
      Temp =< MRate -> [(not H) | mutate(T, MRate)];
      true -> [H | mutate(T, MRate)]
    end.

% Instances
%
% getInstance/1 returns an instance of the knapsack problem. 
% Each instance is defined by a tuple {Capacity, Items}, where Capacity is an integer with the
% knapsack capacity and Items is a list of items. In this list of items, each item is represented
% as a tuple {Weight, Profit}, where Weight indicates the weight of the item and Profit, its profit.
% ============================================
% Example:
% 	project:getInstance(ks300).
% Returns the instance with identifier ks300. 
% ============================================
getInstance(test1) -> "hola";
getInstance(test) -> {12, [{5, 10}, {4, 7}, {8, 1}, {3, 5}, {7, 10}]};
getInstance(ks45) -> {58181, [{4990, 1945}, {1142, 321}, {7390, 2945}, {10372, 4136}, {3114, 1107}, {2744, 1022}, {3102, 1101}, {7280, 2890}, {112738, 47019}, {3960, 1530}, {8564, 3432}, {5630, 2165}, {4506, 1703}, {3112, 1106}, {1240, 370}, {2014, 657}, {2624, 962}, {3020, 1060}, {2310, 805}, {2078, 689}, {3926, 1513}, {9656, 3878}, {32708, 13504}, {4830, 1865}, {2034, 667}, {4766, 1833}, {40006, 16553}, {3422, 1261}, {6686, 2593}, {3240, 1170}, {2288, 794}, {2042, 671}, {18142, 7421}, {14718, 6009}, {4634, 1767}, {6744, 2622}, {2362, 831}, {2102, 701}, {12944, 5222}, {7872, 3086}, {2500, 900}, {7942, 3121}, {2958, 1029}, {126010, 52555}, {1278, 389}]};
getInstance(ks50) -> {341045, [{4912, 1906}, {99732, 41516}, {56554, 23527}, {1818, 559}, {108372, 45136}, {6750, 2625}, {1484, 492}, {3072, 1086}, {13532, 5516}, {12050, 4875}, {18440, 7570}, {10972, 4436}, {1940, 620}, {122094, 50897}, {5558, 2129}, {10630, 4265}, {2112, 706}, {6942, 2721}, {39888, 16494}, {71276, 29688}, {8466, 3383}, {5662, 2181}, {231302, 96601}, {4690, 1795}, {18324, 7512}, {3384, 1242}, {7278, 2889}, {5566, 2133}, {706, 103}, {10992, 4446}, {27552, 11326}, {7548, 3024}, {934, 217}, {32038, 13269}, {1062, 281}, {184848, 77174}, {2604, 952}, {37644, 15572}, {1832, 566}, {10306, 4103}, {1126, 313}, {34886, 14393}, {3526, 1313}, {1196, 348}, {1338, 419}, {992, 246}, {1390, 445}, {56804, 23552}, {56804, 23552}, {634, 67}]}.

% Sequential solver 
%
% solve/2 solves an instance by testing n different solutions.
% ============================================
% Example:
% 	project:solve(knapsack:getInstance(ks45), 1000000).
% Tries a million of solutions for instance with identifier ks45 and returns a tuple of three
% elements {Solution, Weight, Profit}, where Solution contains the actual solution found and
% Weight and Profit indicate the total weight and profit packed within the knapsack, respectively.
% ============================================
solve({Capacity, Elem}, Iterations) -> 
		EvalArr = rndSolution(length(Elem)),
		Evaluate = evaluate(EvalArr, { Capacity, Elem } ),
		if
			Iterations > 0 ->  CandArr = mutate(EvalArr, 0.5),
			Candidate = evaluate(CandArr,{Capacity,Elem}),
			
				if
					tl (Candidate) >  tl (Evaluate) -> solve({CandArr,Candidate}, Iterations-1);
					true -> solve({EvalArr,Evaluate}, Iterations-1)
				end;
		true -> [Elem ++ Evaluate]
		end.

listener({ _ , _} , _, 0) -> "Finished"
listener({Capacity, Elem}, Iterations, CPU) ->
	MaxProfit = 0, %%  como buscar el mejor profit de la tuple y receibir mensajes para comparar
	recieve
		SolArr -> Sol
		if 
			SolArr > MaxProfit -> MaxProfit = SolArr,
			true -> 
		listener( {Capacity,Elem} , Iteraciones, CPU -1)
	end.

divide( {_, _}, _, 0 )-> "Finished"; 
divide( {Capacity, Elem}, Iteraciones,CPU)-> 
	spawn(solve, {Capacity,Elem},Iteraciones),
	divide( {Capacity, Elem}, Iteraciones,CPU - 1).

% Concurrent solver 
%
% solveConcurrent/3 solves an instance by testing n different solutions on m different processes.
% ============================================
% Example:
% 	project:solveConcurrent(knapsack:getInstance(ks45), 1000000, 4).
% Tries a million of solutions for instance with identifier ks45 by splitting the work into
% four concurrent processes and returns a tuple of three elements {Solution, Weight, Profit},
% where Solution contains the actual solution found and Weight and Profit indicate the total
% weight and profit packed within the knapsack, respectively.
% ============================================
solveConcurrent({Capacity, Elem}, Iterations, CPU) -> 
	Pid = spawn(listener ,{Capacity,Elem},Iterations,CPU))   % hacer con spawn
	register(pidListener, Pid) % pidListener ! para hacer referencia a el proceso
	%regster (miproceso,self()) regresa a la terminal
	Frac = trunc(Iteraciones / CPU),
	divide({Capacity, Elem}, Frac,CPU);



%Pid = spawn(fun main:solve/3)
%	recieve
 %   	if
	%	    CPU > 0 ->  Pid ! solve([Capacity | Elem], Iterations/CPU);
	%				solveConcurrent({Capacity,Elem}, Iterations , CPU-1 );
   %     true -> [Capacity ++ Elem ]
		%		end.
%	ok.


% === Test cases (internal use) ===
% Use this code to test if your codes are working as expected. 
test() ->
	io:format("~p~n", [evaluate([true, false, false, false, true], {12, [{5, 10}, {4, 7}, {8, 1}, {3, 5}, {7, 10}]})]), % {12,20}
	io:format("~p~n", [evaluate([false, false, true], {7, [{1, 10}, {4, 3}, {12, 2}]})]), % {0,0}
	io:format("~p~n", [evaluate([true, true, true], {10, [{1, 10}, {10, 13}, {4, 2}]})]), % {5,12}
	io:format("~p~n", [mutate([true, true, true], 1)]), % [false,false,false]
	io:format("~p~n", [mutate([true, true, true], 0)]). % [true,true,true]

% === Evaluation ===
%
% solve/3 evaluates the running time of the solvers on specific instances. If properly implemented,
% the concurrent solver should be faster than the sequential. However, the quality of the solutions
% should be similar for both solvers.
% ============================================
% Example:
% 	project:solve(ks1000, 100000, 4).
% Solves the instance with identifier ks1000 by using (1) the sequential solver with 100000 solutions 
% and (2) the concurrent solver with also 100000 solutions and four processors. Please be aware that
% this test may take some time to finish.

solve(Name, Solutions, Processors) ->	
	Instance = getInstance(Name),	
	{T1, {_, _, P1}} = timer:tc(projectSolution, solve, [Instance, Solutions]),		
	io:format("Instance: ~p.~n", [Name]),
	io:format("Sequential approach): ~p seconds (profit = ~p).~n", [T1 / 1000000, P1]),	
	{T2, {_, _, P2}} = timer:tc(projectSolution, solveConcurrent, [Instance, Solutions, Processors]),		
	io:format("Concurrent approach): ~p seconds (profit = ~p).~n", [T2 / 1000000, P2]),
	ok.



start() ->
	Instance = getInstance(test),
  Sol = rndSolution(5),
	io:fwrite("~p~n", [Sol]),
  MSol = mutate(Sol, 0.5),
  io:fwrite("~p~n", [MSol]),
  Evaluation = evaluate(MSol, Instance),
  io:fwrite("~p~n", [Evaluation]),
	io:fwrite("~p~n", solve(getInstance(ks45), 1000000)).
	%io:fwrite(evaluate([true, false, false, false, true], {12, [{5, 10}, {4, 7}, {8, 1}, {3, 5}, {7, 10}]})).

	