%%%-------------------------------------------------------------------
%%% @author Nicolas Dutly & Mevlüt Tatli
%%% @doc A distributed PSO implementation, implementing a global topology.
%%%
%%% Usage: call pso_global_topo:start/8 with the parameters described in
%%% the function header.
%%%
%%%
%%% Usage example: pso_global_topo:start(100,2,2,0.75,2,-5,5,500)
%%% starts the PSO algorithm with 100 particles, social and cognitive weight
%%% factors of 2, particle inertia of 0.75, optimizing a two dimensional problem
%%% described in the function 'cost_function/1'. The search space is [-5,5] and the
%%% number of iterations 500.
%%%
%%% @end
%%% Created : 13. nov. 2018 15:52
%%%-------------------------------------------------------------------
-module(pso_global_topo).
-author("Nicolas Dutly & Mevlüt Tatli").

%% API
-export([start/8, init_particle/7]).

%%%
%%% Initializes N particles and starts
%%% the optimization process.
%%%
%%% @param N: Number of particles
%%% @param W_s: Social weight factor
%%% @param W_c: Cognitive weight factor
%%% @param Phi: Momentum weight
%%% @param Dim: The dimension of the search space
%%% @param Lo: lower search space bound
%%% @param Hi: upper search space bound
%%% @param Epochs: Number of iterations
%%%
%%%

start(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs) ->
  TimeStart = os:system_time(),
  init(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs),
  TimeElapsed = os:system_time() - TimeStart,
  io:format("~n~nElapsed time: ~p", [TimeElapsed]).

%% Initializes the PSO algorithm, spawning N nodes.
init(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs) ->
  {ok, [_ | Ns]} = file:consult('enodes.conf'),
  NbOfTedaNodes = length(Ns),
  NbOfParticlesPerNode = round(math:ceil(N / NbOfTedaNodes)),
  %Spawn a fraction of the total amount of particles on each erlang node available in the TEDA environment
  P_listTMP = [spawn(Id, ?MODULE, init_particle, [Dim, Lo, Hi, W_s, W_c, Phi, self()]) || _ <- lists:seq(1, NbOfParticlesPerNode), Id <- Ns],
  io:format("~p", [P_listTMP]),
  P_list = lists:flatten(P_listTMP),
  GMin = wait_for_particle(N, []),
  master_optimize(Epochs, GMin, N, P_list).


%% Initializes particles, given candidate search space and hyper-parameters.
%% Notifies the master process once the initialization is done.
init_particle(Dim, Lo, Hi, W_s, W_c, Phi, MPid) ->
  Init_pos = [Lo + rand:uniform() * (Hi - Lo) || _ <- lists:seq(1, Dim)],
  Init_vel = [-abs(Hi - Lo) + rand:uniform() * abs(Hi - Lo) || _ <- lists:seq(1, Dim)],
  MPid ! {done, Init_pos},
  LocalMin = Init_pos,
  receive
    {start, GlobalMin} -> optimize(Init_pos, Init_vel, LocalMin, GlobalMin, W_s, W_c, Phi, MPid, Lo, Hi)
  end.

%% Wait for particles to finish calculations and collect results.
wait_for_particle(0, GMin) ->
  GMin;
wait_for_particle(N, []) ->
  receive
    {done, Val} ->
      wait_for_particle(N - 1, Val)

  end;
%% If the master receives a better position for the global minimum, it will update
%% the swarm minimum.
wait_for_particle(N, GMin) ->
  receive
    {done, Val} ->
      New = cost_function(Val),
      Old = cost_function(GMin),
      if
        New < Old -> wait_for_particle(N - 1, Val);
        true -> wait_for_particle(N - 1, GMin)
      end
  end.


%% The cost function describing the optimization problem. We included the functions
%% that were used in our report for reproducibility.
cost_function(L) ->
  X = lists:nth(1, L),
  Y = lists:nth(2, L),
  %%(Schaffer N.2)[(0,0)]=0
  %0.5 + (math:pow(math:sin(math:pow(X, 2) - math:pow(Y, 2)), 2) - 0.5) / math:pow(1 + 0.001 * (math:pow(X, 2) + math:pow(Y, 2)), 2).

  %%(Levi N.13)[(1,1)]=0
  %math:pow(math:sin(3*math:pi()*X),2)+math:pow(X-1,2)*(1+math:pow(math:sin(3*math:pi()*Y),2))+math:pow(Y-1,2)*(1+math:pow(math:sin(2*math:pi()*Y),2)).

  %%(Goldstein-Price function)[(0,-1)]=3
  %(1+math:pow(X+Y+1,2)*(19-14*X+3*math:pow(X,2)-14*Y+6*X*Y+3*math:pow(Y,2)))*(30+math:pow(2*X-3*Y,2)*(18-32*X+12*math:pow(X,2)+48*Y-36*X*Y+27*math:pow(Y,2))).


  % ackley's function (minima: (0,0))
  %-20 * math:exp(-0.2 * math:sqrt(0.5 * (math:pow(X, 2) + math:pow(Y, 2)))) - math:exp(0.5 * (math:cos(2 * math:pi() * X) + math:cos(2 * math:pi() * Y))) + math:exp(1) + 20.
  %% Himmelblau's function [minima:(3,2), (3.584,-1.848), (-2.805,3.1313), (-3.779, -3.38)]=0
  math:pow(math:pow(X, 2) + Y - 11, 2) + math:pow((X + math:pow(Y, 2) - 7), 2).

%% The particle optimization procedure, particles will update their respective
%% location and velocity using this function.
optimize(InitPos, InitVel, LocalMin, GlobalMin, W_s, W_c, Phi, MPid, Lo, Hi) ->
  R_p = rand:uniform(),
  R_g = rand:uniform(),
  T1 = [Phi * X || X <- InitVel],
  T2 = lists:zipwith(fun(X, Y) -> W_c * R_p * (X - Y) end, LocalMin, InitPos),
  T3 = lists:zipwith(fun(X, Y) -> W_s * R_g * (X - Y) end, GlobalMin, InitPos),
  NewVel = lists:zipwith3(fun(X, Y, Z) -> X + Y + Z end, T1, T2, T3),
  NewPos = lists:zipwith(fun(X, Y) -> X + Y end, NewVel, InitPos),
  L = [X || X <- NewPos, (X < Lo) or (X > Hi)],
  Len = length(L),
  if
    Len =/= 0 -> init_particle(length(InitPos), Lo, Hi, W_s, W_c, Phi, MPid);
    true -> ok
  end,
  MPid ! {done, NewPos},
  receive
    {start, NewGlobalMin} ->
      optimize(NewPos, NewVel, LocalMin, NewGlobalMin, W_s, W_c, Phi, MPid, Lo, Hi)
  end.

%% The master optimization loop used to collect particle results after
%% every iteration and if needed updating the swarm optimum.
master_optimize(0, SwarmMin, _, _) ->
  {H, M, S} = time(),

  io:format("~n==============================================================================~n"),
  io:format("[~2..0b:~2..0b:~2..0b] Optimization completed. ~nSwarm optimum location: ~p~nSwarm optimum value: ~p~n", [H, M, S, SwarmMin, cost_function(SwarmMin)]),
  io:format("================================================================================"),

  SwarmMin;
master_optimize(N, SwamMin, NbOfParticles, PList) ->
  %io:format("~n---------------EPOCH[~p]----------------------~n", [N]),
  [Pid ! {start, SwamMin} || Pid <- PList],
  %%io:format("start signal sent to particles~n"),
  %io:format("waiting for particles...~n"),
  GMin = wait_for_particle(NbOfParticles, []),
  NewMinVal = cost_function(GMin),
  OldMinVal = cost_function(SwamMin),
  if
    NewMinVal < OldMinVal ->
      %io:format("Found new swarm min: ~p~n", [cost_function(GMin)]),
      file:write_file("schaffer.csv", io_lib:fwrite("~p,~p~n", [500 - N, NewMinVal]), [append]),
      master_optimize(N - 1, GMin, NbOfParticles, PList);
    NewMinVal >= OldMinVal ->
      master_optimize(N - 1, SwamMin, NbOfParticles, PList);
    true -> ok
  end.
