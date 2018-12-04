%%%-------------------------------------------------------------------
%%% @author Nicolas Dutly & Mevlüt Tatli
%%% @doc A distributed PSO implementation
%%% TODO add doc
%%%
%%% @end
%%% Created : 13. nov. 2018 15:52
%%%-------------------------------------------------------------------
-module(pso).
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

start(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs) ->
  TimeStart = os:system_time(),
  init(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs),
  TimeElapsed = os:system_time() - TimeStart,
  io:format("~n~nElapsed time: ~p", [TimeElapsed]).

init(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs) ->
  %io:format("Spawning and initializing ~p particles...~n", [N]),

  P_list = [spawn(?MODULE, init_particle, [Dim, Lo, Hi, W_s, W_c, Phi, self()]) || _ <- lists:seq(1, N)],
  GMin = wait_for_particle(N, []),
  %io:format("~nInitialization done.~n"),
  %io:format("Initial swarm min: ~p~n", [cost_function(GMin)]),
  master_optimize(Epochs, GMin, N, P_list).


%% Particle initialization
init_particle(Dim, Lo, Hi, W_s, W_c, Phi, MPid) ->
  Init_pos = [Lo + rand:uniform() * (Hi - Lo) || _ <- lists:seq(1, Dim)],
  Init_vel = [-abs(Hi - Lo) + rand:uniform() * abs(Hi - Lo) || _ <- lists:seq(1, Dim)],
  MPid ! {done, Init_pos},
  LocalMin = Init_pos,
  receive
    {start, GlobalMin} -> optimize(Init_pos, Init_vel, LocalMin, GlobalMin, W_s, W_c, Phi, MPid)
  end.

%% Wait for particles to finish calculations and collect results.
wait_for_particle(0, GMin) ->
  %io:format("Done waiting for particles~n"),
  GMin;
wait_for_particle(N, []) ->
  receive
    {done, Val} ->
      wait_for_particle(N - 1, Val)

  end;
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



cost_function(L) ->
  X = lists:nth(1, L),
  Y = lists:nth(2, L),
  % ackley's function (minima: (0,0))
  -20 * math:exp(-0.2 * math:sqrt(0.5 * (math:pow(X, 2) + math:pow(Y, 2)))) - math:exp(0.5 * (math:cos(2 * math:pi() * X) + math:cos(2 * math:pi() * Y))) + 0.5772156649 + 20.
%% The function we want to minimize (Himmelblau's function)
%% The minimas are: (3,2), (3.584,-1.848), (-2.805,3.1313), (-3.779, -3.38)
%% math:pow(math:pow(X,2)+Y-11,2)+math:pow((X+math:pow(Y,2)-7),2).

%% Particle optimization procedure
optimize(InitPos, InitVel, LocalMin, GlobalMin, W_s, W_c, Phi, MPid) ->
  % recursive dimensions
  %io:format("| [~p]  Updating particle position...~n", [self()]),
  R_p = rand:uniform(),
  R_g = rand:uniform(),
  T1 = [Phi * X || X <- InitVel],
  T2 = lists:zipwith(fun(X, Y) -> W_c * R_p * (X - Y) end, LocalMin, InitPos),
  T3 = lists:zipwith(fun(X, Y) -> W_s * R_g * (X - Y) end, GlobalMin, InitPos),
  NewVel = lists:zipwith3(fun(X, Y, Z) -> X + Y + Z end, T1, T2, T3),
  NewPos = lists:zipwith(fun(X, Y) -> X + Y end, NewVel, InitPos),
  MPid ! {done, NewPos},
  receive
    {start, NewGlobalMin} ->
      optimize(NewPos, NewVel, LocalMin, NewGlobalMin, W_s, W_c, Phi, MPid)
  end.

%% The master loop optimization
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
      master_optimize(N - 1, GMin, NbOfParticles, PList);
    NewMinVal >= OldMinVal ->
      master_optimize(N - 1, SwamMin, NbOfParticles, PList);
    true -> ok
  end.
