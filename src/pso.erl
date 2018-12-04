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
-export([start/8, init_particle/7, init/9]).

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
  io:format("[INFO] PSO optimization started with 5x~p particles. Params: ~n
  W_s -> ~p
  W_c -> ~p
  Phi -> ~p
  Dim -> ~p
  Lo -> ~p
  Hi -> ~p
  Epochs -> ~p~n", [N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs]),
  TimeStart = os:system_time(),
  Min = init_groups(5, N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs),
  TimeElapsed = os:system_time() - TimeStart,

  {H, M, S} = time(),
  io:format("~n==============================================================================~n"),
  io:format("[~2..0b:~2..0b:~2..0b] Optimization completed. ~nSwarm optimum location: ~p~nSwarm optimum value: ~p~n", [H, M, S, Min, cost_function(Min)]),
  io:format("================================================================================"),
  io:format("~n~nElapsed time: ~p", [TimeElapsed]).

init(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs, MasterPID) ->
  P_list = [spawn(?MODULE, init_particle, [Dim, Lo, Hi, W_s, W_c, Phi, self()]) || _ <- lists:seq(1, N)],
  GMin = wait_for_particle(N, []),
  Sol = master_optimize(Epochs, GMin, N, P_list),
  MasterPID ! {group_done, Sol}.


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
  %%(Himmelblau's function)
  %% The minimas are: (3,2), (3.584,-1.848), (-2.805,3.1313), (-3.779, -3.38)
  %math:pow(math:pow(X, 2) + Y - 11, 2) + math:pow((X + math:pow(Y, 2) - 7), 2),
  %% Ackley's function
  %% The minimas are (0,0)
  -20 * math:exp(-0.2 * math:sqrt(0.5 * (math:pow(X, 2) + math:pow(Y, 2)))) - math:exp(0.5 * (math:cos(2 * math:pi() * X) + math:cos(2 * math:pi() * Y))) + 0.5772156649 + 20.
%% Particle optimization procedure
optimize(InitPos, InitVel, LocalMin, GlobalMin, W_s, W_c, Phi, MPid) ->
  % recursive dimensions
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
  SwarmMin;
master_optimize(N, SwamMin, NbOfParticles, PList) ->
  [Pid ! {start, SwamMin} || Pid <- PList],
  GMin = wait_for_particle(NbOfParticles, []),
  NewMinVal = cost_function(GMin),
  OldMinVal = cost_function(SwamMin),
  if
    NewMinVal < OldMinVal ->
      master_optimize(N - 1, GMin, NbOfParticles, PList);
    NewMinVal >= OldMinVal ->
      master_optimize(N - 1, SwamMin, NbOfParticles, PList);
    true -> ok
  end.

init_groups(GroupNb, N, WS, WC, Phi, Dim, Lo, Hi, Epochs) ->
  [spawn(?MODULE, init, [N, WS, WC, Phi, Dim, Lo, Hi, Epochs, self()]) || _ <- lists:seq(1, GroupNb)],
  waitForGroups(GroupNb, []).

waitForGroups(0, Val) ->
  Val;
waitForGroups(GroupNb, []) ->
  receive
    {group_done, Min} ->
      waitForGroups(GroupNb - 1, Min)
  end;
waitForGroups(GroupNb, OldMin) ->
  receive
    {group_done, Min} ->
      New = cost_function(Min),
      Old = cost_function(OldMin),
      if
        New < Old -> waitForGroups(GroupNb - 1, Min);
        true -> waitForGroups(GroupNb - 1, OldMin)
      end
  end.