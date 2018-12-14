%%%-------------------------------------------------------------------
%%% @author Nicolas Dutly & Mevlüt Tatli
%%% @doc A distributed PSO implementation, implementing a social topology.
%%%
%%% The social topology is implemented by assigning the particles to one
%%% of five independent groups.
%%%
%%% Usage: call pso_social_topo:start/8 with the parameters described in
%%% the function header.
%%%
%%%
%%% Usage example: pso_global_topo:start(100,2,2,0.75,2,-5,5,500)
%%% starts the PSO algorithm with 100 particles devided into 5 groups,
%%% social and cognitive weight factors of 2, particle inertia of 0.75,
%%% optimizing a two dimensional problem described in the function 'cost_function/1'.
%%% The search space is [-5,5] and the number of iterations 500.
%%% @end
%%% Created : 13. nov. 2018 15:52
%%%-------------------------------------------------------------------
-module(pso_social_topo).
-author("Nicolas Dutly & Mevlüt Tatli").

%% API
-export([start/8, init_particle/7, init/10]).

%%%
%%% Initializes N particles split into 5 independent particle groups
%%% and start optimization
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
  io:format("[INFO] PSO optimization started with ~p particles. Params: ~n
  W_s -> ~p
  W_c -> ~p
  Phi -> ~p
  Dim -> ~p
  Lo -> ~p
  Hi -> ~p
  Epochs -> ~p~n", [N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs]),
  Min = init_groups(5, round(N / 5), W_s, W_c, Phi, Dim, Lo, Hi, Epochs),

  {H, M, S} = time(),
  io:format("~n==============================================================================~n"),
  io:format("[~2..0b:~2..0b:~2..0b] Optimization completed. ~nSwarm optimum location: ~p~nSwarm optimum value: ~p~n", [H, M, S, Min, cost_function(Min)]),
  io:format("================================================================================"),

init(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs, MasterPID, TEDAList) ->
  NbOfTedaNodes = length(TEDAList),
  NbOfParticlesPerNode = round(math:ceil(N / NbOfTedaNodes)),
  %Spawn a fraction of the total amount of particles on each erlang node available in the TEDA environment
  P_listTMP = [spawn(Id, ?MODULE, init_particle, [Dim, Lo, Hi, W_s, W_c, Phi, self()]) || _ <- lists:seq(1, NbOfParticlesPerNode), Id <- TEDAList],
  P_list = lists:flatten(P_listTMP),
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

  %%(Schaffer N.2)[(0,0)]=0
  0.5 + (math:pow(math:sin(math:pow(X, 2) - math:pow(Y, 2)), 2) - 0.5) / math:pow(1 + 0.001 * (math:pow(X, 2) + math:pow(Y, 2)), 2).

%%(Levi N.13)[(1,1)]=0
%math:pow(math:sin(3*math:pi()*X),2)+math:pow(X-1,2)*(1+math:pow(math:sin(3*math:pi()*Y),2))+math:pow(Y-1,2)*(1+math:pow(math:sin(2*math:pi()*Y),2)).

%%(Goldstein-Price function)[(0,-1)]=3
%(1+math:pow(X+Y+1,2)*(19-14*X+3*math:pow(X,2)-14*Y+6*X*Y+3*math:pow(Y,2)))*(30+math:pow(2*X-3*Y,2)*(18-32*X+12*math:pow(X,2)+48*Y-36*X*Y+27*math:pow(Y,2))).

%%(Himmelblau's function)[(3,2),(3.584,-1.848),(-2.805,3.1313),(-3.779, -3.38)]=0
%math:pow(math:pow(X, 2) + Y - 11, 2) + math:pow((X + math:pow(Y, 2) - 7), 2),

%% Ackley's function (0,0)
%-20 * math:exp(-0.2 * math:sqrt(0.5 * (math:pow(X, 2) + math:pow(Y, 2)))) - math:exp(0.5 * (math:cos(2 * math:pi() * X) + math:cos(2 * math:pi() * Y))) + 0.5772156649 + 20.

optimize(InitPos, InitVel, LocalMin, GlobalMin, W_s, W_c, Phi, MPid, Lo, Hi) ->
  % recursive dimensions
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

%Create five groups on different TEDA nodes
init_groups(GroupNb, N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs) ->
  {ok, [_ | Ns]} = file:consult('enodes.conf'),
  GroupPIDList = lists:sublist(Ns, 1, GroupNb),
  [spawn(EID, ?MODULE, init, [N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs, self(), Ns]) || EID <- GroupPIDList],
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
        New < Old ->
          io:format("Found new swarm optimum: ~p~n", [New]),
          waitForGroups(GroupNb - 1, Min);
        true -> waitForGroups(GroupNb - 1, OldMin)
      end
  end.
