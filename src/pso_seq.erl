%%%-------------------------------------------------------------------
%%% @author mevlut
%%% @copyright (C) 2018, <COMPANY>
%%% @doc A sequential PSO implementation
%%%
%%% @end
%%% Created : 30. Nov 2018 9:43 AM
%%%-------------------------------------------------------------------
-module(pso_seq).
-author("mevlut").

%% API
-export([start/8]).

start(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs) ->
  pso(Dim, N, W_s, W_c, Phi, Hi, Epochs).

pso(Dim, NbParticles, WS, WP, WG, Interval, Iterations) ->
  Time_start = os:system_time(),


  [ParticlesPos, ParticlesSpeed, ParticlesPBest] = initiateParticles(0, NbParticles, Dim, Interval, [], [], []),

  %io:format("Particles pos: ~p, speed: ~p, local min: ~p~n",[ParticlesPos, ParticlesSpeed,ParticlesPBest]),

  [GlobalMin, GlobalMinValue] = get_swarm_min(0, NbParticles, ParticlesPBest, Dim, undef, undef),
  %io:format("Initial Global min at: ~p, value: ~p~n~n", [GlobalMin,GlobalMinValue]),


  [FinalGB, FinalGBV] = optimize_function(1, NbParticles, 1, Iterations, Dim, ParticlesPos, ParticlesSpeed, ParticlesPBest, GlobalMin, GlobalMinValue, GlobalMin, GlobalMinValue, WS, WP, WG),
  io:format("Global min at: ~p, value: ~p~n", [FinalGB, FinalGBV]),
  Time_end = os:system_time(),

  Time_total = Time_end - Time_start,

  io:format("time to complet: ~p~n", [Time_total]),

  terminate.


initiateParticles(NbParticles, NbParticles, _, _, Position, Speed, PBest) ->
  [Position, Speed, PBest];

initiateParticles(N, NbParticles, Dim, Interval, Position, Speed, PBest) ->
  NewPos = initiatePos(Dim, [], Interval),
  NewSpeed = initiatePos(Dim, [], Interval),
  NewPositions = lists:append(Position, [NewPos]),
  NewSpeeds = lists:append(Speed, [NewSpeed]),
  NewPBest = lists:append(PBest, [NewPos]),
  initiateParticles(N + 1, NbParticles, Dim, Interval, NewPositions, NewSpeeds, NewPBest).


initiatePos(0, Pos, _) ->
  Pos;
initiatePos(N, Pos, Interval) when N > 0 ->
  NewCoord = [myUniform(Interval)],
  NewPos = lists:append(NewCoord, Pos),
  initiatePos(N - 1, NewPos, Interval).

get_swarm_min(0, NbParticles, Positions, Dim, undef, undef) ->
  NewGBest = lists:nth(1, Positions),
  NewGBValue = evaluate(Dim, NewGBest),
  get_swarm_min(2, NbParticles, Positions, Dim, NewGBest, NewGBValue);
get_swarm_min(N, NbParticles, _, _, GBest, GBValue) when N > NbParticles ->
  [GBest, GBValue];
get_swarm_min(N, NbParticles, Positions, Dim, GBest, GBValue) ->
  NewPBest = lists:nth(N, Positions),
  NewPBValue = evaluate(Dim, NewPBest),
  if
    NewPBValue < GBValue -> get_swarm_min(N + 1, NbParticles, Positions, Dim, NewPBest, NewPBValue);
    true -> get_swarm_min(N + 1, NbParticles, Positions, Dim, GBest, GBValue)
  end.


optimize_function(N, NbParticles, M, Iterations, Dim, Positions, Velocities, PBests, _, _, NextGB, NextGBV, WS, WP, WG) when N > NbParticles ->
  optimize_function(1, NbParticles, M + 1, Iterations, Dim, Positions, Velocities, PBests, NextGB, NextGBV, NextGB, NextGBV, WS, WP, WG);

optimize_function(_, _, M, Iterations, _, _, _, _, GBest, GBValue, _, _, _, _, _) when M > Iterations ->
  [GBest, GBValue];

optimize_function(N, NbParticles, M, Iterations, Dim, Positions, Velocities, PBests, GBest, GBValue, NextGB, NextGBV, WS, WP, WG) ->
  Rp = rand:uniform(),
  Rg = rand:uniform(),
  Pos = lists:nth(N, Positions),
  Vel = lists:nth(N, Velocities),
  PB = lists:nth(N, PBests),
  [NewPos, NewVel, NewPB, NewPBValue] = updateLocation(Dim, Dim, Pos, Vel, PB, GBest, Rp, Rg, WS, WP, WG),
  NewPositions = setnth(N, Positions, NewPos),
  NewVelocities = setnth(N, Velocities, NewVel),
  NewPBests = setnth(N, PBests, NewPB),
  if
    NewPBValue < GBValue ->
      optimize_function(N + 1, NbParticles, M, Iterations, Dim, NewPositions, NewVelocities, NewPBests, GBest, GBValue, NewPB, NewPBValue, WS, WP, WG);
    true ->
      optimize_function(N + 1, NbParticles, M, Iterations, Dim, NewPositions, NewVelocities, NewPBests, GBest, GBValue, NextGB, NextGBV, WS, WP, WG)
  end.



updateLocation(0, Dim, Position, Velocity, PBest, _, _, _, _, _, _) ->
  %io:format("Position: ~p, Velocity: ~p~n",[Position,Velocity]),
  PBValue = evaluate(Dim, PBest),
  NewPosValue = evaluate(Dim, Position),
  if
    NewPosValue < PBValue -> [Position, Velocity, Position, NewPosValue];
    true -> [Position, Velocity, PBest, PBValue]
  end;
updateLocation(N, Dim, Position, Velocity, PBest, GBest, Rp, Rg, WS, WP, WG) when N > 0 ->
  V = lists:nth(N, Velocity),
  P = lists:nth(N, Position),
  PB = lists:nth(N, PBest),
  G = lists:nth(N, GBest),
  %io:format("V: ~p, P: ~p, PB: ~p, G: ~p,Rp ~p, Rg ~p~n",[V,P,PB,G,Rp,Rg]),
  NewV = WS * V + WP * Rp * (PB - P) + WG * Rg * (G - P),
  NewP = P + NewV,
  %io:format("NewV: ~p, NewP: ~p~n",[NewV,NewP]),
  NewVel = setnth(N, Velocity, NewV),
  NewPos = setnth(N, Position, NewP),
  updateLocation(N - 1, Dim, NewPos, NewVel, PBest, GBest, Rp, Rg, WS, WP, WG).




evaluate(2, Coordinates) ->
  X = lists:nth(1, Coordinates),
  Y = lists:nth(2, Coordinates),
  math:pow(math:pow(X, 2) + Y - 11, 2) + math:pow((X + math:pow(Y, 2) - 7), 2).



myUniform(N) ->
  case rand:uniform(2) of
    1 -> -(rand:uniform(N));
    2 -> rand:uniform(N)
  end.

setnth(N, List, Value) when N =:= 1 ->
  NewList = [Value] ++ lists:nthtail(1, List),
  NewList;
setnth(N, List, Value) when N > 1 ->
  NewList = lists:sublist(List, N - 1) ++ [Value] ++ lists:nthtail(N, List),
  NewList.