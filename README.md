# ErlPSO
A Distributed particle swarm optimization (PSO) implementation in erlang.
Nicolas D. Mevlüt T.

---
*Implemented for the project of the IN.5022 Concurrent and Distributed Computing [HS 18] course.*

This project aims to implement a high performance, distributed particle swarm optimizer. For a short description on particle swarm algorithms, I recommend you take a look at

[Particle swarm optimization](https://en.wikipedia.org/wiki/Particle_swarm_optimization)

## Project structure

The distributed PSO implementations can be found in


`src/pso_global_topo.erl` (global swarm topology)

`src/pso_social_topo.erl` (social swarm topology)

The sequential PSO implementation can be found in


`src/pso_seq.erl`

## TEDA deployment

The implementations are meant to be run in the TEDA environment. The user should create a folder in the TEDA `apps` directory prior to deployment. For example, if a user wants to test the distributed swarm topology, he would need to create a directory `apps/pso_global_topo` and move the files `pso_global_topo.erl` and `quickdeploy-global-topo.sh` to the newly created directory.
After doing so, the user should update the variables in the script to match his environment. Once done, the script can be run:

`./quickdeploy-global-topo.sh`

## Quickstart

To use the optimizer, you need to do the following

1. Customize the cost function

Modify the function `cost_function` in the implementation file you want to run. Several test functions are already availble.
The main implementation is `pso_global_topo.erl`, which implements a global swarm topology.
The file `pso_social_topo.erl` is a slight variation, implementing a social swarm topology

2. Call `pso_global_topo:start(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs)`, where

| Parameter name | Meaning | Recommended testing values |
|:--------------:|---------|----------------------------|
|`N`             | Number of particles | 10-100 |
|`W_s`           | Social weight. Determines how much particles are attracted to the global minimum. | 1-2 |
| `W_c`          | Cognitive weight. Determines how much particles are attracted to the local minimum. | 1-2 |
| `Phi`          | Inertia of the particles | (0,1] |
| `Dim`          | The dimension of the problem to optimize. | problem dependent |
| `Lo`           | The lower search bound | problem dependent. |
| `Hi`           | The upper search bound | problem dependent |
| `Epochs`       | Number of epochs | 100-1000 |

---

## Why use a distributed PSO ? 

If the particle swarm algorithm is used with a high amount of particles, then distributing the particles on different erlang nodes results in a massive performance boost, as can be seen in the following plot:

![pso_plot](data/result.png)

Both version where started with following parameters (Variable number of particles):

`W_s = 2`

`w_c = 2`

`Phi = 0.75`

`Dim = 2`

`Lo = -5`

`Hi = 5`

`Epochs = 200`

The function used to test the PSO implementation is [Himmelblau's function](https://en.m.wikipedia.org/wiki/Himmelblau%27s_function):

![3D plot of Himmelblau's function](http://infinity77.net/global_optimization/_images/HimmelBlau.png)
![Contour plot](https://upload.wikimedia.org/wikipedia/commons/d/d8/Log_LC_Himmelblau_Function.PNG)
From
http://infinity77.net/global_optimization/_images/HimmelBlau.png
https://upload.wikimedia.org/wikipedia/commons/d/d8/Log_LC_Himmelblau_Function.PNG
