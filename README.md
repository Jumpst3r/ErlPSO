# ErlPSO
A Distributed particle swarm optimization (PSO) implementation in erlang.
Authors: Nicolas D. Mevlüt T.

---

This project aims to implement a high performance, distributed particle swarm optimizer. Developed during the CDC18 course. For a short description on particle swarm algorithms, I recommend you take a look at

https://en.wikipedia.org/wiki/Particle_swarm_optimization

## Quickstart

To use the optimizer, you need to do the following

1. Customize the cost function

Modify the function `cost_function` in `pso.erl`. 

2. Call `pso:init(N, W_s, W_c, Phi, Dim, Lo, Hi, Epochs)`, where

| Parameter name | Meaning | Recommended testing values |
|:--------------:|---------|----------------------------|
|`N`             | Number of particles | 10-100 |
|`W_s`           | Social weight. Determines how much particles are attracted to the global minimum. | 1-2 |
| `W_c`          | Cognitive weight. Determines how much particles are attracted to the local minimum. | 1-2 |
| `Phi`          | Inertia of the particles | (0,1] |

