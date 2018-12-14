#!/bin/bash

# This script is designed to facilitate the TEDA deployment for the
# distributed version of the pso_social_topo optimizer.
# Make sure that the master is alive if deployment fails.
# Intended to be run in the TEDA directory apps/pso_social_topo/

CMD="pso_social_topo:start(200,2,2,0.75,2,-5,5,100)"
USER="dutlyn"
MASTER="diufvm38.unifr.ch"

echo "Deploying monkeys"

echo "Pinging..."
../../scripts/ping.sh pso_social_topo hosts.conf
echo "Pinging Done"

echo "Making..."
../../scripts/depl_app.sh pso_social_topo make
echo "Making Done"

echo "Deploying..."
../../scripts/depl_enodes.sh pso_social_topo hosts_alive.conf
echo "Deploying Done"

echo "Running... $CMD"
../../scripts/run.sh pso_social_topo $CMD hosts_alive.conf $MASTER $USER
echo "Running Done"