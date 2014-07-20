A short description of each of the projects:

abui
====

Argumentation-based Bottom Up Induction (based on this paper: http://www.icml2010.org/papers/284.pdf)
The goal in this case was to collect observations of properties from diseased soybeans and,
using multiple agents, have them argue back and forth to come to a consistent hypothesis
that relates the disease properties to a disease classification.

computer_vision
===============

detection.go - Implemented in the Go programming language, the goal here is similar to
that in the Viola-Jones paper on facial detection: A large collection of Haar-like features
are extracted from a set of training images and, using adaboost, are used to create a
stronger classifier by appropriately weighting the weak set of Haar-like features.  A cascade
of these strong classifiers is then assembled such that more false positives are pruned off
at each step while trying to maintain the false negative rate as low as possible.

pca_lda.m - A comparison of the two techniques (in this case using PCA for dimension reduction)
for facial recognition.

game
====

A small game using the bullet physics engine where spheres are thrown into a moving box.
Used in conjunction with an accelerometer connected to a Motorola microcontroller running
an RTOS to control the throwing.

p2p_example
===========

A simple example of the type of query flooding that would occur in a peer-to-peer network
such as gnutella for a file search (this doesn't implement any type of file transfer).

ray_tracer
==========

An old ray tracing project that has support for n-deep reflections, refraction, distribution
effects (fuzzy reflection and refraction), sharp and soft shadows and diffusing shading.

scheme
======

Some small AI type of programs written in scheme (i.e., boolean simplifier, csp solver, etc.).

stylometry
==========

The goal here is to determine the authorship of some unknown work by selecting from a pool of
authors that it has been trained on.

