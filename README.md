Shmup prototype in Haskell, using Netwire 5 and SDL 0.6.5
=========================================================

Introduction
------------

This project is a very simple and rough shmup engine, with a little of level design possible. It implements a game mechanic about bouncing, where the pattern to navigate through is created by the reflection of the player bullets on ennemies

Requirements
------------

To build this project, you only need to install Netwire 5 and SDL package for Haskell (with cabal for example), and then to compile MinimumViableProduct.hs

Configuration
-------------

By creating the configMinimumViableProduct file using the model given by exampleConfigMinimumViableProduct, you can modify ennemies in the game.
Comments lines in it must be preceded by --

Soon-to-be-features
-------------------

 - Adding a life counter for the ennemies
 - Adding a Hit-box for the player
 - Updating ennemies by pressing Enter
 - Adding online new config files and switching between them
