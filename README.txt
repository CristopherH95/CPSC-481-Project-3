Project 3 'Ant Colony' Optimization

Written by:
	TEAM NLP
	Cristopher Hernandez
	Suyash Singh

Contact:
	cristopherh@csu.fullerton.edu
	suyash.vardhansingh@csu.fullerton.edu

Class:
	CPSC-481-03

Introduction:
	Build an Ant Colony Optimization swarm that will start from one corner of a 2D grid of cells, 
    containing obstacles, and attempt to find a high-quality path to the diagonally opposite corner.


Build:
	Executable from the command line without compilation. 
  	However, a compiled version can be created using:

  	(COMPILE-FILE "project2.lisp")

  	In a Common Lisp REPL


Installation:
	files list:
		project3.lisp
		grid_a.txt
		grid_b.txt
		grid_c.txt
		grid_d.txt

Setup:
	This solution was written and tested with GNU Common Lisp for Windows.
  	CLISP for windows can be downloaded and installed here: https://sourceforge.net/projects/clisp/files/

Usage:
  	run on command line:

  	clisp project2.lisp

  	load compiled .fas in REPL:

  	(LOAD "project2.fas")

Extra Features:
  	None

Bugs:
  	'Ant' agents can easily become stuck in a loop of cells, preventing them from completing the maze.