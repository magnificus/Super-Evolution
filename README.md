# Super-Evolution

## Background

This repository contains the mostly successful experiment with regression analysis using genetic programming, so in a nutshell, given an arbitrary number of sample points containing the values of different input variables along with the output result, it'll try to find an equation that saisfies these conditions. The exciting thing about this approach is that it in theory works for any equation, it's not limited to for example linear regression or quadratic regression.

## How it works

This is assuming you're familiar with genetic algorithms already.

The system uses an internal representation of different genes that are expanded into an Abstract Syntax Tree (https://en.wikipedia.org/wiki/Abstract_syntax_tree), that is then evaluated using the provided inputs. Fitness is calculated as sum of squares differences between the output from the AST and the given result. This of course means that lower values correspond to higher fitness, and a fitness of 0.0 means that the solution has been truly "found".

## Using the program

You can either choose to compile the program to an .exe or just use the main function inside of SuperEvolution.hs, it'll parse the text file "input.txt" for your input data and output new generations until you stop the program.

## Results

Results have been good so far, but need to test more and test more possible configurations, results are not really reproducible because of the high degree of randomness involved, but for this input:

x = 1, y = 4, res = 3

x = 2, y = 5, res = 7

x = 1, y = 5, res = 4

x = 2, y = 1, res = 3

x = 3, y = 10, res = 17


I have been able to reliably get the "solution" (it's x^2 + y - 2), though the time it takes to get the result varies. This is definitely on the lower end of provided inputs though, the more you give it the better it'll become.

## Problems

Overfitting, though doesn't seem to be as big of an issue as anticipated, will write more on this.
