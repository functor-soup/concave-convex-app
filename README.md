# concave-convex-app

Simple web-app where one draws a simple polygon and the app tells you whether the polygon is
concave or convex.

(As of now, there is no intersection detection)

## Overview

Exercise in using clojurescript

## Work left to do

1. Code cleanup (core.cljs is still pretty ugly)
2. Detect line intersection and warn user
3. Better UI

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

