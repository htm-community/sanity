# ComportexViz

A web-based visualization layer for
[Comportex](http://github.com/nupic-community/comportex/).

See it in action in [this blog
post](http://floybix.github.io/2014/07/11/visualization-driven-development-of-the-cortical-learning-algorithm/).

ComportexViz runs HTM models in the browser with interactive
controls. The model state from recent timesteps is kept, so you can step
back in time. You can inspect input values, encoded input bits, and the
columns that make up cortical region layers. Within a column you can inspect
cells and their distal dendrite segments. Feed-forward and distal synapses
can be shown.

Kept timesteps are shown in a row at the top of the display.
Below that, the blocks represent input fields (squares) and
layers of cortical columns (circles). Depending on the display mode,
these may be shown in 2D grids from a single time step, or as one
vertical line per timestep, allowing several time steps to be shown
in series. Also, summarised time series are shown in the 'plots' tab.


## Usage

Get [Leiningen](http://leiningen.org/) first.

Clone [Comportex](http://github.com/nupic-community/comportex/),
and install it to your local Maven repository (~/.m2):

```
lein install
```

Clone ComportexViz, and then build it:

```
lein do cljsbuild clean, cljsbuild once demos
```

Now open `public/demos/*.html` in a web browser, preferably Google
Chrome. Each HTML page loads the corresponding model defined
in `examples/demos/comportexviz/demos/`.


## License

Copyright © 2014 Felix Andrews

Distributed under your choice of
* the Eclipse Public License, the same as Clojure.
* the GNU Public Licence, Version 3 http://www.gnu.org/licenses/gpl.html, the same as NuPIC.
