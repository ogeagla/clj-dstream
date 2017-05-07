# clj-dstream

## What Is It
Density-based data stream clustering for arbitrary dimension data written in Clojure.

Reference [paper](refpaper).


Sample data stream with moving cluster:
![Three Clusters GIF][clustergif]


[refpaper]: https://github.com/ogeagla/clj-dstream/raw/master/papers/sigproc-sp.pdf
[clustergif]: https://github.com/ogeagla/clj-dstream/raw/master/doc/resources/3-clusters-1-at-a-time/animated-loop.gif "Three Clusters One At A Time"

## Crater Dataset

When faced with data shapes like a crater, which can look like so:
![Crater Grids SVG][cratergrids]
Many clustering algorithms would combine these clusters into one, but DStream results look better:
![Crater Clusters GIF][craterclusters]

[craterclusters]: https://github.com/ogeagla/clj-dstream/raw/master/doc/resources/crater/animated-clusters-loop.gif "Crater Clusters"
[cratergrids]: https://github.com/ogeagla/clj-dstream/raw/master/doc/resources/crater/grids-crater-sampling-000000000-rainbow2.svg "Crater Data Stream"


## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
