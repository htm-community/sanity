# ComportexViz Bridge

The ComportexViz client must be initialized with a bridge to a server. This server might run in the browser, it might run remotely, or it could potentially even be a static set of files that the bridge knows how to read. The bridge is responsible for making sure someone listens to the `into-journal` (required) and `into-sim` (optional) channels.
