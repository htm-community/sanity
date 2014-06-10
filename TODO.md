# TODO

* cache display data for each time step

* split up viz-canvas namespace

* generate html ui from code
* or, have only one page that can load different examples
* tabs
  * description
  * controls
  * noise (nb control channel)
  * parameter values (edit!)
  * region details
  * selected column details
  * plots

* trace back predicted columns to
  * inbits  (weighted by some sort of confidence?)
  * inputs  (reverse encoders?)

* get rid of mq ns, have each ns give a channel and connect them in page.

* legend for colours
* display duty cycles
* display numbers of dendrites
* show parameter values
* ui to change parameter values
* and restart sim
* snapshots (store state of `steps`)
* graph anomaly rate over time
* graph activation level over time
* 2D input fields
* 2D column layouts
* multiple regions in hierarchy
* do not call model functions from viz-canvas
