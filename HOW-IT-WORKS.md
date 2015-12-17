# How it works

_(This page is currently a draft / dumping ground.)_

- [High-level description](#high-level)
- [The network / messaging layer](#network-messaging-layer)
  - [Channels](#channels)
  - [Drawing boxes](#drawing-boxes)
  - [Why Transit?](#why-transit)
- [The messages](#the-messages)
  - [Journal](#journal)
    - ["subscribe"](#subscribe)
    - ["get-network-shape"](#get-network-shape)
    - ["get-capture-options"](#get-capture-options)
    - ["set-capture-options"](#set-capture-options)
    - ["get-apical-segments"](#get-apical-segments)
    - ["get-distal-segments"](#get-distal-segments)
    - ["get-proximal-segments"](#get-proximal-segments)
    - ["get-apical-synapses"](#get-apical-synapses)
    - ["get-distal-synapses"](#get-distal-synapses)
    - ["get-proximal-synapses"](#get-proximal-synapses)
    - ["get-layer-stats"](#get-layer-stats)
    - ["get-column-cells"](#get-column-cells)
    - ["get-layer-bits"](#get-layer-bits)
    - ["get-sense-bits"](#get-sense-bits)
  - [Simulation](#simulation)
    - ["subscribe-to-status"](#subscribe-to-status)
    - ["run"](#run)
    - ["pause"](#pause)
    - ["toggle"](#toggle)
    - ["step"](#step)

## <a name="high-level" />High-level description

This page focuses on Sanity's data flow. What is the client sending to the
server? And vice versa?

Here's the system. Imagine you're the user interface -- click handlers and
such. So there's you, and then there are these other things that you can't see
or touch. All you can do is put messages into a queue that's intended for them.
Meanwhile, they're off doing their own thing, while also watching their message
queue. They can't see you or touch you. They can't even respond to your messages
unless you send them a reference to a message queue that you're watching.

It's not difficult to build a system like this, and it's trivial to do with
core.async channels. But Sanity goes further: it does this across multiple
machines.

Here's a useful way of describing Sanity's data flow. Imagine a typical
single-process system with multiple components each taking values from their own
channels and putting values into other channels. Now: rip that system apart.
Put some of the components on different machines. Sanity does this without
making the UI or the components do messaging differently. It's just channels.

## <a name="network-messaging-layer" />The network / messaging layer

Let's dive right in.

When running HTM on a server, the browser connects to the server via a
websocket. Every message arriving on the websocket is a string that contains a
[transit]((http://blog.cognitect.com/blog/2014/7/22/transit))-serialized
vector. The vector generally looks like:

~~~clojure
`["put!" target-id msg]`
~~~

Clients and servers send messages to each others' targets, usually by wrapping a
"channel" abstraction around the target-id. That's where the `put!` comes
from. Clients and servers tell each other about their own target-ids, but the
process needs to be jumpstarted somehow. The client needs to know about at least
one server target a priori. This is sometimes hardcoded (the Sanity runner knows
to expect a `journal` and a `simulation` target) and other times is injected
into a webpage (the Sanity notebook hosts an arbitrary number of journals in one
webpage, so it generates a uniquely named equivelent to `journal` on every
`viz`). New targets are generated all the time and are sent inside of `msg`s.

The `msg` is arbitrary large. It is often a vector containing a command and
arguments, and other times it's a response to a specific request or
subscription.

### <a name="channels" />Channels

Sanity's messaging is built on top of:

- [core.async](https://github.com/clojure/core.async) channels
- The idea that you can add extra information to a message during serialization
  and deserialization

Combining these ideas, you get the ability to pass channels inside of messages
to other machines. The message recipient is free to put values back into that
channel. Message recipients can respond to your message if you give them a
channel. They can even respond multiple times (i.e. a "subscription").

So, messages are passed via channels, and messages can contain channels.

### <a name="drawing-boxes" />Drawing boxes

When you send a message, you know what's in it, and the final recipient of the
message knows what to expect. You don't know whether the recipient is in your
process, on your machine, or in the cloud -- and you don't want to know. The
recipient isn't interested in you either.

In between you and the recipient, there are zero or more intermediaries. Your
message might get serialized and deserialized, or it might not.

So what do you do? You annotate the parts of the message that deserve special
attention from the intermediary. You say things like:

- "This part of this message is a channel. If you're transferring it out of my
  process, please work some magic to let others use it."
- "This part of this message is big, and I'm going to send it multiple times. If
  this is going across the network, try to cache it remotely so that you don't
  have to keep sending it."

So, in other words:

- You put parts of the message into a box, which we call a "marshal".
- The recipient takes them out of the box.
- Neither you nor the recipient even know what the "internet" is. But whoever is
  doing serializing and deserializing probably does.
- With a marshal, you request some non-standard serialization.
- The serialization code has no idea what to expect from your message, but it
  knows what a marshal is. When it sees a marshal, it adds information to the
  message to aid the next deserializer.

Normally it's dirty for intermediaries to inspect messages, but serializers and
deserializers already have to do it, so let's use them!

### <a name="why-transit" />Why Transit?

Initially, it was because it was convenient for Clojure while being accessible
from everywhere else. It's like JSON, but the Map keys can be anything (not just
strings), and has a Set type. There are other cool little things to get excited
about.

But the real reason we use Transit is its customizable read-handlers and
write-handlers. They're how this whole "drawing boxes" / "marshal" approach
works.

## <a name="the-messages" />The messages

For these messages, there's something important to remember: they're not just
remotely inspecting a data structure. They're inspecting:

- the active cells and columns from the end of the timestep
- the predicted cells from the beginning of the timestep
- the segments and synapses that caused these activations
- the learning that occurred during this timestep

When the client asks you for the synapses at a particular timestep, they're
looking to you to choose the best data. For example, you'll probably want to
send the synapse permanences as they were at the beginning of the timestep, not
at the end. But you might also send the synapse-learning that happened during
this timestep, so that the client can draw these in a different color
designating learning.

If you're experimenting with the HTM algorithms, you might go further. Nothing's
stopping you from showing a synapse reaching back 5 timesteps.

Naming conventions:

- In a synapse:
  - the "source" is the presynaptic neuron
  - the "target" is the postsynaptic neuron

### <a name="journal" />Journal

#### <a name="subscribe" />"subscribe"

**Parameters:**

- `steps_channel_marshal`

**Side effects:** Save the `steps_channel`. Whenever the model is stepped, put
the new step into the provided channel. This subscription lasts until the client
disconnects.

~~~python
# Example step
{
    'snapshot-id': 'choose a unique identifier for this model snapshot',
    'timestep': 42,
    'display-value': [['myField1', 'Training'], ['myField2', 1.21]]
}
~~~

#### <a name="get-network-shape" />"get-network-shape"

**Response:**

~~~python
# Example response
{
    'senses': {
        'mySense1': {
            'ordinal': 0, # Display order
            'dimensions': (200,),
        },
    }
    'regions': {
        'myRegion1': {
            'myLayer1': {
                'ordinal': 1, # Display order
                'cellsPerColumn': 32,
                'dimensions': (20,),
            }
        }
    }
}
~~~

#### <a name="get-capture-options" />"get-capture-options"

**Response:**

~~~python
# Example capture options
{
    'keep-steps': 50,
    'ff-synapses': {
        'capture?': False,
        'only-active?': True,
        'only-connected?': True,
    },
    'distal-synapses': {
        'capture?': False,
        'only-active?': True,
        'only-connected?': True,
        'only-noteworthy-columns?': True,
    },
    'apical-synapses': {
        'capture?': False,
        'only-active?': True,
        'only-connected?': True,
        'only-noteworthy-columns?': True,
    },
}
~~~

#### <a name="set-capture-options" />"set-capture-options"

**Parameters:**

- `capture_options`

**Side effects:** Start using these new capture-options for all clients.

#### Get apical/distal/proximal segments

<a name="get-apical-segments" />
<a name="get-distal-segments" />
<a name="get-proximal-segments" />

**Messages:**

- `"get-apical-segments"`
- `"get-distal-segments"`
- `"get-proximal-segments"`

**Parameters:**

- `snapshot_id`
- `region_id`
- `layer_id`
- `segment_selector`
  - Examples:
    - `[]` = none
    - `[1, 7]` = all segments for columns 1, 7
    - `{1: [2]}` = all segments for column 1, cell 2
    - `{1: {2: [3, 4]}}` = the third and fourth segments on column 1, cell 2
- `response_channel_marshal`

**Response:**

The segments of the given apical/distal/proximal type by cell index and segment
index.

~~~python
{
    0: {
        0: {
            'n-conn-act': 14,
            'n-conn-tot': 18,
            'n-disc-act': 2,
            'n-disc-tot': 10,
            'stimulus-th': 12,
            'learning-th': 8,
        }
    },
    1: {},
    2: {},
}
~~~

#### Get apical/distal/proximal synapses

<a name="get-apical-synapses" />
<a name="get-distal-synapses" />
<a name="get-proximal-synapses" />

**Messages:**

- `"get-apical-synapses"`
- `"get-distal-synapses"`
- `"get-proximal-synapses"`

**Parameters:**

- `snapshot_id`
- `region_id`
- `layer_id`
- `segment_selector`
  - Examples:
    - `[]` = none
    - `[1, 7]` = all synapses for columns 1, 7
    - `{1: [2]}` = all synapses for column 1, cell 2
    - `{1: {2: [3, 4]}}` = all synapses on the third and fourth segments on column 1, cell 2
- `synapse_states` a set potentially containing:
  - `"active"`
  - `"inactive"`
  - `"disconnected"`
- `response_channel_marshal`

**Response:**

Synapses by state

~~~python
# Example response
{
    'active': [
        {
            'src-id': 'mySense1',
            'src-col': 48,
            'perm': 0.4,
            'src-dt' 1,
        },
        {
            'src-id': 'myRegion1',
            'src-lyr': 'myLayer1',
            'src-col': 23,
            'perm': 0.3,
            'src-dt' 1,
        }
    ],
    'inactive': {},
    'disconnected': {},
}
~~~

#### <a name="get-layer-stats" />"get-layer-stats"

**Parameters**:

- `snapshot_id`
- `region_id`
- `layer_id`
- `fetches` a set potentially containing each of the following values:
  - `"n-unpredicted-active-columns"`
  - `"n-predicted-inactive-columns"`
  - `"n-predicted-active-columns"`
- `response_channel_marshal`

**Response**:

~~~python
# Example response
{
    'n-unpredicted-active-columns': 10,
    'n-predicted-inactive-columns': 10,
    'n-predicted-active-columns': 30,
}
~~~

#### <a name="get-column-cells" />"get-column-cells"

**Parameters:** `snapshot_id`, `region_id`, `layer_id`, `column`,
`response_channel_marshal`

**Response:**

~~~python
# Example response
{
    'cells-per-column': 32,
    'active-cells': set([1, 2, 3]),
    'prior-predicted-cells': set([2, 3, 4]),
}
~~~

#### <a name="get-layer-bits" />"get-layer-bits"

**Parameters:**

- `snapshot_id`
- `region_id`
- `layer_id`
- `fetches` a set potentially containing each of the following values. Servers
  might only implement some of these values. Feel free to ignore some.
  - `"active-columns"`
  - `"pred-columns"`
  - `"overlaps-columns-alpha"`
  - `"boost-columns-alpha"`
  - `"active-freq-columns-alpha"`
  - `"n-segments-columns-alpha"`
  - `"tp-columns"`
  - `"break?"`
- `onscreen_bits_marshal` a BigValueMarshal for the onscreen columns. Servers
  are free to choose whether they pay attention to onscreen bits. It won't hurt
  anything if you return data for offscreen bits.
- `response_channel_marshal`

**Response:**

~~~python
# Example response. Here `fetches` was `set(['active-columns', 'pred-columns'])`
{
    'active-columns': set([42, 43, 44]),
    'pred-columns': set([40, 41, 42]),
}
~~~

#### <a name="get-sense-bits" />"get-sense-bits"

**Parameters:**

- `snapshot_id`
- `sense_id`
- `fetches` a set potentially containing each of:
  - `"active-bits"`
  - `"pred-bits-alpha"`
- `onscreen_bits_marshal` a BigValueMarshal for the onscreen bits. Servers are
  free to choose whether they pay attention to onscreen bits. It won't hurt
  anything if you return data for offscreen bits.
- `response_channel_marshal`

**Response:**

~~~python
# Example response
{
    'active-bits': set([3, 4, 5,]),
}
~~~

### Simulation

#### <a name="subscribe-to-status" />"subscribe-to-status"

**Parameters:** `subscriber_channel_marshal`

**Side effects:** Save the `subscriber_channel`. Whenever the simulation is
paused or unpaused, put `[true]` or `[false]` on the channel, depending on
whether the simulation is now going.

**Response:** Put the simulation's current `[true]` / `[false]` status on the
channel immediately, without waiting for a pause / unpause.

**Remarks:** The boolean is wrapped in a list because some parts of the pipeline
might get confused by a falsy value like `false`.

#### <a name="run" />"run"

**Side effects:** Unpause the simulation if it's currently paused.

#### <a name="pause" />"pause"

**Side effects:** Pause the simulation if it's currently going.

#### <a name="toggle" />"toggle"

**Side effects:** Unpause the simulation if it's currently paused, pause the
simulation if it's currently going.

#### <a name="step" />"step"

**Side effects:** Perform one step of the simulation.

_(This page is currently a draft / dumping ground.)_
