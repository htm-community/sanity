# How it works

_(This page is currently a draft / dumping ground.)_

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

## The network / messaging layer

Let's dive right in.

When running HTM on a server, the browser connects to the server via a
websocket. Every message arriving on the websocket is a string that contains a
[transit]((http://blog.cognitect.com/blog/2014/7/22/transit))-serialized
vector. The vector generally looks like:

~~~clojure
`[target-id :put! msg]`
~~~

Clients and servers send messages to each others' targets, usually by wrapping a
"channel" abstraction around the target-id. That's where the `put!` comes
from. Clients and servers tell each other about their own target-ids, but the
process needs to be jumpstarted somehow. The client needs to know about at least
one server target a priori. This is sometimes hardcoded (the Sanity runner knows
to expect an `:into-journal` and an `:into-sim` target) and other times is
injected into a webpage (the Sanity notebook hosts an arbitrary number of
journals in one webpage, so it generates a uniquely named equivelent to
`:into-journal` on every `viz`). New targets are generated all the time and are
sent inside of `msg`s.

The `msg` is arbitrary large. It is often a vector containing a command and
arguments, and other times it's a response to a specific request or
subscription.

### Channels

Sanity's messaging is built on top of:

- [core.async](https://github.com/clojure/core.async) channels
- The idea that you can add extra information to a message during serialization
  and deserialization

Combining these ideas, you get the ability to pass channels inside of messages
to other machines. The message recipient is free to put values back into that
channel. Message recipients can respond to your message if you give them a
channel. They can even respond multiple times (i.e. a "subscription").

So, messages are passed via channels, and messages can contain channels.

### The high level: Drawing boxes

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

#### Why Transit?

Initially, it was because it was convenient for Clojure while being accessible
from everywhere else. It's like JSON, but the Map keys can be anything (not just
strings), and has a Set type. There are other cool little things to get excited
about.

But the real reason we use Transit is its customizable read-handlers and
write-handlers. They're how this whole "drawing boxes" / "marshal" approach
works.

## The messages

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

_(This page is currently a draft / dumping ground.)_
