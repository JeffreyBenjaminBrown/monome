A [monome](https://monome.org/) is a USB-powered grid of two-dimensional switches with LEDs under them, that can talk to OSC via this [protocol](https://monome.org/docs/osc/).

I would like to use an OSC library -- probably [HOsc](https://www.stackage.org/package/hosc), right? -- to interact with a monome.

There already exists a [Python library](https://github.com/artfwo/pymonome) for doing that. I am able to run the first program in the [monome Python tutorial](https://monome.org/docs/grid-studies/python/). (It uses that library.) The tutorial program looks like it's 13 lines of code, but really there are only two lines of non-boilerplate code:

    def on_grid_key(self, x, y, s):
      print("key:", x, y, s)
      self.grid.led_level_set(x, y, s*15)

It detects the monome's address (which might be "/dev/ttyUSB0\P"?) and reacts to key presses from it. Whenever a key is pressed, it prints the key's coordinates and state, and sends a "light up" or "stop being lit up" instruction to that button. (Light levels are encoded as numbers from 0 to 15. State is binary: 1 = pressed, 0 = not pressed.)

The monome's OSC protocol is pretty simple. If I can just connect to it, send one message and receive one message, I'll be off to the races -- but I don't know how.

Any help will be enormously appreciated.
