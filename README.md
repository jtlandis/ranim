# ranim

<!-- badges: start -->
<!-- badges: end -->

**ranim** is an R package for creating hierarchical, shape-based animations. Build complex animated visualizations using composable shapes and actions that evolve over time.

## Overview

ranim provides a framework for creating animations where:

- **Shapes** form a hierarchy: each shape can contain other shapes
- **Transformations** cascade down: moving a parent automatically affects all children
- **Actions** drive animation: apply time-varying transformations like movement, rotation, and color change
- **Rendering** is flexible: animate interactively on-screen or export to GIF

The package is built on [S7](https://rconsortium.github.io/S7/) classes and is designed to handle complex shape hierarchies with smooth, composable animations.

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jtlandis/ranim")
```

## Quick Start

### Create a Simple Animation

```r
library(ranim)

# Create a canvas (window)
w <- window(bl = pos(0, 0), tr = pos(10, 10))

# Add a shape (rectangle)
rect_obj <- rect(
  width = 2, height = 2, 
  color = "red",
  trans = transform(pos(5, 5))
)
w@child(rect_obj)

# Add an action (move to new position over 2 seconds)
rect_obj@action(
  translate(to = pos(8, 8), time = time(2))
)

# Animate on screen
anim(w, fps = 25)

# Or export to GIF
render_gif(w, "animation.gif", fps = 25)
```

### Chain Multiple Actions

Actions can be composed with `@then()`:

```r
rect_obj@action(
  translate(to = pos(8, 8), time = time(1))@then(
    color("red", "blue", time = time(1))
  )@then(
    scale2(size = 1, time = time(0.5))
  )
)
```

### Create Shape Hierarchies

Children inherit transformations from parents:

```r
# Parent group
group <- shape(trans = transform(pos(5, 5)))
w@child(group)

# Add children
for (i in 1:5) {
  child <- point(
    color = "steelblue",
    trans = transform(pos(i - 3, 0))
  )
  group@child(child)
}

# Rotate the entire group
group@action(
  rotate(degrees = 360, time = time(3))
)
```

### Spawn Grids of Shapes

Dynamically create grids with optional staggering:

```r
container <- shape(trans = transform(pos(5, 5)))
w@child(container)

container@action(
  spawn_grid(
    nrow = 5, ncol = 5,
    color = "steelblue",
    xtime = 0.05, ytime = 0.05,
    actions = list(
      scale2(size = 0.8, time = time(0.5))
    )
  )
)
```

## Core Concepts

### Shapes

The fundamental building blocks. Common shapes:

- `shape()` — Generic shape (often used as a container)
- `rect()` — Rectangle
- `point()` — Point (dot)
- `polygon()` — Polygon defined by vertices
- `text()` — Text label
- `line()` — Line connecting points

All shapes extend the base `shape` class and support:
- Position, size, and rotation (via `trans`)
- Parent-child relationships
- Child shapes
- Animated actions
- Color

### Transformations

Shapes have a `trans` (transform) object that encodes:
- **Position** (`global` and `offset`): where the shape is
- **Size** (`size`): scale factor
- **Angle** (`angle`): rotation in degrees
- **Anchor**: reference point for transformations

Query functions:
- `obj_pos()` — Get position
- `obj_size()` — Get size
- `obj_angle()` — Get rotation angle
- `obj_anchor()` — Get anchor point

Direct modification:
- `obj_translate()` — Move to new position
- `obj_scale()` — Scale around a point
- `obj_rotate()` — Rotate around a point
- `obj_color()` — Change color

### Actions

Time-varying functions that modify shapes. Create actions with:

- `translate(to, time = time(...))` — Move to a position
- `rotate(degrees = ..., time = time(...))` — Rotate by angle
- `scale2(size = ..., time = time(...))` — Scale to target size
- `color(from, to, time = time(...))` — Color transition
- `colors(list, durations)` — Multi-color sequence
- `spawn_grid(...)` — Create a grid of children
- `action(func, time = ...)` — Custom action

Compose actions:
- `action1@then(action2)` — Run sequentially
- `action@repeating(n)` — Repeat n times

### Time

Controls animation pacing:

```r
t <- time(
  duration = 2,              # 2 seconds
  ease = ease(ein = ecubic), # Easing function
  repeating = 1              # Repeat once (play twice total)
)
```

Easing functions:
- `elinear` — Linear (no easing)
- `ecubic` — Cubic (t^3)
- `equad` — Quadratic (t^2)
- `esin` — Sine
- `ecos` — Cosine
- `ease(ein = ..., eout = ...)` — Combine easing curves

### Windows

The animation canvas:

```r
w <- window(
  bl = pos(0, 0),        # Bottom-left corner
  tr = pos(10, 10),      # Top-right corner
  fps = 25               # Optional: set frame rate
)
```

### Trackers

Preserve shape references across cloning operations:

```r
trk <- tracker(w)
trk@track(my_rect = rect_obj, my_point = point_obj)

# Add animations...

# Save checkpoint
checkpoint <- trk@clone()

# Continue animating...

# Restore to checkpoint
restored <- checkpoint@restore()
```

## Rendering

### Interactive Animation

Display on-screen with `anim()`:

```r
anim(w, fps = 25)  # 25 frames per second
anim(w)            # Real-time (no fixed FPS)
```

### Export to GIF

Save animation as GIF:

```r
render_gif(
  w,
  file = "animation.gif",
  fps = 25,
  width = 8, height = 6,      # Dimensions in inches
  units = "in",
  loop = 0                     # 0 = loop infinitely
)
```

## Examples

See the included vignette for comprehensive examples:

```r
vignette("ranim-introduction")
```

## Key Features

✨ **Hierarchical**: Parent-child relationships with automatic transformation inheritance

🎬 **Composable Actions**: Chain animations together with `@then()`

⏱️ **Flexible Timing**: Control speed, easing, and repetition

🎨 **Rich Shapes**: Rectangles, points, polygons, text, and lines

📊 **Dynamic Grids**: Spawn grids of shapes with staggered timing

💾 **State Snapshots**: Use trackers to save and restore animation checkpoints

🎯 **Precise Control**: Direct access to transformations for custom animations

## Documentation

Full function documentation is available after installation:

```r
?shape              # Base shape class
?window             # Animation canvas
?action             # Animation actions
?time               # Time control
?pos                # Position vectors
?transform          # Spatial transformations
?translate          # Translation action
?rotate             # Rotation action
?scale2             # Scaling action
?color              # Color transitions
?render_gif         # GIF export
```

## Architecture

The package uses [S7](https://rconsortium.github.io/S7/) for object-oriented programming, providing:
- Clear class hierarchies
- Method dispatch
- Property validation
- Efficient object copying

All animations operate on the principle of:

1. **Step**: Advance animation time by a delta
2. **Render**: Draw current state to graphics device
3. **Repeat**: Continue until all actions are complete

## License

MIT

## Author

Justin Landis

## Contributing

Contributions are welcome! Please open an issue or pull request on GitHub.

## See Also

- [S7 package](https://rconsortium.github.io/S7/) — Object system used by ranim
- [magick package](https://jeroen.r-universe.dev/magick) — GIF creation backend
- [vctrs package](https://vctrs.r-lib.org/) — Vectorization infrastructure
