# Penrose-tile-with-Beveling

Penrose tile with Beveling.

Generates tilings, adds type to them and adds curves to eliminate hard edges.

Made with Godot and shaders.

PenroseTile.gdshader is both godot shader and compute shader.

All code is in single file because of static_init/init bug.

## Preview

[![texture, no border](readme/texture-no-border.png)](readme/texture-no-border.png)
[![texture, border](readme/texture-border.png)](readme/texture-border.png)
[![no texture, border](readme/no-texture-border.png)](readme/no-texture-border.png)


## Demo usage

[![demo preview](readme/demo-preview.png)](readme/demo-preview.png)
Left side contain sliders to control camera position and zoom.
Right side toggles 
* texture or plain color
* border
* perpendicular lines from rhomb center to its opposite side
* Coloring opposite non-center rhomb points.

Right side sliders control position of current area or reader point.
Moving them will change 
* "Bigger area" is big yellow texture area
* "Single area" is small radius of blue texture
* "Random area" is area of random textures, seed is its position
* "Point reader" is single point calculated using compute shader
