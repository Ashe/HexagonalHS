#version 430 core
layout(location = 0) in vec3 inPosition;
uniform mat4 transform;
uniform mat4 view;
uniform mat4 projection;
uniform vec3 tiles[1000];
void main() {

  // Get the position of the vertex
  vec4 vPosition = vec4(inPosition, 1.0);

  // Calculate translation matrix for current primitive
  vec3 tile = tiles[gl_InstanceID];
  float visible = clamp(tile.z, 0, 1);
  mat4 tileM = mat4(1.0, 0.0, 0.0, 0.0,
                    visible, tile.z, 0.0, 0.0,
                    0.0, 0.0, visible, 0.0,
                    tile.x, 0.0, tile.y, 1.0);

  // Calculate final position
  gl_Position = projection * view * transform * tileM * vPosition;
}
