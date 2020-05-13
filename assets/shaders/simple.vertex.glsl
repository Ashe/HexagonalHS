#version 430 core
layout(location = 0) in vec3 inPosition;
uniform mat4 transform;
uniform mat4 view;
uniform mat4 projection;
void main() {
  vec4 vPosition = vec4(inPosition, 1.0);
  gl_Position = projection * view * transform * vPosition;
}
