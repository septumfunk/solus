#version 410 core

layout(location = 0) in vec3 vv3_pos;
layout(location = 1) in vec2 vv2_uv;
layout(location = 2) in vec4 vc_vcolor;

out vec2 fv2_uv;
out vec4 fc_vcolor;

uniform mat4 m_projection;
uniform mat4 m_campos;
uniform mat4 m_model;

void main() {
    gl_Position = m_projection * m_campos * m_model * vec4(vv3_pos, 1.0);
    fv2_uv = vv2_uv;
    fc_vcolor = vc_vcolor;
}