#version 410 core

in vec2 fv2_uv;
in vec4 fc_vcolor;
out vec4 oc_frag;

uniform sampler2D t_sampler;

void main() {
    vec4 tex_color = texture(t_sampler, fv2_uv);
    oc_frag = tex_color * fc_vcolor;
    if (oc_frag.a < 0.01) discard;
}