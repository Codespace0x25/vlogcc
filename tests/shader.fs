#version 460

out vec4 fragColor;

void main()
{
   
    vec2 uv = gl_FragCoord.xy / vec2(600.0, 700.0); 

    vec4 color1 = vec4(1.0, 0.0, 0.0, 1.0);
    vec4 color2 = vec4(1.0, 1.0, 0.0, 1.0); 
    vec4 color3 = vec4(0.0, 1.0, 0.0, 1.0); 
    vec4 color4 = vec4(0.0, 0.0, 0.0, 1.0); 

    if (uv.y < 0.5) {
        fragColor = mix(color1, color2, uv.y * 2.0);
    } else {
        fragColor = mix(color3, color4, (uv.y - 0.5) * 2.0);
    }

    if (uv.x < 0.5) {
        fragColor = mix(fragColor, color1, uv.x * 2.0);
    } else {
        fragColor = mix(fragColor, color4, (uv.x - 0.5) * 2.0);
    }
}
