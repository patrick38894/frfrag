// Mandelbrot fractal shader
// Originally by Patrick Romero
// Modified by Forrest Alexander

uniform vec2 zoom = vec2(4.0,4.0);
uniform vec2 screen = vec2(1280.0,960.0);
uniform vec2 center = vec2(0.0,0.0);
uniform float step = 0.01;
uniform float thresh = 8.0;
uniform int iter = 1000;
vec2 offset(vec2 p)
{
    return (p - screen / 2.0);
}
vec2 scale(vec2 p)
{
    return (p * zoom / screen + center);
}
vec2 complexMult(vec2 a, vec2 b)
{
    return vec2((a.x * b.x - a.y * b.y), (a.x * b.y + a.y * b.x));
}
float mandelbrot
(vec2 c, int n, float step)
{
    float sum = 0.0;
    vec2 z = c;
    for(int i = 0; (i < n); i = (1 + i))
    {
        z = complexMult(z,z) + c;
        if((thresh < dot(z, z))) break;
        sum = (sum + step);
    }
    return sum;
}
vec4 colormap(float x)
{
    return vec4(x * 2.0, (1.0 - x * 10.0), 0.5, 1.0);
}
void main()
{
    gl_FragColor = colormap(mandelbrot(scale(offset(gl_FragCoord.xy)),
                                       iter,
                                       step));
}
