#version 430 core
vec2 zoom = vec2(4., 4.);
vec2 screen = vec2(1280,960);
vec2 center = vec2(0,0);
float step = 0.01;
float thresh = 8.0;
int iter = 1000;

out vec4 fColor;

vec4 colorFunc (float colormap)
{
        return vec4 (colormap * 2,
                     1 - colormap * 10,
                     0.5,
                     1.0);
}

vec2 offset(vec2 p) {
    return p - screen/2;
}

vec2 scale(vec2 p) {
    return p * zoom / screen + center;
}


vec2 complexmult(vec2 a, vec2 b) {
    return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

float mandelbrot(vec2 c, int i, float step) {
    float sum = 0.0;
    vec2 z = c;
    for (int n = 0; n < i; ++n) {
        z = complexmult(z,z) + c;
        if (z.x * z.x + z.y * z.y >= thresh)
            break;
        sum += step;
    }
    return sum;
}

void main (void)
{
    fColor = colorFunc(mandelbrot(scale(offset(gl_FragCoord.xy)), iter, step));
}



