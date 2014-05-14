#version 430 core
vec2 sc = vec2(4., 4.);
vec2 zoom = vec2(1., 1.);
vec2 screen = vec2(1280,960);
vec2 center = vec2(-1,-0.75);
float step = 0.1;
int iter = 1000;
float thresh = 4.0;
//uniform vec2 fractal;
//uniform vec2 time;

out vec4 fColor;

void colorFunc (float colormap)
{
        fColor.g = 0.1*(sin(colormap * 1000.)) + 0.1;
        fColor.r = 200. * mod (colormap, 0.005) ;
        fColor.b = 0.002/colormap; 
        fColor.a = 1.0;
}

vec2 offset(vec2 p) {
	vec2 screen = vec2(640,480);
    p.x -= screen.x/2.0;
    p.y -= screen.y/2.0;
    return p;
}

vec2 scale(vec2 p) {
    p.x /= screen.x/sc.x;
    p.y /= screen.x/sc.y;
    p.x *= zoom.x;
    p.y *= zoom.y;
    p += center;
    return p;
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
    vec2 position = offset(gl_FragCoord.xy) * vec2(-1,1);
    vec2 complex = scale(position);
    float colormap = mandelbrot(complex, iter, step);
    if (colormap > 16) discard;
    colorFunc(colormap);
    gl_FragDepth = 0;
}



