#version 150

#define PI 3.141592654
#define TWOPI 6.28

uniform float u_time;
out vec4 fragColor;

#define M_PI 3.1415926535897932384626433832795

float rand(vec2 co)
{
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

mat3 rotateX(float theta) {
    float c = cos(theta);
    float s = sin(theta);
    return mat3(
        vec3(1, 0, 0),
        vec3(0, c, -s),
        vec3(0, s, c)
    );
}

mat3 rotateY(float theta) {
    float c = cos(theta);
    float s = sin(theta);
    return mat3(
        vec3(c, 0, s),
        vec3(0, 1, 0),
        vec3(-s, 0, c)
    );
}

mat3 rotateZ(float theta) {
    float c = cos(theta);
    float s = sin(theta);
    return mat3(
        vec3(c, -s, 0),
        vec3(s, c, 0),
        vec3(0, 0, 1)
    );
}

// Distance from p to plane (at y = 0)
float sdPlane( vec3 p )
{
    return p.y;
}

float sdSphere( vec3 p, float s ) {
    return length(p) - s;
}

// Distance from p to horizontal torus with major radius t.x and minor radius t.y
float sdTorus( vec3 p, vec2 t )
{
    return length( vec2(length(p.xz)-t.x, p.y)) - t.y;
}


// Non-Euclidean distance function, so the sphere "length8(p) = k" is more squarish.
float length8( vec2 p )
{
    p = p*p; p = p*p; p = p*p;
    return pow( p.x + p.y, 1.0/8.0 );
}


float sdTorus88( vec3 p, vec2 t )
{
    vec2 q = vec2(length8(p.xz)-t.x,p.y);
    return length8(q)-t.y;
}

float sdDiamond(vec3 p, vec3 h ) // h = { cos a, sin a, height }
{
    // Tetrahedron = Octahedron - Cube
   // float box = sdBox( p - vec3(0,-2.0*h.z,0), vec3(2.0*h.z) );
 
    float d = 0.0;
    d = max( d, abs( dot(p, vec3( -h.x, h.y, 0 )) ));
    d = max( d, abs( dot(p, vec3(  h.x, h.y, 0 )) ));
    d = max( d, abs( dot(p, vec3(  0, h.y, h.x )) ));
    d = max( d, abs( dot(p, vec3(  0, h.y,-h.x )) ));
    float octa = d - h.z;
    return octa; // Subtraction
 }

float sdCylinder(vec3 p, float h, float r) {
    // How far inside or outside the cylinder the point is, radially
    float inOutRadius = length(p.xy) - r;
    
    // How far inside or outside the cylinder is, axially aligned with the cylinder
    float inOutHeight = abs(p.z) - h/2.0;
    
    // Assuming p is inside the cylinder, how far is it from the surface?
    // Result will be negative or zero.
    float insideDistance = min(max(inOutRadius, inOutHeight), 0.0);
    // Assuming p is outside the cylinder, how far is it from the surface?
    // Result will be positive or zero.
    float outsideDistance = length(max(vec2(inOutRadius, inOutHeight), 0.0));
    
    return insideDistance + outsideDistance;
}

// Distance from p to box whose half-dimensions are b.x, b.y, b.z
float sdBox( vec3 p, vec3 b )
{
    vec3 d = abs(p) - b;
    return min(max(d.x,max(d.y,d.z)),0.0) + length(max(d,0.0));
}

// union primitives 1 and 2
// d1 is a vec2 where .x is the distance, and .y is the color/material code.
vec2 opU( vec2 d1, vec2 d2 )
{
    return (d1.x<d2.x) ? d1 : d2;
}

// subtract primitive 2 from primitive 1, where d1 is distance to primitive 1.
float opS( float d1, float d2 )
{
    return max(-d2,d1);
}

// intersection of two primitives
float opI( float d1, float d2 )
{
    return max(d1, d2);
}

// Return (x,y) where x is minimum distance from pos to objects in the scene, and
// y is the material of the closest object.
vec2 map( in vec3 pos ) {
    // vec2 res = opU( vec2( sdPlane(pos), 1.0 ), 
    //                 vec2( sdSphere(pos - vec3( 0.0, 0.25, 0.0), 0.25 ), 46.9 )
    //               );

        float sphere = sdSphere(pos, 0.75 + sin(u_time));
        float cube = sdBox(pos, vec3(0.6, 0.6, 0.6));

        float height = 3.0;
        float cylinderRadius = 0.3;
        float cylinderRadius2 = 0.15;
        float cylinder = sdCylinder( pos, height, cylinderRadius);
        float cylinder2 = sdCylinder(pos, height, cylinderRadius2);

        float sphereCubeIntersection = opI(cube, sphere);
        float difference = opS( sphereCubeIntersection, cylinder );
        vec2 res = opU( vec2(difference, 0.0), vec2(cylinder2, 0.0) );

        vec3 planet_1_pos = rotateY(u_time / 1.5) * pos;
        vec3 planet_2_pos = rotateY(u_time / 2.0) * pos;
        vec3 planet_3_pos = rotateY(u_time / 3.5) * pos;

        // PLANET #1 (BUMPY ASTEROID)
        float sphere2 = sdSphere( planet_1_pos + vec3(1.5, 0.0, 0.0), 0.3) + 0.05*sin(25.0*planet_1_pos.x)*sin(25.0*planet_1_pos.y)*sin(15.0*planet_1_pos.z);
        res = opU(res, vec2(sphere2, 1.0));

        // PLANET #2 (LOOKS LIKE SATURN)
        float torus = sdTorus(planet_2_pos-vec3(2.5, 0.0, 0.0), vec2(0.5,0.05) );
        res = opU(res, vec2(torus, 2.0));

        float torusSphere = sdSphere(planet_2_pos-vec3(2.5, 0.0, 0.0), 0.2);
        res = opU(res, vec2(torusSphere, 2.5));

        // PLANET #3 (DIAMOND)
        float diamond = sdDiamond( planet_3_pos - vec3( 4.0, 0.0, 0.0), vec3(1.0, 0.4, 0.2) );
        res = opU( res, vec2(diamond, 3.0) );

        float torus88 = sdTorus88( planet_3_pos - vec3(4.0, 0.0, 0.0), vec2(0.5, 0.025));
        res = opU( res, vec2(torus88, 3.0));

        return res;
}

// RAYMARCHING ALGORITHM
// http://jamie-wong.com/2016/07/15/ray-marching-signed-distance-functions/
// Cast a ray from origin ro in direction rd until it hits an object.
// Return (t,m) where t is distance traveled along the ray, and m
// is the material of the object hit.
vec2 castRay( in vec3 ray_origin, in vec3 ray_direction ) {
    const float DEPTH_MIN = 1.0;
    const float DEPTH_MAX = 20.0;
    const int MAX_MARCHING_STEPS = 100;

    float depth = DEPTH_MIN;
    float material = -1.0;

    for (int i = 0; i < MAX_MARCHING_STEPS; i++) {
        float EPSILON = 0.0005 * depth;
        vec2 res = map( ray_origin + ray_direction * depth );
        if ( res.x < EPSILON || depth > DEPTH_MAX ) break;
        // Move along the view ray.
        depth += res.x;
        material = res.y;
    }

    if ( depth > DEPTH_MAX ) material = -1.0;
    return vec2( depth, material );
}

// TODO: FIGURE OUT HOW THIS WORKS
// Compute normal vector to surface at pos, using central differences method?
vec3 calculateNormal( in vec3 pos ) {
    vec2 EPSILON = vec2(1.0, -1.0) * 0.5773 * 0.0005;
    return normalize( EPSILON.xyy*map( pos + EPSILON.xyy ).x + 
                      EPSILON.yyx*map( pos + EPSILON.yyx ).x + 
                      EPSILON.yxy*map( pos + EPSILON.yxy ).x + 
                      EPSILON.xxx*map( pos + EPSILON.xxx ).x  );
}

vec3 rgbtocolor( in vec3 rgb ) {
    return rgb / 255.0;
}

// Return a color value when casting ray from origin to direction
vec3 render( in vec3 ray_origin, in vec3 ray_direction ) {

    // Background color gradient.
    vec3 color = vec3(0.0, 0.0, 0.0) + ray_direction.y * 0.15;

    vec2 result = castRay( ray_origin, ray_direction );
    
    float distance = result.x;
    float material = result.y;

    if ( material > -0.5 ) {
        vec3 position = ray_origin + distance * ray_direction;
        vec3 normal = calculateNormal( position );
        vec3 reflected = reflect( ray_direction, normal );

        float v1 = sin(position.x + u_time);
        float v2 = sin(position.y + u_time);
        float v3 = sin(position.x + position.y + u_time);
        float v4 = sin(length(position) + 1.7 * u_time);
        float v = v1 + v2 + v3 + v4;

        // Material Color
       if (material == 0.0) {
       // color = vec3(1.0, 1.0, 0.0);
        color = rgbtocolor(vec3(255.0, 204.0, 0.0));
        color += vec3(cos(1. * v), cos(1. * v), cos(1. * v));
       }
       if (material == 1.0) {
        // color = vec3(1.0, 0.0, 0.0);
        color = rgbtocolor(vec3(255, 128, 0));
       }
       else if (material == 2.0) {
        color = rgbtocolor(vec3(255.0, 255.0, 100.0));
       }
       else if (material == 2.5) {
        color = rgbtocolor(vec3(200.0, 200.0, 153.0));
       }
       else if (material == 3.0) {
        color = rgbtocolor(vec3(0.0, 32.0, 128.0));
       }

        // if (material == 200.0) {
        //     color = 0.45 + 0.35 * sin( vec3(0.05, 0.08, 0.10) * ( material - 1.0) );
        //     color += vec3(cos(1. * v), cos(1. * v), cos(1. * v));
        // }


        // Light Source
        vec3 light = normalize( vec3(-0.4 * sin(u_time), 0.7, -0.6 * cos(u_time)) );
        
        // Phong Shading
        float ambient = clamp( 0.5 + 0.5 * normal.y, 0.0, 1.0 );
        float diffuse = clamp( dot( normal, light), 0.0, 1.0 );
        float specular = pow(clamp(dot(reflected, light), 0.0, 1.0), 16.0);

        // Compute Lighting
        vec3 lin = vec3(0.0);
        lin += 1.30 * diffuse * vec3(1.00, 0.80, 0.55);
        lin += 2.00 * specular * vec3(1.00, 0.90, 0.70) * diffuse;
        lin += 0.40 * ambient * vec3(0.40, 0.60, 1.00);
        color = color * lin;
    }


    return vec3( clamp(color, 0.0, 1.0) );
}

// https://www.scratchapixel.com/lessons/mathematics-physics-for-computer-graphics/lookat-function
// Return a transformation matrix that will transform a ray from view space
// to world coordinates, given the eye point, the camera target, and an up vector.
// This assumes that the center of the camera is aligned with the negative z axis in
// view space when calculating the ray marching direction.

mat3 setCamera( in vec3 origin, in vec3 target, float roll ) {
    // Compute forward axis.
    vec3 cw = normalize( target - origin );
    vec3 cp = vec3( sin(roll), cos(roll), 0.0 );
    // Compute right vector.
    vec3 cu = normalize( cross(cw, cp) );
    // Compute up vector.
    vec3 cv = normalize( cross(cu, cw) );
    // Set 3x3 matrix using the right, up, and forward vector as from point.
    return mat3( cu, cv, cw );
}

void main()
{

    // Set up time variable t passed in as a uniform.
    float t = u_time;

    // Set up window coordinates and resolution.
    float window_width = 1028.0;
    float window_height = 768.0;
    vec2 u_resolution = vec2(window_width, window_height);
    
    // Set up pixel coordinates.
    vec2 pixel = (-u_resolution.xy + 2.0 * gl_FragCoord.xy) / u_resolution.y;

    // Set up mouse coordinates for interaction.
    // TODO

    // Set up camera origin and target.
    vec3 camera_origin = vec3(0.0, 1.0, 8.0 );
    vec3 camera_target = vec3(0.0, 0.0, 0.0);
    float camera_roll = 0.0;

    // Camera-to-world matrix transformation.
    mat3 camera = setCamera( camera_origin, camera_target, camera_roll );

    vec3 camera_direction = camera * normalize( vec3(pixel.xy, 2.0) );


    float size = 30.0;
    float prob = 0.95;
    
    vec2 pos = floor(1.0 / size * gl_FragCoord.xy);
    
    float c = 0.0;
    float starValue = rand(pos);
    
    if (starValue > prob)
    {
        vec2 center = size * pos + vec2(size, size) * 0.5;
        
        float t = 0.9 + 0.2 * sin(u_time + (starValue - prob) / (1.0 - prob) * 45.0);
                
        c = 1.0 - distance(gl_FragCoord.xy, center) / (0.5 * size);
        c = c * t / (abs(gl_FragCoord.y - center.y)) * t / (abs(gl_FragCoord.x - center.x));
    }
    else if (rand(gl_FragCoord.xy / u_resolution.xy) > 0.996)
    {
        float r = rand(gl_FragCoord.xy);
        c = r * (0.25 * sin(u_time * (r * 5.0) + 720.0 * r) + 0.75);
    }

    vec3 color = vec3(0.0, 0.0, 0.0);

    color += vec3(c);

    vec3 planetsColor = render( camera_origin, camera_direction );

    if (length(planetsColor) > 0.1) {
        color = planetsColor;
    } else {
        color += planetsColor;
    }

    // Gamma correction.
    // http://iquilezles.org/www/articles/outdoorslighting/outdoorslighting.htm
    color = pow( color, vec3(0.4545) );

    fragColor = vec4( color, 1.0 );
}