module Synonyms where
import Expressions
import Vector

float = FloaT
int = IntT
bool = BoolT
vec = VecT
mat = MatT

vec2  = vec float N2 
vec3  = vec float N3 
vec4  = vec float N4 
ivec2 = vec int N2 
ivec3 = vec int N3 
ivec4 = vec int N4 
bvec2 = vec bool N2
bvec3 = vec bool N3
bvec4 = vec bool N4


true = Bool True
false = Bool False
