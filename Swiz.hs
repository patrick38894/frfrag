module Swiz where
import Vector
class Swiz repr where
    x       :: Vec   v => repr (v a) -> repr a
    y       :: Vec2p v => repr (v a) -> repr a
    z       :: Vec3p v => repr (v a) -> repr a
    w       :: Vec4p v => repr (v a) -> repr a
    xx      :: Vec   v => repr (v a) -> repr (V2 a)
    xy      :: Vec2p v => repr (v a) -> repr (V2 a)
    xz      :: Vec3p v => repr (v a) -> repr (V2 a)
    xw      :: Vec4p v => repr (v a) -> repr (V2 a)
    yw      :: Vec4p v => repr (v a) -> repr (V2 a) 
    yx      :: Vec2p v => repr (v a) -> repr (V2 a)
    yy      :: Vec2p v => repr (v a) -> repr (V2 a)
    yz      :: Vec3p v => repr (v a) -> repr (V2 a)
    zw      :: Vec4p v => repr (v a) -> repr (V2 a)
    zx      :: Vec3p v => repr (v a) -> repr (V2 a)
    zy      :: Vec3p v => repr (v a) -> repr (V2 a)
    zz      :: Vec3p v => repr (v a) -> repr (V2 a)
    wx      :: Vec4p v => repr (v a) -> repr (V2 a)
    wy      :: Vec4p v => repr (v a) -> repr (V2 a) 
    wz      :: Vec4p v => repr (v a) -> repr (V2 a)
    ww      :: Vec4p v => repr (v a) -> repr (V2 a)
    xxx     :: Vec v   => repr (v a) -> repr (V3 a) 
    xxy     :: Vec2p v => repr (v a) -> repr (V3 a) 
    xxz     :: Vec3p v => repr (v a) -> repr (V3 a) 
    xxw     :: Vec4p v => repr (v a) -> repr (V3 a) 
    xyx     :: Vec2p v => repr (v a) -> repr (V3 a) 
    xyy     :: Vec2p v => repr (v a) -> repr (V3 a) 
    xyz     :: Vec3p v => repr (v a) -> repr (V3 a) 
    xyw     :: Vec4p v => repr (v a) -> repr (V3 a)
    xzx     :: Vec3p v => repr (v a) -> repr (V3 a) 
    xzy     :: Vec3p v => repr (v a) -> repr (V3 a) 
    xzz     :: Vec3p v => repr (v a) -> repr (V3 a) 
    xzw     :: Vec4p v => repr (v a) -> repr (V3 a) 
    xwx     :: Vec4p v => repr (v a) -> repr (V3 a) 
    xwy     :: Vec4p v => repr (v a) -> repr (V3 a) 
    xwz     :: Vec4p v => repr (v a) -> repr (V3 a) 
    xww     :: Vec4p v => repr (v a) -> repr (V3 a)
    yxx     :: Vec2p v => repr (v a) -> repr (V3 a) 
    yxy     :: Vec2p v => repr (v a) -> repr (V3 a) 
    yxz     :: Vec3p v => repr (v a) -> repr (V3 a) 
    yxw     :: Vec4p v => repr (v a) -> repr (V3 a) 
    yyx     :: Vec2p v => repr (v a) -> repr (V3 a) 
    yyy     :: Vec2p v => repr (v a) -> repr (V3 a) 
    yyz     :: Vec3p v => repr (v a) -> repr (V3 a) 
    yyw     :: Vec4p v => repr (v a) -> repr (V3 a)
    yzx     :: Vec3p v => repr (v a) -> repr (V3 a) 
    yzy     :: Vec3p v => repr (v a) -> repr (V3 a) 
    yzz     :: Vec3p v => repr (v a) -> repr (V3 a) 
    yzw     :: Vec4p v => repr (v a) -> repr (V3 a) 
    ywx     :: Vec4p v => repr (v a) -> repr (V3 a) 
    ywy     :: Vec4p v => repr (v a) -> repr (V3 a) 
    ywz     :: Vec4p v => repr (v a) -> repr (V3 a) 
    yww     :: Vec4p v => repr (v a) -> repr (V3 a)
    zxx     :: Vec3p v => repr (v a) -> repr (V3 a) 
    zxy     :: Vec3p v => repr (v a) -> repr (V3 a) 
    zxz     :: Vec3p v => repr (v a) -> repr (V3 a) 
    zxw     :: Vec4p v => repr (v a) -> repr (V3 a) 
    zyx     :: Vec2p v => repr (v a) -> repr (V3 a) 
    zyy     :: Vec3p v => repr (v a) -> repr (V3 a) 
    zyz     :: Vec3p v => repr (v a) -> repr (V3 a) 
    zyw     :: Vec4p v => repr (v a) -> repr (V3 a)
    zzx     :: Vec3p v => repr (v a) -> repr (V3 a) 
    zzy     :: Vec3p v => repr (v a) -> repr (V3 a) 
    zzz     :: Vec3p v => repr (v a) -> repr (V3 a) 
    zzw     :: Vec4p v => repr (v a) -> repr (V3 a) 
    zwx     :: Vec4p v => repr (v a) -> repr (V3 a) 
    zwy     :: Vec4p v => repr (v a) -> repr (V3 a) 
    zwz     :: Vec4p v => repr (v a) -> repr (V3 a) 
    zww     :: Vec4p v => repr (v a) -> repr (V3 a)
    wxx     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wxy     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wxz     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wxw     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wyx     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wyy     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wyz     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wyw     :: Vec4p v => repr (v a) -> repr (V3 a)
    wzx     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wzy     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wzz     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wzw     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wwx     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wwy     :: Vec4p v => repr (v a) -> repr (V3 a) 
    wwz     :: Vec4p v => repr (v a) -> repr (V3 a) 
    www     :: Vec4p v => repr (v a) -> repr (V3 a)
    xxxx    :: Vec   v => repr (v a) -> repr (V4 a) 
    xxxy    :: Vec2p v => repr (v a) -> repr (V4 a) 
    xxxz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xxxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xxyx    :: Vec2p v => repr (v a) -> repr (V4 a) 
    xxyy    :: Vec2p v => repr (v a) -> repr (V4 a) 
    xxyz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xxyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    xxzx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xxzy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xxzz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xxzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xxwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xxwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xxwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xxww    :: Vec4p v => repr (v a) -> repr (V4 a)
    xyxx    :: Vec2p v => repr (v a) -> repr (V4 a) 
    xyxy    :: Vec2p v => repr (v a) -> repr (V4 a) 
    xyxz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xyxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xyyx    :: Vec2p v => repr (v a) -> repr (V4 a) 
    xyyy    :: Vec2p v => repr (v a) -> repr (V4 a) 
    xyyz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xyyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    xyzx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xyzy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xyzz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xyzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xywx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xywy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xywz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xyww    :: Vec4p v => repr (v a) -> repr (V4 a)
    xzxx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xzxy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xzxz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xzxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xzyx    :: Vec2p v => repr (v a) -> repr (V4 a) 
    xzyy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xzyz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xzyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    xzzx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xzzy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xzzz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    xzzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xzwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xzwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xzwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xzww    :: Vec4p v => repr (v a) -> repr (V4 a)
    xwxx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwxy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwxz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwyx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwyy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwyz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    xwzx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwzy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwzz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    xwww    :: Vec4p v => repr (v a) -> repr (V4 a)   
    yxxx    :: Vec2p v => repr (v a) -> repr (V4 a) 
    yxxy    :: Vec2p v => repr (v a) -> repr (V4 a) 
    yxxz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yxxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yxyx    :: Vec2p v => repr (v a) -> repr (V4 a) 
    yxyy    :: Vec2p v => repr (v a) -> repr (V4 a) 
    yxyz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yxyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    yxzx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yxzy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yxzz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yxzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yxwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yxwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yxwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yxww    :: Vec4p v => repr (v a) -> repr (V4 a)
    yyxx    :: Vec2p v => repr (v a) -> repr (V4 a) 
    yyxy    :: Vec2p v => repr (v a) -> repr (V4 a) 
    yyxz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yyxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yyyx    :: Vec2p v => repr (v a) -> repr (V4 a) 
    yyyy    :: Vec2p v => repr (v a) -> repr (V4 a) 
    yyyz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yyyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    yyzx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yyzy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yyzz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yyzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yywx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yywy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yywz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yyww    :: Vec4p v => repr (v a) -> repr (V4 a)
    yzxx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yzxy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yzxz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yzxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yzyx    :: Vec2p v => repr (v a) -> repr (V4 a) 
    yzyy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yzyz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yzyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    yzzx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yzzy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yzzz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    yzzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yzwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yzwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yzwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    yzww    :: Vec4p v => repr (v a) -> repr (V4 a)
    ywxx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywxy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywxz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywyx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywyy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywyz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    ywzx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywzy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywzz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    ywww    :: Vec4p v => repr (v a) -> repr (V4 a)   
    zxxx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zxxy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zxxz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zxxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zxyx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zxyy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zxyz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zxyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    zxzx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zxzy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zxzz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zxzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zxwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zxwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zxwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zxww    :: Vec4p v => repr (v a) -> repr (V4 a)
    zyxx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zyxy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zyxz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zyxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zyyx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zyyy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zyyz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zyyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    zyzx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zyzy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zyzz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zyzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zywx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zywy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zywz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zyww    :: Vec4p v => repr (v a) -> repr (V4 a)
    zzxx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zzxy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zzxz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zzxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zzyx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zzyy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zzyz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zzyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    zzzx    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zzzy    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zzzz    :: Vec3p v => repr (v a) -> repr (V4 a) 
    zzzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zzwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zzwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zzwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zzww    :: Vec4p v => repr (v a) -> repr (V4 a)
    zwxx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwxy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwxz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwyx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwyy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwyz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    zwzx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwzy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwzz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    zwww    :: Vec4p v => repr (v a) -> repr (V4 a)   
    wxxx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxxy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxxz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxyx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxyy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxyz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    wxzx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxzy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxzz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wxww    :: Vec4p v => repr (v a) -> repr (V4 a)
    wyxx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyxy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyxz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyyx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyyy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyyz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    wyzx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyzy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyzz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wywx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wywy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wywz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wyww    :: Vec4p v => repr (v a) -> repr (V4 a)
    wzxx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzxy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzxz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzyx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzyy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzyz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    wzzx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzzy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzzz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wzww    :: Vec4p v => repr (v a) -> repr (V4 a)
    wwxx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwxy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwxz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwxw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwyx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwyy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwyz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwyw    :: Vec4p v => repr (v a) -> repr (V4 a)
    wwzx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwzy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwzz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwzw    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwwx    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwwy    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwwz    :: Vec4p v => repr (v a) -> repr (V4 a) 
    wwww    :: Vec4p v => repr (v a) -> repr (V4 a)   
