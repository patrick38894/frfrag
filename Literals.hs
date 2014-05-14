module Literals where

data Lit            = Float Float
                    | Int Int
                    | Bool Bool
                    | Vec2 Vec2
                    | Vec3 Vec3
                    | Vec4 Vec4
                    | Mat2 Mat2
                    | Mat3 Mat3
                    | Mat4 Mat4
                    | Mat4x3 Mat4x3
                    | Mat3x4 Mat3x4
                    | Mat3x2 Mat3x2
                    | Mat2x3 Mat2x3
                    | IVec2 IVec2
                    | IVec3 IVec3
                    | IVec4 IVec4
                    | IMat2 IMat2
                    | IMat3 IMat3
                    | IMat4 IMat4
                    | IMat4x3 IMat4x3
                    | IMat3x4 IMat3x4
                    | IMat3x2 IMat3x2
                    | IMat2x3 IMat2x3
                    | BVec2 BVec2
                    | BVec3 BVec3
                    | BVec4 BVec4
                    | BMat2 BMat2
                    | BMat3 BMat3
                    | BMat4 BMat4
                    | BMat4x3 BMat4x3
                    | BMat3x4 BMat3x4
                    | BMat3x2 BMat3x2
                    | BMat2x3 BMat2x3
                    deriving Show

type GVec2 a        = (a, a)
type GVec3 a        = (a, a, a)
type GVec4 a        = (a, a, a, a)
type GMat2 a        = (GVec2 a, GVec2 a)
type GMat3 a        = (GVec3 a, GVec3 a, GVec3 a)
type GMat4 a        = (GVec4 a, GVec4 a, GVec4 a, GVec4 a)
type GMat4x3 a      = (GVec3 a, GVec3 a, GVec3 a, GVec3 a)
type GMat3x4 a      = (GVec4 a, GVec4 a, GVec4 a)
type GMat3x2 a      = (GVec2 a, GVec2 a, GVec2 a)
type GMat2x3 a      = (GVec3 a, GVec3 a)
type Vec2           = GVec2 Float
type Vec3           = GVec3 Float
type Vec4           = GVec4 Float
type Mat2           = GMat2 Float
type Mat3           = GMat3 Float
type Mat4           = GMat4 Float
type Mat4x3         = GMat4x3 Float
type Mat3x4         = GMat3x4 Float
type Mat3x2         = GMat3x2 Float 
type Mat2x3         = GMat2x3 Float
type IVec2          = GVec2 Int
type IVec3          = GVec3 Int
type IVec4          = GVec4 Int
type IMat2          = GMat2 Int
type IMat3          = GMat3 Int
type IMat4          = GMat4 Int
type IMat4x3        = GMat4x3 Int
type IMat3x4        = GMat3x4 Int
type IMat3x2        = GMat3x2 Int 
type IMat2x3        = GMat2x3 Int
type BVec2          = GVec2 Bool
type BVec3          = GVec3 Bool
type BVec4          = GVec4 Bool
type BMat2          = GMat2 Bool
type BMat3          = GMat3 Bool
type BMat4          = GMat4 Bool
type BMat4x3        = GMat4x3 Bool
type BMat3x4        = GMat3x4 Bool
type BMat3x2        = GMat3x2 Bool 
type BMat2x3        = GMat2x3 Bool
