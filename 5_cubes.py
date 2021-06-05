# from some website

import bpy

mylayers = [False]*20
mylayers[0] = True

add_cube = bpy.ops.mesh.primitive_cube_add
for index in range(0, 5):
    add_cube(location=(index*3, 0, 0), layers=mylayers)
