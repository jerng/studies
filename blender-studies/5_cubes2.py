import bpy

# boilerplate
mylayers = [False]*20
mylayers[0] = True

# shorthanding
add_cube = bpy.ops.mesh.primitive_cube_add

# add five cubes with variation
for index in range(0, 5):
    add_cube(   location=(index*3, 0, 0)
            ,   layers=mylayers
            ,   rotation=(index/5,index/10,index/15)
            )