# from http://blenderartists.org/forum/showthread.php?264781-bpy-ops-material-new()
import bpy

context = bpy.context

#using ops 

bpy.ops.material.new()
new_mat = bpy.data.materials[-1]  # the new material is the last one in the list
new_mat.name = "NAME"

# using API

new_mat = bpy.data.materials.new("NAME")
# because there is already a material with name NAME from the op this will have name NAME.00x 
print(new_mat.name)
# setting the diffuse color

new_mat.diffuse.color = (0,0,0) # black


# assigning the new material to the active object
# assume you have a mesh object as context.object
# using ops

bpy.ops.object.material_slot_add()

# the new slot will be the active one 

context.object.active_material = new_mat


# using API
# add the material to the mesh's materials

mesh = context.object.data
mesh.materials.append(new_mat)
