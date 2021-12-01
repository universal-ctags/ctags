# Taken from godot-demo-projects/3d/voxel/world/chunk.gd
func _create_block_collider(block_sub_position):
        var collider = CollisionShape.new()
        collider.shape = BoxShape.new()
        collider.shape.extents = Vector3.ONE / 2
        collider.transform.origin = block_sub_position + Vector3.ONE / 2
        add_child(collider)

static func calculate_block_uvs(block_id):
        # This method only supports square texture sheets.
        var row = block_id / TEXTURE_SHEET_WIDTH
        var col = block_id % TEXTURE_SHEET_WIDTH

        return [
                TEXTURE_TILE_SIZE * Vector2(col, row),
                TEXTURE_TILE_SIZE * Vector2(col, row + 1),
                TEXTURE_TILE_SIZE * Vector2(col + 1, row),
                TEXTURE_TILE_SIZE * Vector2(col + 1, row + 1),
        ]

func id(a):
	return a

remote func r(b):
	return b

remote static func x(c):
	return c
