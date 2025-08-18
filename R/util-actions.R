rotate <- into_action(obj_rotate, expr_map(degrees, degrees * time@delta))

scale <- into_action(obj_scale, expr_map(size, size * time@delta))
