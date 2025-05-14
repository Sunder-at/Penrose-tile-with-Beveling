@tool
extends Node3D

class_name PenroseTile

@export var redo_ready := false: set = set_redo_ready
@export var marker_pos: Vector2: get = _get_marker_pos, set = _set_marker_pos
@export var marker_single_pos: Vector2: get = _get_marker_single_pos, set = _set_marker_single_pos
@export var marker_random_pos: Vector2: get = _get_marker_random_pos, set = _set_marker_random_pos
@export var pixel_reader_pos: Vector2: get = _get_pixel_reader_pos, set = _set_pixel_reader_pos
@export var pixel_color: Color: get = _get_pixel_color, set = _set_pixel_color

@onready var texture_dirt := preload("res://img/dirt.png")
@onready var texture_yellow := preload("res://img/yellow_tile.png")
@onready var texture_blue := preload("res://img/blue_tile.png")
@onready var texture_grass := preload("res://img/grass.png")
@onready var texture_purpur := preload("res://img/purpur.png")
@onready var texture_metal := preload("res://img/metal.png")
@onready var texture_reddish := preload("res://img/reddish.png")

@onready var penrose_shader := preload("res://PenroseTile.gdshader")

@onready var marker := $PenroseTile/Marker3D as Marker3D
@onready var marker_single := $PenroseTile/Marker_Single as Marker3D
@onready var marker_random := $PenroseTile/MarkRand as Marker3D
@onready var pixel_reader := $PenroseTile/PixelReader as Marker3D
@onready var penrose_tile := $PenroseTile as MeshInstance3D

var penr_cons: PenroseConstruct
var ground_colors: PackedColorArray = []
var gr_point : GroundPoint
var penrose_shader_code : String
var selected_marker: int

func _ready() -> void:
	get_penrose_mesh()
	
	gr_point = GroundPoint.new(penr_cons, penrose_shader)
	gr_point.update_configs()
	marker_pos = marker_pos
	marker_single_pos = marker_single_pos
	marker_random_pos = marker_random_pos
	pixel_reader_pos = pixel_reader_pos
	penrose_shader_code = penrose_shader.code
	_get_pixel_color()
	_gui_signals()
	pass


func _gui_signals() -> void:
	var cam := $Camera3D as Camera3D
	
	($GUI/Camera/PositionX as VSlider).set_value_no_signal(1.-(-cam.position.x / 22.))
	($GUI/Camera/PositionY as HSlider).set_value_no_signal(1.-( cam.position.z / 22.))
	($GUI/Camera/Zoom as VSlider).set_value_no_signal( 1.-((cam.size - .1) / 15.) )
	
	($GUI/Camera/PositionY as HSlider).value_changed.connect(Camera_PositionY)
	($GUI/Camera/PositionX as VSlider).value_changed.connect(Camera_PositionX)
	($GUI/Camera/Zoom as VSlider).value_changed.connect(Camera_Zoom)
	Camera_Zoom(0.)
	
	($GUI/Shader/Texture as CheckButton).toggled.connect(Shader_Define)
	($GUI/Shader/Borders as CheckButton).toggled.connect(Shader_Define)
	($GUI/Shader/Perp as CheckButton).toggled.connect(Shader_Define)
	($GUI/Shader/ColorYZ as CheckButton).toggled.connect(Shader_Define)
	Shader_Define(true)
	
	($GUI/Area/Bigger as Button).toggled.connect(Area_Change.bind($GUI/Area/Bigger))
	($GUI/Area/Single as Button).toggled.connect(Area_Change.bind($GUI/Area/Single))
	($GUI/Area/Random as Button).toggled.connect(Area_Change.bind($GUI/Area/Random))
	($GUI/Area/PointReader as Button).toggled.connect(Area_Change.bind($GUI/Area/PointReader))
	($GUI/Area/PointReader as Button).toggled.emit(true)
	
	($GUI/Area/PositionY as HSlider).value_changed.connect(Area_PositionY)
	($GUI/Area/PositionX as VSlider).value_changed.connect(Area_PositionX)
	rhomb_color()
	update_reader()

func Camera_PositionX(value: float) -> void:
	var cam_posx := $GUI/Camera/PositionX as VSlider
	($Camera3D as Camera3D).position.x = 0. - (1.-value) * 22.

func Camera_PositionY(value: float) -> void:
	var cam_posy := $GUI/Camera/PositionY as HSlider
	($Camera3D as Camera3D).position.z = 0. + (1.-value) * 22.


func Camera_Zoom(value: float) -> void:
	var cam_zoom := $GUI/Camera/Zoom as VSlider
	($Camera3D as Camera3D).size = .1 + (1.-value) * 15.
	
	var markers: Array[Marker3D] = [($PenroseTile/Marker3D as Marker3D), ($PenroseTile/Marker_Single as Marker3D),
		($PenroseTile/MarkRand as Marker3D),($PenroseTile/PixelReader as Marker3D), ]
	for mm in markers:
		mm.scale = Vector3.ONE * (1. + 3. * (1.-value))
	

func Shader_Define(_toggle: bool) -> void:
	var shader_define := ''
	shader_define += "#define USE_TEXTURE\n" if ($GUI/Shader/Texture as CheckButton).button_pressed else ''
	shader_define += "#define TRI_BORDER\n" if ($GUI/Shader/Borders as CheckButton).button_pressed else ''
	shader_define += "#define PERPENDICULAR\n" if ($GUI/Shader/Perp as CheckButton).button_pressed else ''
	shader_define += "#define COLOR_YZ\n" if ($GUI/Shader/ColorYZ as CheckButton).button_pressed else ''
	var shader := (penrose_tile.material_override as ShaderMaterial).shader
	shader.code = shader_define + penrose_shader_code

func Area_Change(_toggle: bool, area_button: Button) -> void:
	if !_toggle: return
	var areas: Array[Button] = [($GUI/Area/Bigger as Button), ($GUI/Area/Single as Button), 
		($GUI/Area/Random as Button), ($GUI/Area/PointReader as Button), ]
	var markers: Array[Marker3D] = [($PenroseTile/Marker3D as Marker3D), ($PenroseTile/Marker_Single as Marker3D),
		($PenroseTile/MarkRand as Marker3D),($PenroseTile/PixelReader as Marker3D), ]
	
	for ii in range(len(areas)):
		if areas[ii] == area_button:
			selected_marker = ii
			($GUI/Area/PositionY as HSlider).set_value_no_signal(1.- (markers[ii].position.y / 22.))
			($GUI/Area/PositionX as VSlider).set_value_no_signal(1.- (-markers[ii].position.x / 22.))
		else:
			areas[ii].button_pressed = false
		areas[ii].disabled = areas[ii] == area_button
		markers[ii].visible = areas[ii] == area_button
	
func Area_PositionY(value: float) -> void:
	marker_set_y(selected_marker, (1.-value) * 22.)
	rhomb_color()
	update_reader()
	
func Area_PositionX(value: float) -> void:
	marker_set_x(selected_marker, -(1.-value) * 22.)
	rhomb_color()
	update_reader()

func marker_set_x(index: int, value: float):
	match index:
		0:
			marker_pos.x = value
		1:
			marker_single_pos.x = value
		2:
			marker_random_pos.x = value
		3:
			pixel_reader_pos.x = value

func marker_set_y(index: int, value: float):
	match index:
		0:
			marker_pos.y = value
		1:
			marker_single_pos.y = value
		2:
			marker_random_pos.y = value
		3:
			pixel_reader_pos.y = value

func update_reader() -> void:
	if pixel_reader:
		var gpos = pixel_reader.global_position
		var gr_int := await get_ground(Vector2(gpos.x, gpos.z))
		if gr_int >= 0:
			($GUI/Area/Reader/GroundType as Label).text = str(gr_int)
			($GUI/Area/Reader/GroundColor as ColorRect).color = ground_colors[ gr_int ]

func set_redo_ready(value) -> void:
	self.get_penrose_mesh()

func _process(_delta: float) -> void:
	pass
	
func _get_marker_pos() -> Vector2:
	return marker_pos
func _set_marker_pos(value: Vector2):
	if marker:
		marker.position.x = value.x
		marker.position.y = value.y
		rhomb_color()
	marker_pos = value
	
func _get_marker_single_pos() -> Vector2:
	return marker_single_pos
func _set_marker_single_pos(value: Vector2):
	if marker_single:
		marker_single.position.x = value.x
		marker_single.position.y = value.y
		rhomb_color()
	marker_single_pos = value
	
func _get_marker_random_pos() -> Vector2:
	return marker_random_pos
func _set_marker_random_pos(value: Vector2):
	if marker_random:
		marker_random.position.x = value.x
		marker_random.position.y = value.y
		rhomb_color()
	marker_random_pos = value

func _get_pixel_reader_pos() -> Vector2:
	return pixel_reader_pos
func _set_pixel_reader_pos(value: Vector2):
	if pixel_reader:
		pixel_reader.position.x = value.x
		pixel_reader.position.y = value.y
	pixel_reader_pos = value

func _get_pixel_color() -> Color:
	if pixel_reader:
		var gpos = pixel_reader.global_position
		var gr_int := await get_ground(Vector2(gpos.x, gpos.z))
		if gr_int >= 0:
			return ground_colors[ gr_int ]
	return Color.BLACK

func _set_pixel_color(value: Color):
	return

func get_ground(pos: Vector2) -> int:
	pos -= Vector2(position.x, position.z)
	return await gr_point.submit(pos)


'''
Constructs penrose mesh, is only done at _init
'''
func get_penrose_mesh():
	var offset := 0.0
	var size := 10.0
	var gcorners: PackedVector2Array = [
			Vector2(offset, offset), 
			Vector2(offset + size, offset), 
			Vector2(offset + size,offset + size), 
			Vector2(offset, offset + size), 
		]
	penr_cons = PenroseConstruct.new(gcorners)
	
	#var uvs = PackedVector2Array()
	#var normals = PackedVector3Array()
	var verts := PackedVector2Array()
	var indices := PackedInt32Array()
	
	verts = penr_cons.center.duplicate()
	verts.append_array(penr_cons.points)
	
	var len_center := len(penr_cons.center)
	for ind in range(len_center):
		for side in range(4):
			indices.append(ind)
			indices.append(penr_cons.rhomb_ind[ind][ (side + 1) % 4 ] + len_center)
			indices.append(penr_cons.rhomb_ind[ind][ side ] + len_center)

	var lineVectors: Array = []
	lineVectors.resize(Mesh.ARRAY_MAX)
	lineVectors[Mesh.ARRAY_VERTEX] = verts
	#lineVectors[Mesh.ARRAY_NORMAL] = normals
	#lineVectors[Mesh.ARRAY_TEX_UV] = uvs
	lineVectors[Mesh.ARRAY_INDEX] = indices
	
	var meshl := ArrayMesh.new()
	meshl.add_surface_from_arrays(Mesh.PRIMITIVE_TRIANGLES, lineVectors)
	var mat3d := ShaderMaterial.new()
	mat3d.shader = penrose_shader
	
	var textures := Texture2DArray.new()
	textures.create_from_images([
		texture_dirt.get_image(), 
		texture_yellow.get_image(), 
		texture_blue.get_image(),
		texture_grass.get_image(),
		texture_purpur.get_image(),
		texture_metal.get_image(),
		texture_reddish.get_image()
	])
	
	mat3d.set_shader_parameter("rhomb_len", len_center)
	mat3d.set_shader_parameter("ground_tex", textures)
	
	var reg = RegEx.new()
	reg.compile(r'rh_color[^\{]*\{([^\}]*)}')
	var regcolr = RegEx.new()
	regcolr.compile(r'vec3[^\d\.\)]*((?:[^\d\.\)]*\d*\.\d*)+)[^\d\.\)]*\)')
	ground_colors.clear()
	var reg_str := reg.search(penrose_shader.code).strings[1]
	for mcolr in regcolr.search_all( reg_str ):
		var f_array := mcolr.strings[1].split(',')
		if f_array.size() == 1:
			var rr := float(f_array[0])
			ground_colors.append( Color(rr, rr, rr) )
		elif f_array.size() == 3:
			var rr := float(f_array[0])
			var gg := float(f_array[1])
			var bb := float(f_array[2])
			ground_colors.append( Color(rr, gg, bb) )

	penrose_tile.mesh = meshl
	penrose_tile.material_override = mat3d
	rhomb_color()

'''
Fills rhomb type data
'''
func rhomb_color():
	var len_center := len(penr_cons.center)
	var len_points := len(penr_cons.points_to_rhomb)
	
	penr_cons.rh_ground.clear()
	penr_cons.rh_ground.resize(4 * len_center)
	
	penr_cons.co_ground.clear()
	penr_cons.co_ground.resize(4 * len_points)
	
	var m_radius := 5.
	var single_radius := .5
	var m_rand_radius := 2.
	
	var rand_seed := (int(marker_random_pos.x*100) & 0xffff) | (int(marker_random_pos.y*100) & 0xffff << 16)
	seed(rand_seed)
			
	for ind in range(len_center):
		var ground_type := 0
		if penr_cons.center[ind].distance_to(marker_single_pos) < single_radius:
			ground_type = 2
		elif penr_cons.center[ind].distance_to(marker_pos) < m_radius:
			ground_type = 1
		else:
			ground_type = 0
		if penr_cons.center[ind].distance_to(marker_random_pos) < m_rand_radius:
			ground_type = randi_range(1, 6)
		
		penr_cons.rh_ground.encode_u8(4 * ind, ground_type)

	for p_ind in range(len_points):
		var count_ground := PackedInt32Array()
		count_ground.resize(8)
		var order_ground := PackedInt32Array()
		order_ground.resize(10)
		
		for ind in range(0, len(penr_cons.points_to_rhomb[p_ind]), 2):
			var rh_center := penr_cons.points_to_rhomb[p_ind][ind]
			var rh_size   := penr_cons.points_to_rhomb[p_ind][ind + 1]
			var rh_gr     := penr_cons.rh_ground.decode_u8(4 * rh_center)
			count_ground[rh_gr] += rh_size
			var ang := ((roundi( (penr_cons.points[p_ind] - penr_cons.center[rh_center]).angle() * 10. / PI) + 10) % 20)
			
			for ioff in range(-rh_size, rh_size, 2):
				order_ground[((ang + ioff  + 21)%20) >> 1 ] = rh_gr
			
		var max1 := Vector2i.ZERO
		var max2 := Vector2i.ZERO
		for c_ind in range(len(count_ground)):
			if count_ground[c_ind] >= max1.x:
				max2 = max1
				max1.x = count_ground[c_ind]
				max1.y = c_ind
		
		var in_a_row5 := false
		for c_ind in range(len(order_ground)):
			var is_same := true
			for cc in range(5):
				is_same = is_same && (order_ground[c_ind] == order_ground[(c_ind + cc) % len(order_ground)])
			if is_same:
				in_a_row5 = true
				break

		var out_gr := max1.y
		out_gr = 0 if max1.x == max2.x else out_gr
		out_gr = 0xf if (max1.x == max2.x) && in_a_row5 else out_gr
		penr_cons.co_ground.encode_u32(4 * p_ind, out_gr)

	for ind in range(len_center):
		var rh_type := penr_cons.rhomb_type[ind]
		var rh_longside := penr_cons.rhomb_longside[ind]
		var rh_sides := 0
		for rh_ind in penr_cons.sides[ind]:
			rh_sides = (rh_sides << 4) | (penr_cons.rh_ground.decode_u8(4 * rh_ind) & 0xf)
		var b_info := ((0x1 & rh_longside) << 24) | ((0xf & rh_type) << 20) | (rh_sides << 4) | (penr_cons.rh_ground.decode_u8(4 * ind) & 0xf)
		penr_cons.rh_ground.encode_u32(4 * ind, b_info )

	var rh_ground_img: = Image.create_from_data(len_center, 1, false, Image.FORMAT_RF, penr_cons.rh_ground)
	var rh_ground_tex: = ImageTexture.create_from_image(rh_ground_img)
		
	var co_ground_img: = Image.create_from_data(len_points, 1, false, Image.FORMAT_RF, penr_cons.co_ground)
	var co_ground_tex: = ImageTexture.create_from_image(co_ground_img)
	
	var mat := penrose_tile.material_override as ShaderMaterial
	mat.set_shader_parameter("rhomb_config", rh_ground_tex)
	mat.set_shader_parameter("corner_config", co_ground_tex)
	if gr_point:
		gr_point.update_configs()

class PenroseConstruct:
	class Line:
		'''
		Line has one of 5 angles, and integer offset
		paralel lines are always offseted perpendicularly by integer amount
			offset difference of 1 means a 90degress vector 
			from line 1 to line 2 is always magnitude/length of 1.
		Used to find intersections between lines of different angles -> class CrossPoint
		'''
		static var _ANGLES: Array[float] = []
		static var _SLOPES: Array[float] = []
		static var _OFFSETS: Array[float] = []
		static var _UNIT_VECTORS: Array[Vector2] = []
		static var _R_OFFSETS: Array[float] = []
		@export var type: int
		@export var mult: float
		@export var angle: float: get = get_angle
		@export var slope: float: get = get_slope
		@export var offset_unit: float: get = get_offset_unit
		@export var offset_r: float: get = get_offset_r
		@export var offset: float: get = get_offset
		@export var unit_vector: Vector2: get = get_unit_vector
		var _hash: int
		
		func _init(_type: int, _mult: float = 0):
			self.type = _type
			self.mult = float(_mult)
			self._hash = hash( float(self.type)*.2 + self.mult )
		static func _static_init() -> void:
			for ii in range(5):
				_ANGLES.append(ii * (PI  / 5))
			for aa in _ANGLES:
				_SLOPES.append( tan(aa) )
			for aa in _ANGLES:
				_OFFSETS.append((sin(aa + (PI/2)) - tan(aa) * cos(aa + (PI/2))))
			for aa in _ANGLES:
				_UNIT_VECTORS.append(Vector2(cos(aa), sin(aa)))
				
			#var offsets = [math.sqrt(float(aa)) % 1.0 for aa in [2,3,5,7]]
			for aa: float in ([2.0, 3.0, 5.0, 7.0] as Array[float]):
				var sqrtaa := sqrt(aa)
				_R_OFFSETS.append(sqrtaa - floorf(sqrtaa))
			var offsets_sum := 0.0
			for aa in _R_OFFSETS:
				offsets_sum += aa
			_R_OFFSETS.append( offsets_sum - floorf(offsets_sum) )
		
		func _to_string() -> String:
			return "Line( t:{0}, m:{1})".format(["%d" % self.type, "%+02d" % int(self.mult)])
		func yy(xx: float) -> float:
			return xx * self.slope + self.offset
		func point(xx: float) -> Vector2:
			return Vector2(xx, self.yy(xx))
		func get_angle() -> float:
			return self._ANGLES[self.type]
		func get_slope() -> float:
			return self._SLOPES[self.type]
		func get_offset_unit() -> float:
			return self._OFFSETS[self.type]
		func get_offset_r() -> float:
			return self._R_OFFSETS[self.type]
		func get_offset() -> float:
			return self.offset_unit * (self.mult + self._R_OFFSETS[self.type])
		func get_unit_vector() -> Vector2:
			return self._UNIT_VECTORS[self.type]
		
	class CrossPoint:
		'''
		From 2 class Lines finds their angle difference, must be 2 different angles
		from intersection rhomb type thin or thick and one of 5 orientations
		rhombs will ordered by one line and placed in order, skewered
		this point is not the same as the rhomb center
		'''
		static var _RHOMBS: Array[PackedVector2Array] = []
		static var _RH_ANGLES: Array[Vector2] = []
		static var _LINE2TYPE: PackedInt32Array = []
		var line1: Line
		var line2: Line
		var point: Vector2
		var magnitude1: float
		var magnitude2: float
		var order1: int
		var order2: int
		var rhomb_line1: Vector2
		var rhomb_line2: Vector2
		var rh_type: int
		var line_types: PackedInt32Array
		var rhomb_point: Vector2
		var _hash: int
		
		func _init(_line1: Line, _line2: Line):
			self.line1 = _line1
			self.line2 = _line2
			assert(self.line1.type != self.line2.type, "must be different types")
			var xx := ( self.line2.offset - self.line1.offset )/( self.line1.slope - self.line2.slope )
			var yy := self.line1.yy(xx)
			self.point = Vector2(xx, yy)
			
			self.line_types = [self.line1.type, self.line2.type,]
			self.line_types.sort()
			self._hash = int((hash(self.line1)/2) + (hash(self.line2)/2))
			var l1uv := Vector2(self.line1.unit_vector)
			l1uv.x = l1uv.x if l1uv.x != 0.0 else 1.0
			l1uv.y = l1uv.y if l1uv.y != 0.0 else 1.0
			
			var l2uv := Vector2(self.line2.unit_vector)
			l2uv.x = l2uv.x if l2uv.x != 0.0 else 1.0
			l2uv.y = l2uv.y if l2uv.y != 0.0 else 1.0
			var diff1 := (self.point - self.line1.point(0)) / l1uv
			var diff2 := (self.point - self.line2.point(0)) / l2uv
			
			self.magnitude1 = (diff1.x + diff1.y) / 2
			self.magnitude2 = (diff2.x + diff2.y) / 2
			
			self.rh_type = _LINE2TYPE[ 5 * self.line_types[0] + self.line_types[1] ]
			
			var rhomb_t := ((self.line1.type - self.line2.type) + 5) % 5
			self.rhomb_line1 = _RH_ANGLES[ (((5 - rhomb_t) + self.line1.type) * 2) % 20 ]
			self.rhomb_line2 = _RH_ANGLES[ ((rhomb_t + self.line2.type) * 2) % 20 ]

		static func _static_init() -> void:
			_LINE2TYPE.resize(25)
			for tt in range(15, 15 + 20):
				_RH_ANGLES.append( Vector2( cos(tt * PI/10) , sin(tt * PI/10) ) )
			
			for type in range(10):
				var points := PackedVector2Array()
				points.resize(4)
				var unit := unit_rhomb(type)
				points[0] = (_RH_ANGLES[unit[0]] + _RH_ANGLES[unit[1]]) * .5
				points[1] = -(_RH_ANGLES[unit[2]] + _RH_ANGLES[unit[3]]) * .5
				points[2] = - points[0]
				points[3] = - points[1]
				_RHOMBS.append(points)
				var ltypes := PackedInt32Array([(unit[0] % 10) / 2, (unit[1] % 10) / 2])
				ltypes.sort()
				_LINE2TYPE[ 5 * ltypes[0] + ltypes[1] ] = type
				
		static func unit_rhomb(type: int) -> PackedInt32Array:
			# 0  1  2  3  4  5  6  7  8  9
			# 14 20 31 42 03 01 12 23 34 40
			var r1 := (type % 5)
			var r2 := 0 if type < 5 else 1 # 0 = FAT, 1 = THIN
			var ang_rot := r1 * 2 + r2
			var ang_off1 := 2 if r2 == 0 else 1
			var ang_off2 := 3 if r2 == 0 else 4
			var v0_0 := (ang_rot - ang_off1 + 20) % 20
			var v0_1 := (ang_rot + ang_off1 + 20) % 20
			var v1_0 := (ang_rot - ang_off2 + 5 + 20) % 20
			var v1_1 := (ang_rot + ang_off2 + 5 + 20) % 20
			return PackedInt32Array([v0_0, v0_1, v1_0, v1_1])
		func _to_string() -> String:
			var s = "CrossPoint( t:{} m:{}, t:{} m:{}, p: {})"
			return s.format(["%d"%self.line1.type, "%+02d"%self.line1.mult, 
				"%d"%self.line2.type, "%+02d"%self.line2.mult, "%v"%self.point])

		func get_points(center: Vector2) -> PackedVector2Array:
			var points := PackedVector2Array()
			for rh_p in _RHOMBS[(self.rh_type) % 10]:
				points.append(rh_p + center)
			return points
			
		func is_in_rectangle(corners: Array[Vector2]) -> bool:
			var _ccw = func(p1: Vector2, p2: Vector2, p3: Vector2) -> int:
				var value := (p2.x - p1.x) * (p3.y - p1.y) - (p3.x - p1.x) * (p2.y - p1.y)
				#if math.isclose(value, 0):
					#return 0
				@warning_ignore("narrowing_conversion")
				return signi(value)
			
			var count: int = 0
			
			for cc in range(4):
				count += _ccw.call(corners[cc], corners[(cc+1) % 4], self.point)
			return count == 4 or count == -4
		
		func line(line_type: int) -> Line:
			if line_type == self.line2.type:
				return self.line2
			return self.line1
		func magnitude(line_type: int) -> float:
			if line_type == self.line2.type:
				return self.magnitude2
			return self.magnitude1
		func order(line_type: int) -> int:
			if line_type == self.line2.type:
				return self.order2
			return self.order1
		func rhomb_line(line_type: int) -> Vector2:
			if line_type == self.line2.type:
				return self.rhomb_line2
			return self.rhomb_line1
		func mult(line_type: int) -> float:
			if line_type == self.line2.type:
				return self.line2.mult
			return self.line1.mult
		func coords() -> Array:
			var coords: PackedInt32Array = [NAN, NAN, NAN, NAN, NAN,]
			coords[self.line1.type] = int(self.line1.mult)
			coords[self.line2.type] = int(self.line2.mult)
			return coords

	'''
	Constructs Penrose tilings to be used as a mesh, to find neighbouring rhombs
	from a rhomb point to find other rhombs that use that same point
	'''
	var points: PackedVector2Array
	var points_to_rhomb: Array[PackedInt32Array]
	var center: PackedVector2Array
	var lines: Array[PackedInt32Array]
	var rhomb_ind: Array[PackedInt32Array]
	var rhomb_type: PackedInt32Array
	var rhomb_longside: PackedInt32Array # 0 => longside is Y ; 1 => longside is Z
	var sides: Array[PackedInt32Array]
	var dictsides: Dictionary
	var arraysides: Array[PackedInt64Array]
	var p_skipped : PackedInt32Array

	var rh_ground: PackedByteArray
	var co_ground: PackedByteArray
		
	'''
	Gets 4 points of a rhomb from @cross_x with @point_x as center
	If rhomb corner points already exist then they arent added to @point array but use index of existing
	@points -> array of vector2, rhomb corners, no centers
	@center -> array of vector2, rhomb center 
	@rhomb_ind -> array of [array of 4 int], integers are indexes of @point
	@rhomb_type -> array of int, size of @center, rhomb type 
		# 0  1  2  3  4  5  6  7  8  9
		# 14 20 31 42 03 01 12 23 34 40
	@rhomb_longside -> array of int, size of @center,  0 => longside is Y ; 1 => longside is Z
		needed for shader to discern between Y and Z which angle is smaller or larger 
		part of specific index ordering thats been done in @get_point_indexes
	@points_to_rhomb -> from corner rhomb point gets rhomb center point and its angle to that rhomb
	@arraysides -> array the size of center points of array size 4, which 4 rhombs a rhomb is neighbouring
					its always ordered either clockwise or counterclockwise
	'''
	func add(cross_x: CrossPoint, point_x: Vector2):

		var corners := cross_x.get_points(point_x)
		var center_ind := len(self.center)
		var rh_ind := self.get_point_indexes(corners)
		self.points_to_rhomb.resize(len(self.points))
		
		for pp in range(len(rh_ind)):
			var size := 0
			if   (pp % 2) == 0 and (cross_x.rh_type <  5): size = 2
			elif (pp % 2) == 1 and (cross_x.rh_type <  5): size = 3
			elif (pp % 2) == 0 and (cross_x.rh_type >= 5): size = 1
			elif (pp % 2) == 1 and (cross_x.rh_type >= 5): size = 4
			
			var p_ind := rh_ind[pp]
			self.points_to_rhomb[p_ind].append(center_ind)
			self.points_to_rhomb[p_ind].append(size)
		
		var l_rh_ind := len(rh_ind)
		var asides := PackedInt64Array()
		for rhi in range(l_rh_ind):
			var hash_side := (hash( rh_ind[rhi] ) >> 1) + (hash( rh_ind[(rhi + 1) % l_rh_ind] ) >> 1)
			asides.append(hash_side)
			if not self.dictsides.has(hash_side):
				self.dictsides[hash_side] = PackedInt32Array()
			self.dictsides[hash_side].append_array([len(self.center), rhi])
		
		for pp in range(l_rh_ind):
			self.add_line([rh_ind[pp], rh_ind[(pp + 1) % l_rh_ind], ])
		self.center.append(point_x)
		self.arraysides.append(asides)
		self.rhomb_ind.append(rh_ind)
		self.rhomb_type.append(cross_x.rh_type)
		var sidesize0 :=  roundi( 10. * asin( self.points[rh_ind[0]].distance_to(point_x) ) / PI)
		self.rhomb_longside.append( (rh_ind[0] % 2) ^ int(sidesize0 >= 3) )
		
	'''
	from 4 Vector2 finds their indexes in @points or adds if missing
	special ordering is done so opposite points in a rhomb are always pair or odds
	its done for shader to discern Y and Z
	for angle finding purpose @rh_longside if if Y or Z is the bigger angle 
	'''
	func get_point_indexes(p_vecs: PackedVector2Array) -> PackedInt32Array:
		var point_indexes := PackedInt32Array()
		point_indexes.resize( len(p_vecs) )
		
		for pvecind in range(len(point_indexes)):
			point_indexes[pvecind] = -1

		for pvecind in range(len(p_vecs)):
			for pind in range(len(self.points)):
				if p_vecs[pvecind].distance_to(self.points[pind]) < 0.1:
					point_indexes[pvecind] = pind
					break
		# 0 1 2 3
		# t f t f
		var nextind := len(self.points)
		var prevind := -1
		for pvecind in range(len(point_indexes) * 3):
			var pvecindx := pvecind % len(point_indexes)
			if point_indexes[pvecindx] >= 0:
				prevind = pvecindx
			elif prevind >= 0 and ((prevind + point_indexes[prevind]) % 2) == ((pvecindx + nextind) % 2):
				point_indexes[pvecindx] = nextind
				self.points.append( p_vecs[pvecindx] )
				nextind = len(self.points)
				prevind = pvecindx
		
		if prevind == -1:
			for pvecind in range(len(point_indexes)):
				point_indexes[pvecind] = len(self.points)
				self.points.append( p_vecs[pvecind] )
			prevind = 0
		
		for pvecind in range(len(point_indexes)):
			if point_indexes[pvecind] == -1:
				for p_skip in self.p_skipped.duplicate():
					if ((prevind + point_indexes[prevind]) % 2) == ((pvecind + p_skip) % 2):
						self.points[p_skip] = p_vecs[pvecind]
						self.p_skipped.remove_at(self.p_skipped.find(p_skip))
						point_indexes[pvecind] = p_skip
						break
			if point_indexes[pvecind] == -1:
				if ((prevind + point_indexes[prevind]) % 2) != ((pvecind + len(self.points)) % 2):
					self.p_skipped.append( len(self.points) )
					self.points.append( self.points[0] )
					point_indexes[pvecind] = len(self.points)
					self.points.append( p_vecs[pvecind] )
				
		return point_indexes

	func add_line(line_p: PackedInt32Array):
		line_p.sort()
		for pind in self.lines:
			if line_p[0] == pind[0] and line_p[1] == pind[1]:
				return
		self.lines.append(line_p)

		
	class CrossVec:
		var cross: CrossPoint
		var point: Vector2
		func _init(_cross: CrossPoint, _point: Vector2) -> void:
			self.cross = _cross
			self.point = _point

	'''
	finds lines contained by 4 corners
	'''
	static func mult_minmax(corners: PackedVector2Array, line: Line) -> PackedInt32Array:
		var min_mult: float = INF
		var max_mult: float = -INF
		for corn in corners:
			# y = mx + b
			# b = y - mx
			var cornbmult := ((corn.y - corn.x * line.slope) - line.offset_r)/ line.offset_unit
			min_mult = minf(min_mult, cornbmult)
			max_mult = maxf(max_mult, cornbmult)
		return PackedInt32Array([ceili(min_mult), ceili(max_mult)])

	func _init(gcorners: PackedVector2Array):
		self.points = []
		self.points_to_rhomb = []
		self.center = []
		self.lines = []
		self.rhomb_ind = []
		self.rhomb_type = []
		self.rhomb_longside = []
		self.sides = []
		self.dictsides = {}
		self.arraysides = []
		self.p_skipped = []
		
		self.rh_ground = PackedByteArray()
		self.co_ground = PackedByteArray()
		
		var lineh_set: PackedInt32Array = []
		var cross_set: Array[CrossPoint] = []
		
		var pentmm: Array[PackedInt32Array] = []
		for pentx in range(5):
			pentmm.append( self.mult_minmax(gcorners, Line.new(pentx)) )
			
		# finds all crosspoints contained by corners, to be later turned into rhombs
		for pent1 in range(4):
			for pent2 in range(pent1+1, 5 ):
				var mult1_10 := pentmm[pent1][0]
				var mult1_11 := pentmm[pent1][1]
				var mult2_10 := pentmm[pent2][0]
				var mult2_11 := pentmm[pent2][1]
				for mult1 in range(mult1_10, mult1_11):
					for mult2 in range(mult2_10, mult2_11):
						var crosspoint := CrossPoint.new(Line.new(pent1, mult1), Line.new(pent2, mult2))
						if crosspoint.is_in_rectangle(gcorners):
							if crosspoint not in cross_set:
								cross_set.append(crosspoint)
							if crosspoint.line1._hash not in lineh_set:
								lineh_set.append(crosspoint.line1._hash)
							if crosspoint.line2._hash not in lineh_set:
								lineh_set.append(crosspoint.line2._hash)
		# establishes ordering
		for pent in range(5):
			var mult_10 := pentmm[pent][0]
			var mult_11 := pentmm[pent][1]
			for mult in range(mult_10, mult_11):
				var crosses := cross_set.filter(func(cs: CrossPoint):
					return (pent in cs.line_types and cs.mult(pent) == mult)
				)
				crosses.sort_custom(func(xx: CrossPoint,yy: CrossPoint) -> bool: 
					return xx.magnitude(pent) < yy.magnitude(pent)
				)
				for cint in range(len(crosses)):
					if pent == crosses[cint].line1.type:
						crosses[cint].order1 = cint
					else:
						crosses[cint].order2 = cint
		
		var cross2_set := cross_set.duplicate()
		var crossp: Array[CrossVec] = []
		crossp.append( CrossVec.new(cross2_set.pop_front(), Vector2(0,0)) )

		# works through @crossp rhombs and its line by skewering more rhombs
		# newly made rhombs will be used to do more skewering until next @crossp is empty
		# every line of angle and offset is done once
		while len(crossp) > 0:
			var crossp_n: Array[CrossVec] = []
			for cr_po in crossp:
				var cross_x := cr_po.cross
				var point_x := cr_po.point
				self.add(cross_x, point_x)
				
				for line: Line in [cross_x.line1, cross_x.line2, ] as Array[Line]:
					if line._hash in lineh_set:
						lineh_set.remove_at( lineh_set.find(line._hash) )
					else:
						continue
					
					var crosslist := cross_set.filter(func(cs: CrossPoint):
						return (line.type in cs.line_types and cs.mult(line.type) == line.mult)
					)
					crosslist.sort_custom(func(xx: CrossPoint,yy: CrossPoint) -> bool:
						return xx.order(line.type) < yy.order(line.type)
					)
					
					var order := cross_x.order(line.type)
					var rhomb_unit := cross_x.rhomb_line(line.type)
					var crossl1 := crosslist.slice(order + 1) as Array[CrossPoint]
					var crossl2 := crosslist.slice(0, order) as Array[CrossPoint]
					crossl2.reverse()
					
					var rhomb_p1 := Vector2(point_x) + rhomb_unit * .5
					for cr in crossl1:
						var rhomb_unit1 :=  cr.rhomb_line(line.type)
						if cr in cross2_set:
							crossp_n.append( CrossVec.new(cr, rhomb_p1 + (rhomb_unit1*.5)) )
							cross2_set.erase(cr)
						rhomb_p1 += rhomb_unit1
					
					var rhomb_p2 := Vector2(point_x) - rhomb_unit * .5
					for cr in crossl2:
						var rhomb_unit2 := -cr.rhomb_line(line.type)
						if cr in cross2_set:
							crossp_n.append( CrossVec.new(cr, rhomb_p2 + (rhomb_unit2*.5)) )
							cross2_set.erase(cr)
						rhomb_p2 += rhomb_unit2
					
			crossp.clear()
			crossp = crossp_n
		# finding rhomb neighbours
		for sideind in range(len(self.arraysides)):
			var arrayside := self.arraysides[sideind]
			var sidesurr := PackedInt32Array()
			sidesurr.resize(4)
			for ind in range(4):
				var sidekey := arrayside[ind]
				var side := self.dictsides[sidekey] as PackedInt32Array
				if len(side) > 2:
					if side[0] == sideind:
						sidesurr[side[1]] = side[2]
					else:
						sidesurr[side[3]] = side[0]
			self.sides.append(sidesurr)
		
		self.rh_ground.resize(4 * len(self.center))
		self.co_ground.resize(4 * len(self.points_to_rhomb))


class GroundPoint:
	'''
	Used to find ground type from a single point
	Uses penrose tile gdshader as a compute shader to calculate ground type
	'''
	var penr_cons: PenroseConstruct
	var penrose_shader: Shader

	var renderdevice: RenderingDevice
	var compute_list: int
	var pipeline: RID
	
	var ground_buffer: RID
	var verts_buffer: RID
	var rhomb_corners_buffer: RID
	var rhomb_config_buffer: RID
	var corner_config_buffer: RID
	var input_buffer: RID
	
	var uniform_set: RID
	
	signal do_update
	
	func _init(_penr_cons: PenroseConstruct, _penrose_shader: Shader) -> void:
		penr_cons = _penr_cons
		penrose_shader = _penrose_shader
		
		var shader_string := "#version 450\n#define IS_COMPUTE\n" + penrose_shader.code
		var shader_src := RDShaderSource.new()
		shader_src.set_stage_source(RenderingDevice.SHADER_STAGE_COMPUTE, shader_string)

		renderdevice = RenderingServer.create_local_rendering_device()
		var shader_spirv := renderdevice.shader_compile_spirv_from_source(shader_src)
		var c_shader := renderdevice.shader_create_from_spirv(shader_spirv)
		pipeline = renderdevice.compute_pipeline_create(c_shader)
		
		var datain := PackedByteArray()
		datain.resize(1*4)
		ground_buffer = renderdevice.storage_buffer_create(datain.size(), datain)
		var ground_uniform := RDUniform.new()
		ground_uniform.uniform_type = RenderingDevice.UNIFORM_TYPE_STORAGE_BUFFER
		ground_uniform.binding = 0
		ground_uniform.add_id(ground_buffer)
		
		var dataverts := PackedFloat32Array()
		for vec in penr_cons.center:
			dataverts.append(vec.x)
			dataverts.append(vec.y)
		for vec in penr_cons.points:
			dataverts.append(vec.x)
			dataverts.append(vec.y)
		
		verts_buffer = renderdevice.storage_buffer_create(dataverts.size() * 4, dataverts.to_byte_array())
		var verts_uniform := RDUniform.new()
		verts_uniform.uniform_type = RenderingDevice.UNIFORM_TYPE_STORAGE_BUFFER
		verts_uniform.binding = 1
		verts_uniform.add_id(verts_buffer)
		
		var center_len := penr_cons.center.size()
		var datarhcorners := PackedInt32Array()
		for rind in penr_cons.rhomb_ind:
			datarhcorners.append(rind[0] + center_len)
			datarhcorners.append(rind[1] + center_len)
			datarhcorners.append(rind[2] + center_len)
			datarhcorners.append(rind[3] + center_len)
		
		rhomb_corners_buffer = renderdevice.storage_buffer_create(datarhcorners.size() * 4, datarhcorners.to_byte_array())
		var rhcorn_uniform := RDUniform.new()
		rhcorn_uniform.uniform_type = RenderingDevice.UNIFORM_TYPE_STORAGE_BUFFER
		rhcorn_uniform.binding = 2
		rhcorn_uniform.add_id(rhomb_corners_buffer)
		
		rhomb_config_buffer = renderdevice.storage_buffer_create(penr_cons.rh_ground.size(), penr_cons.rh_ground)
		var rhconf_uniform := RDUniform.new()
		rhconf_uniform.uniform_type = RenderingDevice.UNIFORM_TYPE_STORAGE_BUFFER
		rhconf_uniform.binding = 3
		rhconf_uniform.add_id(rhomb_config_buffer)
		
		corner_config_buffer = renderdevice.storage_buffer_create(penr_cons.co_ground.size(), penr_cons.co_ground)
		var coconf_uniform := RDUniform.new()
		coconf_uniform.uniform_type = RenderingDevice.UNIFORM_TYPE_STORAGE_BUFFER
		coconf_uniform.binding = 4
		coconf_uniform.add_id(corner_config_buffer)
		
		var datainput := PackedByteArray()
		datainput.resize(4 * 4)
		datainput.encode_s32(2 * 4, penr_cons.center.size() + penr_cons.points.size() )
		datainput.encode_s32(3 * 4, penr_cons.center.size() )
		
		input_buffer = renderdevice.storage_buffer_create(datainput.size(), datainput)
		var input_uniform := RDUniform.new()
		input_uniform.uniform_type = RenderingDevice.UNIFORM_TYPE_STORAGE_BUFFER
		input_uniform.binding = 5
		input_uniform.add_id(input_buffer)
		
		uniform_set = renderdevice.uniform_set_create([ground_uniform, verts_uniform, rhcorn_uniform, rhconf_uniform, 
			coconf_uniform, input_uniform], c_shader, 0)
		
	func update_configs() -> void:
		if compute_list > 0: 
			await do_update
		renderdevice.buffer_update(rhomb_config_buffer , 0, penr_cons.rh_ground.size(), penr_cons.rh_ground)
		renderdevice.buffer_update(corner_config_buffer, 0, penr_cons.co_ground.size(), penr_cons.co_ground)
	
	func submit(pos: Vector2) -> int:
		if compute_list > 0: 
			await do_update
			
		renderdevice.buffer_update(input_buffer, 0, 2 * 4, PackedFloat32Array([pos.x, pos.y]).to_byte_array())
		
		compute_list = renderdevice.compute_list_begin()
		renderdevice.compute_list_bind_compute_pipeline(compute_list, pipeline)
		renderdevice.compute_list_bind_uniform_set(compute_list, uniform_set, 0)
		renderdevice.compute_list_dispatch(compute_list, 1, 1, 1)
		renderdevice.compute_list_end()
		renderdevice.submit()
		renderdevice.sync()
		compute_list = -1
		do_update.emit()

		return renderdevice.buffer_get_data(ground_buffer).decode_s32(0)
