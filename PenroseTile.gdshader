#define USE_TEXTURE

//#ifndef IS_COMPUTE
//#define USE_TEXTURE
//#define TRI_BORDER
//#define PERPENDICULAR
//#define COLOR_YZ
//#endif

#ifdef IS_COMPUTE
const float PI = 3.141592654;

layout(binding = 0) restrict buffer writeonly block2{
	int result_type;
};
layout(binding = 1) restrict buffer readonly vertsblock{
    vec2 verts[];
};
layout(binding = 2) restrict buffer readonly rhcornblock{
    ivec4 rhomb_corners[];
};
layout(binding = 3) restrict buffer readonly rcblock{
    int rhomb_config[];
};
layout(binding = 4) restrict buffer readonly ccblock{
    int corner_config[];
};
layout(binding = 5) restrict buffer readonly inblock{
    vec2 vec;
    int verts_len;
    int rhomb_len;
};
vec3 baryCoord;
int gr_x_type;
int rh_type;
int rh_longside;
int rh_sides;
float gr_y;
float gr_z;
vec2 vec_x;
#else
shader_type spatial;
render_mode unshaded;

uniform int rhomb_len = 0;
uniform usampler2D rhomb_config;
uniform usampler2D corner_config;
uniform sampler2DArray ground_tex;

varying vec3 baryCoord;
varying flat int gr_x_type;
varying flat lowp int rh_type;
varying flat lowp int rh_longside;
varying flat lowp int rh_sides;
varying float gr_y;
varying float gr_z;
varying flat vec2 vec_x;
varying vec2 vec;

const vec3 vectors[3] = {
	vec3(1.0, 0.0 ,0.0),
	vec3(0.0, 1.0 ,0.0),
	vec3(0.0, 0.0 ,1.0)
};
#ifndef USE_TEXTURE
const vec3 rh_color[10] = {
	vec3(1.0, 0.0, 0.0),
	vec3(1.0, 1.0, 0.0),
	vec3(1.0, 0.0, 1.0),
	vec3(0.0, 1.0, 1.0),
	vec3(0.0, 0.0, 1.0),

	vec3(0.7, 0.2, 0.2),
	vec3(0.8, 0.8, 0.1),
	vec3(0.7, 0.2, 0.7),
	vec3(0.2, 0.7, 0.7),
	vec3(0.2, 0.2, 0.7)
};
#endif
#endif


#ifdef IS_COMPUTE
float triangle_area(vec3 sides){
    // # area = 0.25 * sqrt( (a + b + c) * (-a + b + c) * (a - b + c) * (a + b - c) )
	return .25 * sqrt( (sides.x + sides.y + sides.z) * (-sides.x + sides.y + sides.z) *
						(sides.x - sides.y + sides.z) * ( sides.x + sides.y - sides.z) );
}
#endif
// pi and p2 create a line, return on which side pp is
bool dot_(vec2 p1, vec2 p2, vec2 pp){
	return  ((p2.x - p1.x) * (pp.y - p1.y) - (p2.y - p1.y) * (pp.x - p1.x)) >= 0.;
}
bool cubicBezier3(vec2 baryC, vec2 point1, vec2 cpoint1, vec2 cpoint2, vec2 point2){
	float rati =.75* distance(point1, point2)/( distance(point1, cpoint1) + distance(cpoint2, point2) );

	cpoint1 = mix(point1, cpoint1, rati);
	cpoint2 = mix(point2, cpoint2, rati);

	vec2 b_dist = vec2(distance(baryC, point1), distance(baryC, point2));

	//float tt = baryC.x / (baryC.x + baryC.y);
	float tt = b_dist.x / (b_dist.x + b_dist.y);

	float tt_m1 = 1. - tt;
	vec2 cb_xy = tt_m1*tt_m1*tt_m1 * point1 + 3.*tt_m1*tt_m1*tt * cpoint1 +
		3.*tt_m1*tt*tt * cpoint2 + tt*tt*tt * point2 +1e-7;

	bool p1_b = dot_(point1, cb_xy, baryC);
	bool p2_b = dot_(cb_xy, point2, baryC);

	return !(!p1_b && !p2_b);
}
// from a point laying on a triangle side,
// finds point perpendicular to it that lays on a different triangle side
vec3 perpendicular(vec3 point, ivec3 ang){
	vec3 angf = vec3(ang) * PI *.1;
	vec3 sinangf = sin(angf);
	vec3 cosangf = cos(angf);
	vec3 tanangf = tan(angf-1e-6);

	int ii;
	ii = point.x < 1e-4 ? 0 : ii;
	ii = point.y < 1e-4 ? 1 : ii;
	ii = point.z < 1e-4 ? 2 : ii;

	vec3 perp;
	vec2 tside = vec2(point[(ii+2)%3] * tanangf[(ii+1)%3] , point[(ii+1)%3] * tanangf[(ii+2)%3]);
	perp[ii] = min( tside.x , tside.y ) * sinangf[ii] / ( sinangf[(ii+2)%3] * sinangf[(ii+1)%3] );
	bool stepyz = tside.x > tside.y;
	perp[(ii+1)%3] = stepyz ? 0. : (1. - perp[ii]);
	perp[(ii+2)%3] = stepyz ? (1. - perp[ii]) : 0.;
	perp += 1e-5;
	return perp;
}

//bool ground_calc(vec3 strn, vec3 baryC, ivec3 ang){
	//// z , x == y
	//vec3 cross1, cross2;
	//cross1 = vec3(.0, strn.z, strn.y) / (strn.z + strn.y);
	//cross2 = vec3(strn.z, .0, strn.x) / (strn.x + strn.z);
	//vec3 cross1b, cross2b;
	//
	//cross1b = perpendicular(cross1, ang);
	//cross2b = perpendicular(cross2, ang);
	//
	//cross1b = strn.y < 1e-5 ? vec3(1., 0., 0.) : cross1b;
	//cross2b = strn.x < 1e-5 ? vec3(0., 1., 0.) : cross2b;
//
	//return cubicBezier3(baryC.xy, cross1.xy, cross1b.xy, cross2b.xy, cross2.xy );
//}
vec2 rh_angle(int ang){
	return vec2( cos(float(ang)*PI*.1 ), sin(float(ang)*PI*.1 ) );
}

ivec4 unit_rhomb(int type){
	//# 0  1  2  3  4  5  6  7  8  9
	//# 14 20 31 42 03 01 12 23 34 40
	int r1 = (type % 5);
	int r2 = type < 5? 0 : 1; //# 0 = FAT, 1 = THIN
	int ang_rot = r1 * 2 + r2;
	int ang_off1 = r2 == 0? 2 : 1;
	int ang_off2 = r2 == 0? 3 : 4;
	ivec4 v_ang;
	v_ang[0] = (ang_rot - ang_off1 + 20) % 20;
	v_ang[1] = (ang_rot + ang_off1 + 20) % 20;
	v_ang[2] = (ang_rot - ang_off2 + 5 + 20) % 20;
	v_ang[3] = (ang_rot + ang_off2 + 5 + 20) % 20;
	return v_ang;
}
int find_rh_side(){
	vec2 v_dir = normalize(vec - vec_x);

	ivec4 v_ang = unit_rhomb(rh_type);
	vec2 point1 = .5*(rh_angle(v_ang[0]) + rh_angle(v_ang[1]));
	vec2 point2 = .5*(rh_angle(v_ang[2]) + rh_angle(v_ang[3]));

	ivec2 side;
	side.x = int(dot_(vec2(0.), point1, v_dir));
	side.y = int(dot_(vec2(0.), point2, v_dir));
	int iside = (side.y << 1) | (side.x ^ side.y);

	return (rh_sides >> (iside)*4) & 0xf;
}


#ifdef IS_COMPUTE
void vertex(int VERTEX_ID){
	//vec2 VERTEX = verts[VERTEX_ID];
#else
void vertex(){
	vec = VERTEX.xy;
	gr_y = 0.;
	gr_z = 0.;
#endif


	if(VERTEX_ID < rhomb_len){

#ifdef IS_COMPUTE
		int rh_c = rhomb_config[VERTEX_ID];
		vec_x = verts[VERTEX_ID];
#else
		UV = vec2(.0, .0);
		baryCoord = vectors[0];
		int rh_c = int(texelFetch(rhomb_config, ivec2(VERTEX_ID, 0), 0).r);
		vec_x = VERTEX.xy;
#endif

		gr_x_type = rh_c & 0xf;
		rh_sides = (rh_c >> 4) & 0xffff;
		rh_type = (rh_c >> 20) & 0xf;
		rh_longside = (rh_c >> 24) & 0x1;


	}else if((VERTEX_ID % 2) == 0){
#ifdef IS_COMPUTE
	    int typecolRhomb = corner_config[VERTEX_ID - rhomb_len];
#else
		UV = vec2(1. , 0. );
		baryCoord = vectors[ 1 ];
		int typecolRhomb = int(texelFetch(corner_config, ivec2(VERTEX_ID - rhomb_len, 0), 0).r);
#endif
		gr_y = (typecolRhomb & 0xf) == 0xf ? -1. : float(typecolRhomb & 0xf);
	}else{
#ifdef IS_COMPUTE
		int typecolRhomb = corner_config[VERTEX_ID - rhomb_len];
#else
		UV = vec2(0. , 1. );
		baryCoord = vectors[ 2 ];
		int typecolRhomb = int(texelFetch(corner_config, ivec2(VERTEX_ID - rhomb_len, 0), 0).r);
#endif
		gr_z = (typecolRhomb & 0xf) == 0xf ? -1. : float(typecolRhomb & 0xf);
	}
}

void fragment() {
	ivec3 ang;

	ang.x = 5;
	ang.y = 2 - (rh_type / 5) + (1 + 2 * (rh_type / 5)) * rh_longside;
	ang.z = 5 - ang.y;

	ivec3 gr_type;
	gr_type.x = gr_x_type;

#ifdef  IS_COMPUTE
	gr_type.y = int(gr_y);
	gr_type.z = int(gr_z);
#else
	gr_type.y = int(round(gr_y / baryCoord.y));
	gr_type.z = int(round(gr_z / baryCoord.z));
#endif
	vec4 gr_str = vec4(7., 3., 3., 0.);

	gr_type.y = gr_type.y == -1 ? gr_type.x : gr_type.y;
	gr_str.y  = gr_type.y == -1 ? 0. : gr_str.y;
	gr_type.z = gr_type.z == -1 ? gr_type.x : gr_type.z;
	gr_str.z  = gr_type.z == -1 ? 0. : gr_str.z;


	int rh_side_type = find_rh_side();
	int rh_ground = gr_type.x;

	vec3 v10h = vec3(1.,0.,.5);

	vec3 b_xy = gr_str.yxw / (gr_str.x + gr_str.y);
	vec3 b_yz = gr_str.wzy / (gr_str.z + gr_str.y);
	vec3 b_zx = gr_str.zwx / (gr_str.z + gr_str.x);

	vec3 b_yz_y = rh_side_type == gr_type.x ? vec3(0., .75, .25) : v10h.yzz;

	vec3 b_yz_y_per = rh_side_type == gr_type.x ? perpendicular(b_yz_y.xyz, ang) : mix(v10h.yxy, b_yz_y.xyz, 0.25);
	vec3 b_yz_z_per = rh_side_type == gr_type.x ? perpendicular(b_yz_y.xzy, ang) : mix(v10h.yyx, b_yz_y.xzy, 0.25);
	vec3 b_xy_per = perpendicular(b_xy, ang);
	vec3 b_zx_per = perpendicular(b_zx, ang);

	bool b_yz_y_cb = cubicBezier3(baryCoord.xy, b_xy.xy, b_xy_per.xy, b_yz_y_per.xy, b_yz_y.xy);
	bool b_yz_z_cb = cubicBezier3(baryCoord.xy, b_zx.xy, b_zx_per.xy, b_yz_z_per.xy, b_yz_y.xz);

	bool b_yz_side = dot_(v10h.yz, v10h.xy, baryCoord.xy);
	rh_ground =  b_yz_side && !b_yz_y_cb ? gr_type[1]: rh_ground;
	rh_ground = !b_yz_side &&  b_yz_z_cb ? gr_type[2]: rh_ground;

#ifdef  IS_COMPUTE
	result_type = rh_ground;
#else
#ifdef USE_TEXTURE
	ALBEDO = texture(ground_tex, vec3((vec.x * .6),(vec.y * .6), float(rh_ground)) ).rgb;
#else
	ALBEDO = rh_color[rh_ground];
#endif
#ifdef TRI_BORDER
	ALBEDO = mix(vec3(0.0, 0.0, 0.0), ALBEDO, step(.002, min(UV.x,UV.y)) * step( (UV.x+UV.y), .995)  );
	//ALBEDO = mix(ALBEDO, vec3(0.), 1.-step(.01, distance(baryc_str, baryCoord)) );
	//ALBEDO = mix(ALBEDO, vec3(0.), 1.-step(.01, distance(b_xy, baryCoord)) );
	//ALBEDO = mix(ALBEDO, vec3(0.), 1.-step(.01, distance(b_yz, baryCoord)) );
	//ALBEDO = mix(ALBEDO, vec3(0.), 1.-step(.01, distance(b_zx, baryCoord)) );
#endif
#ifdef PERPENDICULAR
	float perpe = step(.01, abs((baryCoord.y/(1.-baryCoord.x)) - (sin(float(ang.y)*PI*.1)*cos(float(ang.z)*PI*.1)) ) );
	ALBEDO = mix(vec3(0., 0. , 1.), ALBEDO, perpe);
#endif
#ifdef COLOR_YZ
	ALBEDO = mix(vec3(0.0, 1.0, 0.0), ALBEDO, step(.08, 1.0 - UV.x)*.5+.5 );
	ALBEDO = mix(vec3(0.0, 0.0, 1.0), ALBEDO, step(.08, 1.0 - UV.y)*.5+.5 );
#endif
#endif
}

#ifdef IS_COMPUTE
// finds which triangle does given point belong to
// then does calculations that regular gdshader does
void main(){
	ivec3 tria = ivec3(-1);
	vec2 vx, vy, vz;
	for(int ind=0; ind < rhomb_len; ind++){
		for(int side=0; side < 4; side++){
			vx = verts[ind];
			vy = verts[ rhomb_corners[ind][ (side + 1) % 4 ] ];
			vz = verts[ rhomb_corners[ind][ side ] ];
			if( dot_(vx, vy, vec) && dot_(vy, vz, vec) && dot_(vz, vx, vec) ){
				tria.x = ind;
				tria.y = rhomb_corners[ind][ (side + 1) % 4 ];
				tria.z = rhomb_corners[ind][ side ];
				break;
			}
		}
		if(tria.x != -1){break;}
	}

	if(tria.x == -1){
		gr_x_type = -1;
		return;
	}

	if((tria.y % 2) == 1){
		int tvv = tria.y;
		tria.y = tria.z;
		tria.z = tvv;
		vy = verts[ tria.y ];
		vz = verts[ tria.z ];
	}

	vertex(tria.x);
	vertex(tria.y);
	vertex(tria.z);

	vec3 posdist  = vec3(distance(vec, vx),distance(vec, vy),distance(vec, vz) );
	vec3 vertdist = vec3(distance(vy, vz), distance(vz, vx), distance(vx, vy) );

	baryCoord.x = triangle_area( vec3(posdist.y, posdist.z, vertdist.x) );
	baryCoord.y = triangle_area( vec3(posdist.z, posdist.x, vertdist.y) );
	baryCoord.z = triangle_area( vec3(posdist.x, posdist.y, vertdist.z) );
	baryCoord /= triangle_area(vertdist);

	fragment();
}
#endif