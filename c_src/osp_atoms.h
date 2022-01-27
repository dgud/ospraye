/*
 *
 *     The enum - atoms used.
 *
 *  Copyright (c) 2022 Dan Gudmundsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

typedef struct {
    const char * name;
    unsigned int   value;
    ERL_NIF_TERM atom;
} osp_atom_t;

osp_atom_t osp_logLevel[] =
    {
     { "debug",     OSP_LOG_DEBUG,   0},
     { "info",      OSP_LOG_INFO,    0},
     { "warning",   OSP_LOG_WARNING, 0},
     { "error",     OSP_LOG_ERROR,   0},
     { "none",      OSP_LOG_NONE,    0},
     { NULL, 0, 0},
    };

osp_atom_t osp_deviceProperty[] =
    {{ "version",            OSP_DEVICE_VERSION, 0},
     { "major",              OSP_DEVICE_VERSION_MAJOR, 0},
     { "minor",              OSP_DEVICE_VERSION_MINOR, 0},
     { "patch",               OSP_DEVICE_VERSION_PATCH, 0},
     { "so_version",         OSP_DEVICE_SO_VERSION, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_dataType[] =
    {{ "device",             OSP_DEVICE, 0},
     { "void_ptr",           OSP_VOID_PTR, 0},
     { "bool",               OSP_BOOL, 0},
     { "object",             OSP_OBJECT, 0},
     { "data",               OSP_DATA, 0},
     { "camera",             OSP_CAMERA, 0},
     { "framebuffer",        OSP_FRAMEBUFFER, 0},
     { "future",             OSP_FUTURE, 0},
     { "geometric_model",    OSP_GEOMETRIC_MODEL, 0},
     { "geometry",           OSP_GEOMETRY, 0},
     { "group",              OSP_GROUP, 0},
     { "image_operation",    OSP_IMAGE_OPERATION, 0},
     { "instance",           OSP_INSTANCE, 0},
     { "light",              OSP_LIGHT, 0},
     { "material",           OSP_MATERIAL, 0},
     { "renderer",           OSP_RENDERER, 0},
     { "texture",            OSP_TEXTURE, 0},
     { "transfer_function",  OSP_TRANSFER_FUNCTION, 0},
     { "volume",             OSP_VOLUME, 0},
     { "volumetric_model",   OSP_VOLUMETRIC_MODEL, 0},
     { "world",              OSP_WORLD, 0},
     { "string",             OSP_STRING, 0},
     { "char",               OSP_CHAR, 0},
     { "vec2c",              OSP_VEC2C, 0},
     { "vec3c",              OSP_VEC3C, 0},
     { "vec4c",              OSP_VEC4C, 0},
     { "uchar",              OSP_UCHAR, 0},
     { "vec2uc",             OSP_VEC2UC, 0},
     { "vec3uc",             OSP_VEC3UC, 0},
     { "vec4uc",             OSP_VEC4UC, 0},
     { "byte",              OSP_UCHAR, 0},
     { "raw",               OSP_UCHAR, 0},
     { "short",              OSP_SHORT, 0},
     { "vec2s",              OSP_VEC2S, 0},
     { "vec3s",              OSP_VEC3S, 0},
     { "vec4s",              OSP_VEC4S, 0},
     { "ushort",             OSP_USHORT, 0},
     { "vec2us",             OSP_VEC2US, 0},
     { "vec3us",             OSP_VEC3US, 0},
     { "vec4us",             OSP_VEC4US, 0},
     { "int",                OSP_INT, 0},
     { "vec2i",              OSP_VEC2I, 0},
     { "vec3i",              OSP_VEC3I, 0},
     { "vec4i",              OSP_VEC4I, 0},
     { "uint",               OSP_UINT, 0},
     { "vec2ui",             OSP_VEC2UI, 0},
     { "vec3ui",             OSP_VEC3UI, 0},
     { "vec4ui",             OSP_VEC4UI, 0},
     { "long",               OSP_LONG, 0},
     { "vec2l",              OSP_VEC2L, 0},
     { "vec3l",              OSP_VEC3L, 0},
     { "vec4l",              OSP_VEC4L, 0},
     { "ulong",               OSP_ULONG, 0},
     { "vec2ul",              OSP_VEC2UL, 0},
     { "vec3ul",              OSP_VEC3UL, 0},
     { "vec4ul",              OSP_VEC4UL, 0},
     { "half",               OSP_HALF, 0},
     { "vec2h",              OSP_VEC2H, 0},
     { "vec3h",              OSP_VEC3H, 0},
     { "vec4h",              OSP_VEC4H, 0},
     { "float",              OSP_FLOAT, 0},
     { "vec2f",              OSP_VEC2F, 0},
     { "vec3f",              OSP_VEC3F, 0},
     { "vec4f",              OSP_VEC4F, 0},
     { "double",             OSP_DOUBLE, 0},
     { "vec2d",              OSP_VEC2D, 0},
     { "vec3d",              OSP_VEC3D, 0},
     { "vec4d",              OSP_VEC4D, 0},
     { "box1i",              OSP_BOX1I, 0},
     { "box2i",              OSP_BOX2I, 0},
     { "box3i",              OSP_BOX3I, 0},
     { "box4i",              OSP_BOX4I, 0},
     { "box1f",              OSP_BOX1F, 0},
     { "box2f",              OSP_BOX2F, 0},
     { "box3f",              OSP_BOX3F, 0},
     { "box4f",              OSP_BOX4F, 0},
     { "linear2f",           OSP_LINEAR2F, 0},
     { "linear3f",           OSP_LINEAR3F, 0},
     { "affine2f",           OSP_AFFINE2F, 0},
     { "affine3f",           OSP_AFFINE3F, 0},
     { "quatf",              OSP_QUATF, 0},
     { "unknown",           OSP_UNKNOWN, 0},
    };
osp_atom_t osp_textureFormat[] =
    {{ "texture_rgba8",           OSP_TEXTURE_RGBA8, 0},
     { "texture_srgba",           OSP_TEXTURE_SRGBA, 0},
     { "texture_rgba32f",         OSP_TEXTURE_RGBA32F, 0},
     { "texture_rgb8",            OSP_TEXTURE_RGB8, 0},
     { "texture_srgb",            OSP_TEXTURE_SRGB, 0},
     { "texture_rgb32f",          OSP_TEXTURE_RGB32F, 0},
     { "texture_r8",              OSP_TEXTURE_R8, 0},
     { "texture_r32f",            OSP_TEXTURE_R32F, 0},
     { "texture_l8",              OSP_TEXTURE_L8, 0},
     { "texture_ra8",             OSP_TEXTURE_RA8, 0},
     { "texture_la8",             OSP_TEXTURE_LA8, 0},
     { "texture_rgba16",          OSP_TEXTURE_RGBA16, 0},
     { "texture_rgb16",           OSP_TEXTURE_RGB16, 0},
     { "texture_ra16",            OSP_TEXTURE_RA16, 0},
     { "texture_r16",             OSP_TEXTURE_R16, 0},
     { "texture_invalid",        OSP_TEXTURE_FORMAT_INVALID, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_texturefilter[] =
    {{ "texture_bilinear",        OSP_TEXTURE_FILTER_BILINEAR, 0},
     { "texture_nearest",        OSP_TEXTURE_FILTER_NEAREST , 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_error_code[] =
    {{ "no_error",             OSP_NO_ERROR         , 0},
     { "unknown_error",        OSP_UNKNOWN_ERROR    , 0},
     { "invalid_argument",     OSP_INVALID_ARGUMENT , 0},
     { "invalid_operation",    OSP_INVALID_OPERATION, 0},
     { "out_of_MEMORY",        OSP_OUT_OF_MEMORY    , 0},
     { "unsupported_cpu",      OSP_UNSUPPORTED_CPU  , 0},
     { "version_mismatch",    OSP_VERSION_MISMATCH , 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_frameBufferFormat[] =
    {{ "fb_none",      OSP_FB_NONE, 0},
     { "fb_srgba",     OSP_FB_SRGBA , 0},
     { "fb_rgba32f",  OSP_FB_RGBA32F, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_frameBufferChannel[] =
    {{ "fb_color",     OSP_FB_COLOR, 0},
     { "fb_depth",     OSP_FB_DEPTH, 0},
     { "fb_accum",     OSP_FB_ACCUM, 0},
     { "fb_variance",  OSP_FB_VARIANCE, 0},
     { "fb_normal",    OSP_FB_NORMAL, 0},
     { "fb_albedo",   OSP_FB_ALBEDO, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_syncEvent[] =
    {{ "none_finished",      OSP_NONE_FINISHED, 0},
     { "world_rendered",     OSP_WORLD_RENDERED, 0},
     { "world_committed",    OSP_WORLD_COMMITTED, 0},
     { "frame_finished",     OSP_FRAME_FINISHED, 0},
     { "task_finished",     OSP_TASK_FINISHED, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_unstructuredCellType[] =
    {{ "tetrahedron",           OSP_TETRAHEDRON, 0},
     { "hexahedron",            OSP_HEXAHEDRON, 0},
     { "wedge",                 OSP_WEDGE, 0},
     { "pyramid",               OSP_PYRAMID, 0},
     { "unknown_cell_type",    OSP_UNKNOWN_CELL_TYPE, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_stereoMode[] =
    {{ "stereo_none",          OSP_STEREO_NONE, 0},
     { "stereo_left",          OSP_STEREO_LEFT, 0},
     { "stereo_right",         OSP_STEREO_RIGHT, 0},
     { "stereo_side_by_side",  OSP_STEREO_SIDE_BY_SIDE, 0},
     { "stereo_top_bottom",    OSP_STEREO_TOP_BOTTOM, 0},
     { "stereo_unknown",      OSP_STEREO_UNKNOWN, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_shutterType[] =
    {{ "shutter_global",           OSP_SHUTTER_GLOBAL, 0},
     { "shutter_rolling_right",    OSP_SHUTTER_ROLLING_RIGHT, 0},
     { "shutter_rolling_left",     OSP_SHUTTER_ROLLING_LEFT, 0},
     { "shutter_rolling_down",     OSP_SHUTTER_ROLLING_DOWN, 0},
     { "shutter_rolling_up",       OSP_SHUTTER_ROLLING_UP, 0},
     { "shutter_unknown",          OSP_SHUTTER_UNKNOWN, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_curveType[] =
    {{ "round",                  OSP_ROUND, 0},
     { "flat",                   OSP_FLAT, 0},
     { "ribbon",                 OSP_RIBBON, 0},
     { "disjoint",               OSP_DISJOINT, 0},
     { "unknown_CURVE_TYPE",    OSP_UNKNOWN_CURVE_TYPE, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_curveBasis[] =
    {{ "linear",                 OSP_LINEAR, 0},
     { "bezier",                 OSP_BEZIER, 0},
     { "bspline",                OSP_BSPLINE, 0},
     { "hermite",                OSP_HERMITE, 0},
     { "catmull_rom",            OSP_CATMULL_ROM, 0},
     { "unknown_curve_basis",   OSP_UNKNOWN_CURVE_BASIS, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_subdivisionMode[] =
    {{ "subdivision_no_boundary",       OSP_SUBDIVISION_NO_BOUNDARY, 0},
     { "subdivision_smooth_boundary",   OSP_SUBDIVISION_SMOOTH_BOUNDARY, 0},
     { "subdivision_pin_corners",       OSP_SUBDIVISION_PIN_CORNERS, 0},
     { "subdivision_pin_boundary",      OSP_SUBDIVISION_PIN_BOUNDARY, 0},
     { "subdivision_pin_all",          OSP_SUBDIVISION_PIN_ALL, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_amrMethod[] =
    {{ "amr_current",                   OSP_AMR_CURRENT, 0},
     { "amr_finest",                    OSP_AMR_FINEST, 0},
     { "amr_octant",                    OSP_AMR_OCTANT, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_volumeFilter[] =
    {{ "volume_filter_nearest",      OSP_VOLUME_FILTER_NEAREST, 0},
     { "volume_filter_trilinear",   OSP_VOLUME_FILTER_TRILINEAR, 0},
     { "volume_filter_tricubic",   OSP_VOLUME_FILTER_TRICUBIC, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_pixelFilterTypes[] =
    {{ "pixelfilter_point",             OSP_PIXELFILTER_POINT, 0},
     { "pixelfilter_box",               OSP_PIXELFILTER_BOX, 0},
     { "pixelfilter_gauss",             OSP_PIXELFILTER_GAUSS, 0},
     { "pixelfilter_mitchell",          OSP_PIXELFILTER_MITCHELL, 0},
     { "pixelfilter_blackman_harris",  OSP_PIXELFILTER_BLACKMAN_HARRIS, 0},
     { NULL, 0, 0},
    };

osp_atom_t osp_intensityQuantity[] =
    {{ "intensity_quantity_radiance",     OSP_INTENSITY_QUANTITY_RADIANCE , 0},
     { "intensity_quantity_irradiance",   OSP_INTENSITY_QUANTITY_IRRADIANCE , 0},
     { "intensity_quantity_intensity",    OSP_INTENSITY_QUANTITY_INTENSITY , 0},
     { "intensity_quantity_power",        OSP_INTENSITY_QUANTITY_POWER , 0},
     { "intensity_quantity_scale",        OSP_INTENSITY_QUANTITY_SCALE , 0},
     { "intensity_quantity_unknown",     OSP_INTENSITY_QUANTITY_UNKNOWN, 0},
     { NULL, 0, 0},
    };
