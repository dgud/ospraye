%%
%%   Erlang Wrapper library for OSPray
%%
%%   Copyright: Dan Gudmundsson
%%   Enum types

-type logLevel() ::
        debug              %% OSP_LOG_DEBUG = 1,
      | info               %% OSP_LOG_INFO = 2,
      | warning            %% OSP_LOG_WARNING = 3,
      | error              %% OSP_LOG_ERROR = 4,
      | none.              %% OSP_LOG_NONE = 5

-type deviceProperty() ::
        version            %% OSP_DEVICE_VERSION = 0,
      | major              %% OSP_DEVICE_VERSION_MAJOR = 1,
      | minor              %% OSP_DEVICE_VERSION_MINOR = 2,
      | patch              %% OSP_DEVICE_VERSION_PATCH = 3,
      | so_version.        %% OSP_DEVICE_SO_VERSION = 4

%% An enum type that represensts the different data types represented in OSPRay
-type dataType() ::
        %% Object reference type.
        device            % OSP_DEVICE = 100,
        %% Void pointer type.
      | void_ptr          % OSP_VOID_PTR = 200,
        %% Booleans, same size as OSP_INT.
      | bool              % OSP_BOOL = 250,
        %% highest bit to represent objects/handles
      | object            % OSP_OBJECT = 0x8000000,
        %% object subtypes
      | data              % OSP_DATA = 0x8000000 + 100,
      | camera            % OSP_CAMERA,
      | framebuffer       % OSP_FRAMEBUFFER,
      | future            % OSP_FUTURE,
      | geometric_model   % OSP_GEOMETRIC_MODEL,
      | geometry          % OSP_GEOMETRY,
      | group             % OSP_GROUP,
      | image_operation   % OSP_IMAGE_OPERATION,
      | instance          % OSP_INSTANCE,
      | light             % OSP_LIGHT,
      | material          % OSP_MATERIAL,
      | renderer          % OSP_RENDERER,
      | texture           % OSP_TEXTURE,
      | transfer_function % OSP_TRANSFER_FUNCTION,
      | volume            % OSP_VOLUME,
      | volumetric_model  % OSP_VOLUMETRIC_MODEL,
      | world             % OSP_WORLD,

        %% Pointer to a C-style NULL-terminated character string.
      | string            % OSP_STRING = 1500,
        %% Character scalar type.
      | char              % OSP_CHAR = 2000,
      | vec2c             % OSP_VEC2C,
      | vec3c             % OSP_VEC3C,
      | vec4c             % OSP_VEC4C,

        %% Unsigned character scalar and vector types.
      | uchar             % OSP_UCHAR = 2500,
      | vec2uc            % OSP_VEC2UC,
      | vec3uc            % OSP_VEC3UC,
      | vec4uc            % OSP_VEC4UC,
      | byte              % XXX OSP_UCHAR, ISPC issue #124
      | raw               % XXX OSP_UCHAR, ISPC issue #124

        %% Signed 16-bit integer scalar.
      | short             % OSP_SHORT = 3000,
      | vec2s             % OSP_VEC2S,
      | vec3s             % OSP_VEC3S,
      | vec4s             % OSP_VEC4S,

        %% Unsigned 16-bit integer scalar.
      | ushort            % OSP_USHORT = 3500,
      | vec2us            % OSP_VEC2US,
      | vec3us            % OSP_VEC3US,
      | vec4us            % OSP_VEC4US,

        %% Signed 32-bit integer scalar and vector types.
      | int               % OSP_INT = 4000,
      | vec2i             % OSP_VEC2I,
      | vec3i             % OSP_VEC3I,
      | vec4i             % OSP_VEC4I,

        %% Unsigned 32-bit integer scalar and vector types.
      | uint              % OSP_UINT = 4500,
      | vec2ui            % OSP_VEC2UI,
      | vec3ui            % OSP_VEC3UI,
      | vec4ui            % OSP_VEC4UI,

        %% Signed 64-bit integer scalar and vector types.
      | long              % OSP_LONG = 5000,
      | vec2l             % OSP_VEC2L,
      | vec3l             % OSP_VEC3L,
      | vec4l             % OSP_VEC4L,

        %% Unsigned 64-bit integer scalar and vector types.
      | ulong              % OSP_ULONG = 5550,
      | vec2ul             % OSP_VEC2UL,
      | vec3ul             % OSP_VEC3UL,
      | vec4ul             % OSP_VEC4UL,

        %% Half precision floating point scalar and vector types (IEEE 754
        %% `binary16`).
      | half              % OSP_HALF = 5800,
      | vec2h             % OSP_VEC2H,
      | vec3h             % OSP_VEC3H,
      | vec4h             % OSP_VEC4H,

        %% Single precision floating point scalar and vector types.
      | float             % OSP_FLOAT = 6000,
      | vec2f             % OSP_VEC2F,
      | vec3f             % OSP_VEC3F,
      | vec4f             % OSP_VEC4F,

        %% Double precision floating point scalar type.
      | double            % OSP_DOUBLE = 7000,
      | vec2d             % OSP_VEC2D,
      | vec3d             % OSP_VEC3D,
      | vec4d             % OSP_VEC4D,

        %% Signed 32-bit integer N-dimensional box types
      | box1i             % OSP_BOX1I = 8000,
      | box2i             % OSP_BOX2I,
      | box3i             % OSP_BOX3I,
      | box4i             % OSP_BOX4I,

        %% Single precision floating point N-dimensional box types
      | box1f             % OSP_BOX1F = 10000,
      | box2f             % OSP_BOX2F,
      | box3f             % OSP_BOX3F,
      | box4f             % OSP_BOX4F,

        %% Transformation types
      | linear2f          % OSP_LINEAR2F = 12000,
      | linear3f          % OSP_LINEAR3F,
      | affine2f          % OSP_AFFINE2F,
      | affine3f          % OSP_AFFINE3F,

      | quatf             % OSP_QUATF,

        %% Guard value.
      | unknown.          % OSP_UNKNOWN = 9999999

%% OSPRay format constants for Texture creation
-type textureFormat() ::
        texture_rgba8          % OSP_TEXTURE_RGBA8,
      | texture_srgba          % OSP_TEXTURE_SRGBA,
      | texture_rgba32f        % OSP_TEXTURE_RGBA32F,
      | texture_rgb8           % OSP_TEXTURE_RGB8,
      | texture_srgb           % OSP_TEXTURE_SRGB,
      | texture_rgb32f         % OSP_TEXTURE_RGB32F,
      | texture_r8             % OSP_TEXTURE_R8,
      | texture_r32f           % OSP_TEXTURE_R32F,
      | texture_l8             % OSP_TEXTURE_L8,
      | texture_ra8            % OSP_TEXTURE_RA8,
      | texture_la8            % OSP_TEXTURE_LA8,
      | texture_rgba16         % OSP_TEXTURE_RGBA16,
      | texture_rgb16          % OSP_TEXTURE_RGB16,
      | texture_ra16           % OSP_TEXTURE_RA16,
      | texture_r16            % OSP_TEXTURE_R16,
        %% Denotes an unknown texture format, so we can properly initialize parameters
      | texture_invalid.       % OSP_TEXTURE_FORMAT_INVALID = 255,

%% Filter modes that can be set on 'texture2d' type OSPTexture
-type texturefilter() ::
        texture_bilinear       % OSP_TEXTURE_FILTER_BILINEAR = 0, %% default bilinear interpolation
      | texture_nearest.       % OSP_TEXTURE_FILTER_NEAREST %% use nearest-neighbor interpolation

%% Error codes returned by various API and callback functions
-type error_code() ::
        no_error            % OSP_NO_ERROR          = 0  %% No error has been recorded
      | unknown_error       % OSP_UNKNOWN_ERROR     = 1, %% An unknown error has occurred
      | invalid_argument    % OSP_INVALID_ARGUMENT  = 2, %% An invalid argument is specified
      | invalid_operation   % OSP_INVALID_OPERATION = 3, %% The operation is not allowed for the specified object
      | out_of_MEMORY       % OSP_OUT_OF_MEMORY     = 4, %% There is not enough memory left to execute the command
      | unsupported_cpu     % OSP_UNSUPPORTED_CPU   = 5, %% The CPU is not supported as it does not support SSE4.1
      | version_mismatch.   % OSP_VERSION_MISMATCH  = 6, %% A module could not be loaded due to mismatching version

%% OSPRay format constants for Frame Buffer creation
-type frameBufferFormat() ::
        fb_none     % OSP_FB_NONE,              % framebuffer will not be mapped by application  OSP_FB_RGBA8,
                                                % one dword per pixel: rgb+alpha, each one byte
      | fb_srgba    % OSP_FB_SRGBA, %% one dword per pixel: rgb (in sRGB space) + alpha, each one byte
      | fb_rgba32f. % OSP_FB_RGBA32F, % one float4 per pixel: rgb+alpha, each one float

%% OSPRay channel constants for Frame Buffer (can be OR'ed together)
-type frameBufferChannel() ::
        fb_color    % OSP_FB_COLOR = (1 << 0),
      | fb_depth    % OSP_FB_DEPTH = (1 << 1),
      | fb_accum    % OSP_FB_ACCUM = (1 << 2),
      | fb_variance % OSP_FB_VARIANCE = (1 << 3),
      | fb_normal   % OSP_FB_NORMAL = (1 << 4), %% in world-space
      | fb_albedo.  % OSP_FB_ALBEDO = (1 << 5)

%% OSPRay events which can be waited on via ospWait()
-type syncEvent() ::
        none_finished     % OSP_NONE_FINISHED = 0,
      | world_rendered    % OSP_WORLD_RENDERED = 10
      | world_committed   % OSP_WORLD_COMMITTED = 20
      | frame_finished    % OSP_FRAME_FINISHED = 30,
      | task_finished.    % OSP_TASK_FINISHED = 100000

%% OSPRay cell types definition for unstructured volumes, values are set to
%% match VTK
-type unstructuredCellType() ::
        tetrahedron          % OSP_TETRAHEDRON = 10,
      | hexahedron           % OSP_HEXAHEDRON = 12,
      | wedge                % OSP_WEDGE = 13,
      | pyramid              % OSP_PYRAMID = 14,
      | unknown_cell_type.   % OSP_UNKNOWN_CELL_TYPE = 255

%% OSPRay camera stereo image modes
-type stereoMode() ::
        stereo_none         % OSP_STEREO_NONE,
      | stereo_left         % OSP_STEREO_LEFT,
      | stereo_right        % OSP_STEREO_RIGHT,
      | stereo_side_by_side % OSP_STEREO_SIDE_BY_SIDE,
      | stereo_top_bottom   % OSP_STEREO_TOP_BOTTOM,
      | stereo_unknown.     % OSP_STEREO_UNKNOWN = 255

-type shutterType() ::
        shutter_global          % OSP_SHUTTER_GLOBAL,
      | shutter_rolling_right   % OSP_SHUTTER_ROLLING_RIGHT,
      | shutter_rolling_left    % OSP_SHUTTER_ROLLING_LEFT,
      | shutter_rolling_down    % OSP_SHUTTER_ROLLING_DOWN,
      | shutter_rolling_up      % OSP_SHUTTER_ROLLING_UP,
      | shutter_unknown.         % OSP_SHUTTER_UNKNOWN = 255

-type curveType() ::
        round                 % OSP_ROUND,
      | flat                  % OSP_FLAT,
      | ribbon                % OSP_RIBBON,
      | disjoint              % OSP_DISJOINT,
      | unknown_CURVE_TYPE.   % OSP_UNKNOWN_CURVE_TYPE = 255

-type curveBasis() ::
        linear                % OSP_LINEAR,
      | bezier                % OSP_BEZIER,
      | bspline               % OSP_BSPLINE,
      | hermite               % OSP_HERMITE,
      | catmull_rom           % OSP_CATMULL_ROM,
      | unknown_curve_basis.  % OSP_UNKNOWN_CURVE_BASIS = 255

-type subdivisionMode() ::
        subdivision_no_boundary      % OSP_SUBDIVISION_NO_BOUNDARY,
      | subdivision_smooth_boundary  % OSP_SUBDIVISION_SMOOTH_BOUNDARY,
      | subdivision_pin_corners      % OSP_SUBDIVISION_PIN_CORNERS,
      | subdivision_pin_boundary     % OSP_SUBDIVISION_PIN_BOUNDARY,
      | subdivision_pin_all.         % OSP_SUBDIVISION_PIN_ALL


%%  % %% AMR Volume rendering methods
-type amrMethod() ::
        amr_current                  % OSP_AMR_CURRENT,
      | amr_finest                   % OSP_AMR_FINEST,
      | amr_octant.                   % OSP_AMR_OCTANT

%% Filter modes that can be set on 'VDB' type OSPVolume, compatible with VKL
-type volumeFilter() ::
       volume_filter_nearest     % OSP_VOLUME_FILTER_NEAREST = 0, %% treating voxel cell as constant
      | volume_filter_trilinear  % OSP_VOLUME_FILTER_TRILINEAR = 100, %% default trilinear interpolation
      | volume_filter_tricubic.  % OSP_VOLUME_FILTER_TRICUBIC = 200 %% tricubic interpolation

%% OSPRay pixel filter types
-type pixelFilterTypes() ::
        pixelfilter_point            % OSP_PIXELFILTER_POINT,
      | pixelfilter_box              % OSP_PIXELFILTER_BOX,
      | pixelfilter_gauss            % OSP_PIXELFILTER_GAUSS,
      | pixelfilter_mitchell         % OSP_PIXELFILTER_MITCHELL,
      | pixelfilter_blackman_harris. % OSP_PIXELFILTER_BLACKMAN_HARRIS

%%  % %% OSPRay light quantity types
-type intensityQuantity() ::
        intensity_quantity_radiance    % OSP_INTENSITY_QUANTITY_RADIANCE, %% unit W/sr/m^2
      | intensity_quantity_irradiance  % OSP_INTENSITY_QUANTITY_IRRADIANCE, %% unit W/m^2
      | intensity_quantity_intensity   % OSP_INTENSITY_QUANTITY_INTENSITY, %% radiant intensity, unit W/sr
      | intensity_quantity_power       % OSP_INTENSITY_QUANTITY_POWER, %% radiant flux, unit W
      | intensity_quantity_scale       % OSP_INTENSITY_QUANTITY_SCALE, %% linear scaling factor for the built-in type
      | intensity_quantity_unknown.    % OSP_INTENSITY_QUANTITY_UNKNOWN

