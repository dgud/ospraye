/*
 *  osp_nif.cpp --
 *
 *     Erlang nif for accessing ospray functionality.
 *
 *  Copyright (c) 2022 Dan Gudmundsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifdef __WIN32__
#define WIN32
//#define WIN32_LEAN_AND_MEAN
//#include <windows.h>
#endif
#include <string.h>
#include <vector>
#include "ospray/ospray.h"

typedef enum {
              ospt_managedObject, ospt_camera, ospt_data,
              ospt_frameBuffer, ospt_future, ospt_geometricModel, ospt_geometry, ospt_group,
              ospt_imageOperation, ospt_instance, ospt_light, ospt_material, ospt_object,
              ospt_renderer, ospt_texture, ospt_transferFunction, ospt_volume,
              ospt_volumetricModel, ospt_world, ospt_device
} osp_object_t;

typedef struct {
    osp_object_t type;
    OSPObject obj;
} osp_mem_t;

extern "C" {
#include "erl_nif.h"

    ERL_NIF_TERM atom_ok;
    ERL_NIF_TERM atom_true;
    ERL_NIF_TERM atom_false;
    ERL_NIF_TERM atom_null;
    ERL_NIF_TERM atom_badarg;
    ERL_NIF_TERM atom_error;

    ErlNifResourceType* osp_resource = NULL;

    ERL_NIF_TERM osp_cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_copyData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceCommit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceGetLastErrorCode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceGetLastErrorMsg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceGetProperty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    // ERL_NIF_TERM osp_deviceRelease(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceRemoveParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    // ERL_NIF_TERM osp_deviceRetain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceSetParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newGeometricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_getBounds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_getCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_getProgress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_getTaskDuration(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_getVariance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_isReady(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_loadModule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_readFrameBuffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newCamera(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newFrameBuffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newGeometry(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newGroup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newImageOperation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newInstance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newMaterial(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newRenderer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newCopiedData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newTexture(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newTransferFunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newVolume(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newVolumetricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newWorld(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    // ERL_NIF_TERM osp_release(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_removeParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_renderFrame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_resetAccumulation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    // ERL_NIF_TERM osp_retain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_setCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_setParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    // ERL_NIF_TERM osp_shutdown(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_wait(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

}  // extern c

#include "osp_atoms.h"

#define make_error(TYPE, Desc)                                          \
    {                                                                   \
        return enif_raise_exception(env,                                \
                                    enif_make_tuple2(env, TYPE, enif_make_string(env, Desc, ERL_NIF_LATIN1))); \
    }


static void osp_make_atom(ErlNifEnv* env, osp_atom_t *entry)
{
    while(entry->name) {
        entry->atom = enif_make_atom(env, entry->name);
        entry++;
    }
}

static int osp_enum_to_atom(osp_atom_t *entry, int value, ERL_NIF_TERM *res)
{
    while(entry->name) {
        if(entry->value == value) {
            *res = entry->atom;
            return 1;
        }
        entry++;
    }
    return 0;
}

static int osp_atom_to_enum(osp_atom_t *entry, ERL_NIF_TERM value, int *res)
{
    while(entry->name) {
        if(enif_is_identical(entry->atom,value)) {
            *res = entry->value;
            return 1;
        }
        entry++;
    }
    return 0;
}


ERL_NIF_TERM osp_make_object(ErlNifEnv* env, OSPObject obj, osp_object_t type)
{
    ERL_NIF_TERM term;
    osp_mem_t *mem = (osp_mem_t *) enif_alloc_resource(osp_resource, sizeof(osp_mem_t));
    mem->obj = obj;
    mem->type = type;
    term = enif_make_resource(env, mem);
    enif_release_resource(mem);
    return term;
}

static void osp_object_gc(ErlNifEnv* env, osp_mem_t* obj)
{
    // fprintf(stderr, "destroy obj %p (%d)\r\n", obj->obj, obj->type);
    if(obj->type == ospt_device) {
        ospDeviceRelease((OSPDevice) obj->obj);
    } else {
        ospRelease(obj->obj);
    }
}


/* API Implementations */

ERL_NIF_TERM osp_cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Future");
    if(mem->type != ospt_future) make_error(atom_badarg, "Object is not a future");
    ospCancel((OSPFuture) mem->obj);
    return atom_ok;
}

ERL_NIF_TERM osp_commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Object");
    if(mem->type == ospt_device) make_error(atom_badarg, "Object is a device");
    ospCommit(mem->obj);
    return atom_ok;
}

ERL_NIF_TERM osp_copyData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *source, *dest;
    ErlNifUInt64 i1,i2,i3;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &source)) make_error(atom_badarg, "Source");
    if(source->type != ospt_data) make_error(atom_badarg, "Object is not a data object");
    if(!enif_get_resource(env, argv[1], osp_resource, (void **) &dest)) make_error(atom_badarg, "Dest");
    if(dest->type != ospt_data) make_error(atom_badarg, "Object is not a data object");

    if(!enif_get_uint64(env, argv[2], &i1)) make_error(atom_badarg, "DestIndex1");
    if(!enif_get_uint64(env, argv[3], &i2)) make_error(atom_badarg, "DestIndex2");
    if(!enif_get_uint64(env, argv[4], &i3)) make_error(atom_badarg, "DestIndex3");

    ospCopyData((OSPData)source->obj, (OSPData)dest->obj, i1, i2, i3);
    return atom_ok;
}

ERL_NIF_TERM osp_deviceCommit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Device");
    if(!mem->type == ospt_device) make_error(atom_badarg, "not a device");
    ospDeviceCommit((OSPDevice) mem->obj);
    return atom_ok;
}

ERL_NIF_TERM osp_deviceGetLastErrorCode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    ERL_NIF_TERM atom;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Device");
    if(!mem->type == ospt_device) make_error(atom_badarg, "not a device");
    OSPError error = ospDeviceGetLastErrorCode((OSPDevice) mem->obj);

    if(!osp_enum_to_atom(osp_error_code, error, &atom)) make_error(atom_error, "atom_not_found");
    return atom;
}

ERL_NIF_TERM osp_deviceGetLastErrorMsg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Device");
    if(!mem->type == ospt_device) make_error(atom_badarg, "not a device");
    const char *error = ospDeviceGetLastErrorMsg((OSPDevice) mem->obj);
    return enif_make_string(env, error, ERL_NIF_LATIN1);
}

ERL_NIF_TERM osp_deviceGetProperty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    int enum_v;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Device");
    if(!mem->type == ospt_device) make_error(atom_badarg, "not a device");
    if(!enif_is_atom(env, argv[1])) make_error(atom_badarg, "DeviceProperty");
    if(!osp_atom_to_enum(osp_deviceProperty, argv[1], &enum_v)) make_error(atom_badarg, "DeviceProperty");
    ErlNifSInt64 res = ospDeviceGetProperty((OSPDevice) mem->obj, (OSPDeviceProperty) enum_v);
    return enif_make_int64(env, res);
}

// ERL_NIF_TERM osp_deviceRelease(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
// {
//     make_error(atom_error, "NYI");
// }

ERL_NIF_TERM osp_deviceRemoveParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Device");
    if(!mem->type == ospt_device) make_error(atom_badarg, "not a device");

    ErlNifBinary id;
    if(!enif_inspect_binary(env, argv[1], &id)) make_error(atom_badarg, "Id");
    ospDeviceRemoveParam((OSPDevice) mem->obj,  (const char*) id.data);
    return atom_ok;
}

// ERL_NIF_TERM osp_deviceRetain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
// {
//     make_error(atom_error, "NYI");
// }

ERL_NIF_TERM osp_deviceSetParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    int type_v;
    void * data_ptr;
    ErlNifBinary binary;

    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Device");
    if(!mem->type == ospt_device) make_error(atom_badarg, "not a device");

    ErlNifBinary id;
    if(!enif_inspect_binary(env, argv[1], &id)) make_error(atom_badarg, "Id");

    if(!enif_is_atom(env, argv[2])) make_error(atom_badarg, "Type");
    if(!osp_atom_to_enum(osp_dataType, argv[2], &type_v)) make_error(atom_badarg, "Type");

    if(enif_is_binary(env, argv[3])) {
        if(!enif_inspect_binary(env, argv[3], &binary)) make_error(atom_badarg, "Data");
        data_ptr = binary.data;
    } else {
        osp_mem_t *obj;
        if(!enif_get_resource(env, argv[0], osp_resource, (void **) &obj)) make_error(atom_badarg, "Data");
        data_ptr = &obj->obj;
    }
    ospDeviceSetParam((OSPDevice) mem->obj,  (const char*) id.data, (OSPDataType) type_v, data_ptr);
    return atom_ok;
}

ERL_NIF_TERM osp_newGeometricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    OSPGeometry Geom;
    if(enif_is_identical(argv[0], atom_null)) {
        Geom = NULL;
    } else {
        if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem))
            make_error(atom_badarg, "Geometry");
        if(mem->type != ospt_geometry) make_error(atom_badarg, "Geometry");
        Geom = (OSPGeometry) mem->obj;
    }
    OSPGeometricModel model = ospNewGeometricModel(Geom);
    if(!model) make_error(atom_error, "invalid Geom");
    return osp_make_object(env, (OSPObject) model, ospt_geometricModel);
}

ERL_NIF_TERM osp_getBounds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Future");
    if(mem->type == ospt_device) make_error(atom_badarg, "Object is a device");
    OSPBounds bounds = ospGetBounds(mem->obj);

    return enif_make_tuple2(env,
                            enif_make_tuple3(env,
                                             enif_make_double(env, bounds.lower[0]),
                                             enif_make_double(env, bounds.lower[1]),
                                             enif_make_double(env, bounds.lower[2])),
                            enif_make_tuple3(env,
                                             enif_make_double(env, bounds.upper[0]),
                                             enif_make_double(env, bounds.upper[1]),
                                             enif_make_double(env, bounds.upper[2])));
}

ERL_NIF_TERM osp_getCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OSPDevice dev = ospGetCurrentDevice();
    if(!dev) make_error(atom_error, "invalid device");
    return osp_make_object(env, (OSPObject) dev, ospt_device);
}

ERL_NIF_TERM osp_getProgress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Future");
    if(mem->type != ospt_future) make_error(atom_badarg, "Object is not a future");
    float res = ospGetProgress((OSPFuture) mem->obj);
    return enif_make_double(env, (double) res);
}

ERL_NIF_TERM osp_getTaskDuration(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Future");
    if(mem->type != ospt_future) make_error(atom_badarg, "Object is not a future");
    float res = ospGetTaskDuration((OSPFuture) mem->obj);
    return enif_make_double(env, (double) res);
}

ERL_NIF_TERM osp_getVariance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "FrameBuffer");
    if(mem->type != ospt_frameBuffer) make_error(atom_badarg, "Object is not a FrameBuffer");
    float res = ospGetVariance((OSPFrameBuffer) mem->obj);
    return enif_make_double(env, (double) res);
}

ERL_NIF_TERM osp_isReady(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    int SyncEvent;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Device");
    if(mem->type != ospt_future) make_error(atom_badarg, "Object is not a future");
    if(!osp_atom_to_enum(osp_dataType, argv[1], &SyncEvent)) make_error(atom_badarg, "SyncEvent");
    if(ospIsReady((OSPFuture) mem->obj, (OSPSyncEvent) SyncEvent)) {
        return atom_true;
    } else {
        return atom_false;
    }
}

ERL_NIF_TERM osp_loadModule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary name;
    ERL_NIF_TERM atom;
    if(!enif_inspect_binary(env, argv[0], &name)) make_error(atom_badarg, "Name");

    OSPError res = ospLoadModule((const char *) name.data);
    if(!osp_enum_to_atom(osp_error_code, res, &atom)) make_error(atom_error, "atom_not_found");
    return atom;
}

ERL_NIF_TERM osp_readFrameBuffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x,y;
    int format, temp;
    int channel;
    unsigned char *src, *dest;
    ERL_NIF_TERM bin;
    size_t size=0;
    osp_mem_t *fb;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &fb))
        make_error(atom_badarg, "FrameBuffer");
    if(fb->type != ospt_frameBuffer) make_error(atom_badarg, "Object is not a Framebuffer");
    if(!enif_get_int(env,  argv[1], &x)) make_error(atom_badarg, "SizeX");
    if(!enif_get_int(env,  argv[2], &y)) make_error(atom_badarg, "SizeY");
    if(!osp_atom_to_enum(osp_frameBufferFormat, argv[3], &format))
        make_error(atom_badarg, "Format");
    if(!osp_atom_to_enum(osp_frameBufferChannel, argv[4], &channel))
        make_error(atom_badarg, "FrameBufferChannel");
    src = (unsigned char *) ospMapFrameBuffer((OSPFrameBuffer) fb->obj, (OSPFrameBufferChannel) channel);
    if(!src) make_error(atom_error, "could not read channel");

    switch(channel) {
    case OSP_FB_COLOR:
        {
            switch(format) {
            case OSP_FB_RGBA8:
            case OSP_FB_SRGBA:
                size = 4*sizeof(char);
                break;
            case OSP_FB_RGBA32F:
                size = 4*sizeof(float);
                break;
            default:
                size = 0;
                break;
            }
        };
        break;
    case OSP_FB_DEPTH:
        size = sizeof(float);
        break;
    case OSP_FB_ALBEDO:
    case OSP_FB_NORMAL:
        size = sizeof(float)*3;
        break;
    case OSP_FB_ACCUM:
    case OSP_FB_VARIANCE:
    default:
        size = 0;
        break;
    }
    size = size*x*y;
    dest = enif_make_new_binary(env, size, &bin);
    memcpy(dest, src, size);
    ospUnmapFrameBuffer(src, (OSPFrameBuffer) fb->obj);
    return bin;
}

ERL_NIF_TERM osp_newCamera(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary type;
    if(!enif_inspect_binary(env, argv[0], &type)) make_error(atom_badarg, "Type");
    OSPCamera obj = ospNewCamera((const char *) type.data);
    if(!obj) make_error(atom_error, "invalid camera");
    return osp_make_object(env, (OSPObject) obj, ospt_camera);
}

ERL_NIF_TERM osp_newData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int Type;
    ErlNifUInt64 NumItems1, NumItems2, NumItems3;

    if(!osp_atom_to_enum(osp_dataType, argv[0], &Type)) make_error(atom_badarg, "Type");
    if(!enif_get_uint64(env, argv[2], &NumItems1)) make_error(atom_badarg, "NumItems1");
    if(!enif_get_uint64(env, argv[4], &NumItems2)) make_error(atom_badarg, "NumItems2");
    if(!enif_get_uint64(env, argv[6], &NumItems3)) make_error(atom_badarg, "NumItems3");

    OSPData data = ospNewData((OSPDataType) Type, NumItems1, NumItems2, NumItems3);
    if(!data) make_error(atom_error, "invalid data");
    return osp_make_object(env, (OSPObject) data, ospt_data);
}

ERL_NIF_TERM osp_newDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary type;
    if(!enif_inspect_binary(env, argv[0], &type)) make_error(atom_badarg, "Type");
    OSPDevice dev = ospNewDevice((const char *) type.data);
    if(!dev) make_error(atom_error, "invalid device");
    return osp_make_object(env, (OSPObject) dev, ospt_device);
}

ERL_NIF_TERM osp_newFrameBuffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x,y;
    int format, temp;
    unsigned int channels=0;
    ERL_NIF_TERM lists_l, lists_h, lists_t;
    if(!enif_get_int(env,  argv[0], &x)) make_error(atom_badarg, "SizeX");
    if(!enif_get_int(env,  argv[1], &y)) make_error(atom_badarg, "SizeY");
    if(!osp_atom_to_enum(osp_frameBufferFormat, argv[2], &format)) make_error(atom_badarg, "Format");
    if(!enif_is_list(env, argv[3])) make_error(atom_badarg, "FrameBufferChannels");
    lists_l = argv[3];
    while(enif_get_list_cell(env, lists_l, &lists_h, &lists_t)) {
        if(!osp_atom_to_enum(osp_frameBufferChannel, lists_h, &temp))
            make_error(atom_badarg, "FrameBufferChannels");
        channels |= temp;
        lists_l = lists_t;
    }
    OSPFrameBuffer fb = ospNewFrameBuffer(x,y,(OSPFrameBufferFormat) format, channels);
    if(!fb)  make_error(atom_error, "Could not create FrameBuffer");
    return osp_make_object(env, (OSPObject) fb, ospt_frameBuffer);
}

ERL_NIF_TERM osp_newGeometry(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary type;
    if(!enif_inspect_binary(env, argv[0], &type)) make_error(atom_badarg, "Type");
    OSPGeometry obj = ospNewGeometry((const char *) type.data);
    if(!obj) make_error(atom_error, "invalid geometry type");
    return osp_make_object(env, (OSPObject) obj, ospt_geometry);
}

ERL_NIF_TERM osp_newGroup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OSPGroup obj = ospNewGroup();
    return osp_make_object(env, (OSPObject) obj, ospt_group);
}

ERL_NIF_TERM osp_newImageOperation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary type;
    if(!enif_inspect_binary(env, argv[0], &type)) make_error(atom_badarg, "Type");
    OSPImageOperation obj = ospNewImageOperation((const char *) type.data);
    return osp_make_object(env, (OSPObject) obj, ospt_imageOperation);
}

ERL_NIF_TERM osp_newInstance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem))
        make_error(atom_badarg, "Group");
    if(mem->type != ospt_group) make_error(atom_badarg, "Group");
    OSPInstance obj = ospNewInstance((OSPGroup) mem->obj);
    return osp_make_object(env, (OSPObject) obj, ospt_instance);
}

ERL_NIF_TERM osp_newLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary type;
    if(!enif_inspect_binary(env, argv[0], &type)) make_error(atom_badarg, "Type");
    OSPLight obj = ospNewLight((const char *) type.data);
    if(!obj) make_error(atom_error, "invalid light type");
    return osp_make_object(env, (OSPObject) obj, ospt_light);
}

ERL_NIF_TERM osp_newMaterial(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary type;
    if(!enif_inspect_binary(env, argv[0], &type)) make_error(atom_badarg, "Type");
    OSPMaterial obj = ospNewMaterial("unused", (const char *) type.data);
    if(!obj) make_error(atom_error, "invalid material type");
    return osp_make_object(env, (OSPObject) obj, ospt_material);
}

ERL_NIF_TERM osp_newRenderer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary type;
    if(!enif_inspect_binary(env, argv[0], &type)) make_error(atom_badarg, "Type");
    OSPRenderer obj = ospNewRenderer((const char *) type.data);
    if(!obj) make_error(atom_error, "invalid type");
    return osp_make_object(env, (OSPObject) obj, ospt_renderer);
}

ERL_NIF_TERM osp_newCopiedData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary Data;
    int Type;
    ErlNifUInt64 NumItems1, NumItems2, NumItems3;
    ErlNifSInt64 ByteStride1, ByteStride2, ByteStride3;
    void *data_ptr;
    std::vector <OSPObject> lists_vec;
    if(enif_is_binary(env, argv[0])) {
        if(!enif_inspect_binary(env, argv[0], &Data)) make_error(atom_badarg, "CopiedData");
        data_ptr = Data.data;
    } else {
        ERL_NIF_TERM lists_l, lists_h, lists_t;
        osp_mem_t *mem;
        if(!enif_is_list(env, argv[0])) make_error(atom_badarg, "CopiedData");
        lists_l = argv[0];
        while(enif_get_list_cell(env, lists_l, &lists_h, &lists_t)) {
            if(!enif_get_resource(env, lists_h, osp_resource, (void **) &mem))
                make_error(atom_badarg, "CopiedData");
            lists_vec.push_back(mem->obj);
            lists_l = lists_t;
        };
        data_ptr = lists_vec.data();
    }

    if(!enif_is_atom(env, argv[1])) make_error(atom_badarg, "Type");
    if(!osp_atom_to_enum(osp_dataType, argv[1], &Type)) make_error(atom_badarg, "Type");

    if(!enif_get_uint64(env, argv[2], &NumItems1)) make_error(atom_badarg, "NumItems1");
    if(!enif_get_int64(env,  argv[3], &ByteStride1)) make_error(atom_badarg, "ByteStride1");
    if(!enif_get_uint64(env, argv[4], &NumItems2)) make_error(atom_badarg, "NumItems2");
    if(!enif_get_int64(env,  argv[5], &ByteStride2)) make_error(atom_badarg, "ByteStride2");
    if(!enif_get_uint64(env, argv[6], &NumItems3)) make_error(atom_badarg, "NumItems3");
    if(!enif_get_int64(env,  argv[7], &ByteStride3)) make_error(atom_badarg, "ByteStride3");

    OSPData temp = ospNewSharedData(data_ptr, (OSPDataType) Type,
                                    NumItems1, ByteStride1,
                                    NumItems2, ByteStride2,
                                    NumItems3, ByteStride3);
    if(!temp) make_error(atom_error, "invalid data");
    // Copy data, so it can be gc'ed/deleted in erlang
    OSPData obj = ospNewData((OSPDataType) Type, NumItems1, NumItems2, NumItems3);
    ospCopyData(temp, obj, 0, 0, 0);
    ospRelease(temp);
    return osp_make_object(env, (OSPObject) obj, ospt_data);
}

ERL_NIF_TERM osp_newTexture(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary type;
    if(!enif_inspect_binary(env, argv[0], &type)) make_error(atom_badarg, "Type");
    OSPTexture obj = ospNewTexture((const char *) type.data);
    if(!obj) make_error(atom_error, "invalid texture type");
    return osp_make_object(env, (OSPObject) obj, ospt_texture);
}

ERL_NIF_TERM osp_newTransferFunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary type;
    if(!enif_inspect_binary(env, argv[0], &type)) make_error(atom_badarg, "Type");
    OSPTransferFunction obj = ospNewTransferFunction((const char *) type.data);
    if(!obj) make_error(atom_error, "invalid transferfunction type");
    return osp_make_object(env, (OSPObject) obj, ospt_transferFunction);
}

ERL_NIF_TERM osp_newVolume(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary type;
    if(!enif_inspect_binary(env, argv[0], &type)) make_error(atom_badarg, "Type");
    OSPVolume obj = ospNewVolume((const char *) type.data);
    if(!obj) make_error(atom_error, "invalid type");
    return osp_make_object(env, (OSPObject) obj, ospt_volume);
}

ERL_NIF_TERM osp_newVolumetricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    OSPVolume Volume;
    if(enif_is_identical(argv[0], atom_null)) {
        Volume = NULL;
    } else {
        if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem))
            make_error(atom_badarg, "Volume");
        if(mem->type != ospt_volume) make_error(atom_badarg, "Volume");
        Volume = (OSPVolume) mem->obj;
    }
    OSPVolumetricModel model = ospNewVolumetricModel(Volume);
    if(!model) make_error(atom_error, "invalid Volume");
    return osp_make_object(env, (OSPObject) model, ospt_volumetricModel);
}

ERL_NIF_TERM osp_newWorld(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OSPWorld obj = ospNewWorld();
    return osp_make_object(env, (OSPObject) obj, ospt_world);
}

// ERL_NIF_TERM osp_release(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
// {
//     make_error(atom_error, "NYI");
// }

ERL_NIF_TERM osp_removeParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Object");
    if(mem->type == ospt_device) make_error(atom_badarg, "is a device");

    ErlNifBinary id;
    if(!enif_inspect_binary(env, argv[1], &id)) make_error(atom_badarg, "Id");
    ospRemoveParam(mem->obj,  (const char*) id.data);
    return atom_ok;
}

ERL_NIF_TERM osp_renderFrame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *frameb, *renderer, *camera, *world;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &frameb))
        make_error(atom_badarg, "FrameBuffer");
    if(frameb->type != ospt_frameBuffer) make_error(atom_badarg, "FrameBuffer");

    if(!enif_get_resource(env, argv[1], osp_resource, (void **) &renderer))
        make_error(atom_badarg, "Renderer");
    if(renderer->type != ospt_renderer) make_error(atom_badarg, "Renderer");

    if(!enif_get_resource(env, argv[2], osp_resource, (void **) &camera))
        make_error(atom_badarg, "Camera");
    if(camera->type != ospt_camera) make_error(atom_badarg, "Camera");

    if(!enif_get_resource(env, argv[3], osp_resource, (void **) &world))
        make_error(atom_badarg, "World");
    if(world->type != ospt_world) make_error(atom_badarg, "World");

    OSPFuture future = ospRenderFrame((OSPFrameBuffer) frameb, (OSPRenderer) renderer,
                                      (OSPCamera) camera, (OSPWorld) world);
    if(!future) make_error(atom_error, "could not create renderer");
    return osp_make_object(env, (OSPObject) future, ospt_future);
}

ERL_NIF_TERM osp_resetAccumulation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem))
        make_error(atom_badarg, "FrameBuffer");
    if(mem->type != ospt_frameBuffer) make_error(atom_badarg, "FrameBuffer");
    ospResetAccumulation((OSPFrameBuffer) mem->obj);
    return atom_ok;
}

// ERL_NIF_TERM osp_retain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
// {
//     make_error(atom_error, "NYI");
// }

ERL_NIF_TERM osp_setCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Device");
    if(!mem->type == ospt_device) make_error(atom_badarg, "not a device");
    ospSetCurrentDevice((OSPDevice) mem->obj);
    return atom_ok;
}

// Set a parameter, where 'mem' points the address of the type specified
ERL_NIF_TERM osp_setParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    int type_v;
    void * data_ptr;
    ErlNifBinary binary;

    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Object");
    if(mem->type == ospt_device) make_error(atom_badarg, "is a device");

    ErlNifBinary id;
    if(!enif_inspect_binary(env, argv[1], &id)) make_error(atom_badarg, "Id");

    if(!enif_is_atom(env, argv[2])) make_error(atom_badarg, "Type");
    if(!osp_atom_to_enum(osp_dataType, argv[2], &type_v)) make_error(atom_badarg, "Type");

    if(enif_is_binary(env, argv[3])) {
        if(!enif_inspect_binary(env, argv[3], &binary)) make_error(atom_badarg, "Data");
        data_ptr = binary.data;
    } else {
        osp_mem_t *obj;
        if(!enif_get_resource(env, argv[0], osp_resource, (void **) &obj)) make_error(atom_badarg, "Data");
        data_ptr = &obj->obj;
    }
    ospSetParam(mem->obj,  (const char*) id.data, (OSPDataType) type_v, data_ptr);
    return atom_ok;

}

// ERL_NIF_TERM osp_shutdown(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
// {
//     make_error(atom_error, "NYI");
// }

ERL_NIF_TERM osp_wait(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Device");
    if(mem->type != ospt_future) make_error(atom_badarg, "Object is not a future");
    ospWait((OSPFuture) mem->obj);
    return atom_ok;
}


/* Initialization */

extern "C" {
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok     = enif_make_atom(env, "ok");
    atom_true   = enif_make_atom(env, "true");
    atom_false  = enif_make_atom(env, "false");
    atom_null   = enif_make_atom(env, "null");
    atom_badarg = enif_make_atom(env, "badarg");
    atom_error  = enif_make_atom(env, "error");
    osp_make_atom(env, osp_logLevel);
    osp_make_atom(env, osp_deviceProperty);
    osp_make_atom(env, osp_dataType);
    osp_make_atom(env, osp_textureFormat);
    osp_make_atom(env, osp_texturefilter);
    osp_make_atom(env, osp_error_code);
    osp_make_atom(env, osp_frameBufferFormat);
    osp_make_atom(env, osp_frameBufferChannel);
    osp_make_atom(env, osp_syncEvent);
    osp_make_atom(env, osp_unstructuredCellType);
    osp_make_atom(env, osp_stereoMode);
    osp_make_atom(env, osp_shutterType);
    osp_make_atom(env, osp_curveType);
    osp_make_atom(env, osp_curveBasis);
    osp_make_atom(env, osp_subdivisionMode);
    osp_make_atom(env, osp_amrMethod);
    osp_make_atom(env, osp_volumeFilter);
    osp_make_atom(env, osp_pixelFilterTypes);
    osp_make_atom(env, osp_intensityQuantity);

    osp_resource = enif_open_resource_type(env, NULL, "osp_obj",
                                           (ErlNifResourceDtor*) osp_object_gc,
                                           ERL_NIF_RT_CREATE, NULL);
    return 0;
}

static ErlNifFunc nif_funcs[] =
  {
   {"cancel", 1, osp_cancel, 0},
   {"commit", 1, osp_commit, 0},
   {"copyData", 5, osp_copyData, 0},
   {"deviceCommit", 1, osp_deviceCommit, 0},
   {"deviceGetLastErrorCode", 1, osp_deviceGetLastErrorCode, 0},
   {"deviceGetLastErrorMsg", 1, osp_deviceGetLastErrorMsg, 0},
   {"deviceGetProperty", 2, osp_deviceGetProperty, 0},
   // {"deviceRelease", 1, osp_deviceRelease, 0},
   {"deviceRemoveParam_nif", 2, osp_deviceRemoveParam, 0},
   // {"deviceRetain", 1, osp_deviceRetain, 0},
   {"deviceSetParam_nif", 4, osp_deviceSetParam, 0},
   {"newGeometricModel", 1, osp_newGeometricModel, 0},
   {"getBounds", 1, osp_getBounds, 0},
   {"getCurrentDevice", 0, osp_getCurrentDevice, 0},
   {"getProgress", 1, osp_getProgress, 0},
   {"getTaskDuration", 1, osp_getTaskDuration, ERL_NIF_DIRTY_JOB_CPU_BOUND},
   {"getVariance", 1, osp_getVariance, 0},
   {"isReady", 2, osp_isReady, 0},
   {"loadModule_nif", 1, osp_loadModule, 0},
   {"readFrameBuffer", 5, osp_readFrameBuffer, 0},
   {"newCamera_nif", 1, osp_newCamera, 0},
   {"newData", 4, osp_newData, 0},
   {"newDevice_nif", 1, osp_newDevice, 0},
   {"newFrameBuffer", 4, osp_newFrameBuffer, 0},
   {"newGeometry_nif", 1, osp_newGeometry, 0},
   {"newGroup", 0, osp_newGroup, 0},
   {"newImageOperation", 1, osp_newImageOperation, 0},
   {"newInstance", 1, osp_newInstance, 0},
   {"newLight_nif", 1, osp_newLight, 0},
   {"newMaterial_nif", 1, osp_newMaterial, 0},
   {"newRenderer", 1, osp_newRenderer, 0},
   {"newCopiedData_nif", 8, osp_newCopiedData, 0},
   {"newTexture_nif", 1, osp_newTexture, 0},
   {"newTransferFunction_nif", 1, osp_newTransferFunction, 0},
   {"newVolume_nif", 1, osp_newVolume, 0},
   {"newVolumetricModel", 1, osp_newVolumetricModel, 0},
   {"newWorld", 0, osp_newWorld, 0},
   // {"release", 1, osp_release, 0},
   {"removeParam_nif", 2, osp_removeParam, 0},
   {"renderFrame", 4, osp_renderFrame, 0},
   {"resetAccumulation", 1, osp_resetAccumulation, 0},
   // {"retain", 1, osp_retain, 0},
   {"setCurrentDevice", 1, osp_setCurrentDevice, 0},
   {"setParam_nif", 4, osp_setParam, 0},
   // {"shutdown", 0, osp_shutdown, 0},
   {"wait", 2, osp_wait, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  };
    ERL_NIF_INIT(osp,nif_funcs,load,NULL,NULL,NULL)

}  // extern c
