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
    ERL_NIF_TERM osp_geometricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_getBounds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_getCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_getProgress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_getTaskDuration(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_getVariance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_isReady(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_loadModule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_mapFrameBuffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
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
    ERL_NIF_TERM osp_newSharedData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newTexture(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newTransferFunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newVolume(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newVolumetricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_newWorld(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_release(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_removeParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_renderFrame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_resetAccumulation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_retain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_setCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_setParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_shutdown(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
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
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_copyData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
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

    if(!enif_is_atom(env, argv[2])) make_error(atom_badarg, "DeviceProperty");
    if(!osp_atom_to_enum(osp_dataType, argv[2], &type_v)) make_error(atom_badarg, "Type");

    if(enif_is_binary(env, argv[3])) {
        if(!enif_inspect_binary(env, argv[3], &binary)) make_error(atom_badarg, "Data");
        data_ptr = binary.data;
    } else {
        osp_mem_t *obj;
        if(!enif_get_resource(env, argv[0], osp_resource, (void **) &obj)) make_error(atom_badarg, "Data");
        data_ptr = mem->obj;
    }
    ospDeviceSetParam((OSPDevice) mem->obj,  (const char*) id.data, (OSPDataType) type_v, data_ptr);
    return atom_ok;
}

ERL_NIF_TERM osp_geometricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_getBounds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_getCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OSPDevice dev = ospGetCurrentDevice();
    if(!dev) make_error(atom_error, "invalid device");
    return osp_make_object(env, (OSPObject) dev, ospt_device);
}

ERL_NIF_TERM osp_getProgress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_getTaskDuration(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_getVariance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_isReady(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
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

ERL_NIF_TERM osp_mapFrameBuffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newCamera(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
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
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newGeometry(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newGroup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newImageOperation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newInstance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newMaterial(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newRenderer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newSharedData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newTexture(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newTransferFunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newVolume(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newVolumetricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_newWorld(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_release(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_removeParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_renderFrame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_resetAccumulation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_retain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_setCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    osp_mem_t *mem;
    if(!enif_get_resource(env, argv[0], osp_resource, (void **) &mem)) make_error(atom_badarg, "Device");
    if(!mem->type == ospt_device) make_error(atom_badarg, "not a device");
    ospSetCurrentDevice((OSPDevice) mem->obj);
    return atom_ok;
}

ERL_NIF_TERM osp_setParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_shutdown(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_wait(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}


/* Initialization */

extern "C" {
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok     = enif_make_atom(env, "ok");
    atom_true   = enif_make_atom(env, "true");
    atom_false  = enif_make_atom(env, "false");
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
   {"geometricModel", 1, osp_geometricModel, 0},
   {"getBounds", 1, osp_getBounds, 0},
   {"getCurrentDevice", 0, osp_getCurrentDevice, 0},
   {"getProgress", 1, osp_getProgress, 0},
   {"getTaskDuration", 1, osp_getTaskDuration, 0},
   {"getVariance", 1, osp_getVariance, 0},
   {"isReady", 2, osp_isReady, 0},
   {"loadModule_nif", 1, osp_loadModule, 0},
   {"mapFrameBuffer", 2, osp_mapFrameBuffer, 0},
   {"newCamera", 1, osp_newCamera, 0},
   {"newData", 4, osp_newData, 0},
   {"newDevice_nif", 1, osp_newDevice, 0},
   {"newFrameBuffer", 4, osp_newFrameBuffer, 0},
   {"newGeometry", 1, osp_newGeometry, 0},
   {"newGroup", 0, osp_newGroup, 0},
   {"newImageOperation", 1, osp_newImageOperation, 0},
   {"newInstance", 1, osp_newInstance, 0},
   {"newLight", 1, osp_newLight, 0},
   {"newMaterial", 1, osp_newMaterial, 0},
   {"newRenderer", 1, osp_newRenderer, 0},
   {"newSharedData", 8, osp_newSharedData, 0},
   {"newTexture", 1, osp_newTexture, 0},
   {"newTransferFunction", 1, osp_newTransferFunction, 0},
   {"newVolume", 1, osp_newVolume, 0},
   {"newVolumetricModel", 1, osp_newVolumetricModel, 0},
   {"newWorld", 0, osp_newWorld, 0},
   {"release", 1, osp_release, 0},
   {"removeParam", 2, osp_removeParam, 0},
   {"renderFrame", 4, osp_renderFrame, 0},
   {"resetAccumulation", 1, osp_resetAccumulation, 0},
   {"retain", 1, osp_retain, 0},
   {"setCurrentDevice", 1, osp_setCurrentDevice, 0},
   {"setParam", 4, osp_setParam, 0},
   {"shutdown", 0, osp_shutdown, 0},
   {"wait", 2, osp_wait, 0},
  };
    ERL_NIF_INIT(osp,nif_funcs,load,NULL,NULL,NULL)
    
}  // extern c
