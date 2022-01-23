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

// #include "ospray.h"

extern "C" {
#include "erl_nif.h"

    ERL_NIF_TERM atom_ok;
    ERL_NIF_TERM atom_true;
    ERL_NIF_TERM atom_false;
    ERL_NIF_TERM atom_badarg;
    ERL_NIF_TERM atom_error;
    // ErlNifResourceType* osp_mem = NULL;
    ERL_NIF_TERM osp_cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_copyData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceCommit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceGetLastErrorCode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceGetLastErrorMsg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceGetProperty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceRelease(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceRemoveParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM osp_deviceRetain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
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

#define make_error(TYPE, Desc)                                          \
    { fprintf(stderr, "OSP:%d error %s\r\n", __LINE__, Desc);           \
        fflush(stderr);                                                 \
        return enif_raise_exception(env,                                \
                                    enif_make_tuple2(env, TYPE, enif_make_string(env, Desc, ERL_NIF_LATIN1))); \
    }


/* Implementations */

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
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_deviceGetLastErrorCode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_deviceGetLastErrorMsg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_deviceGetProperty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_deviceRelease(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_deviceRemoveParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_deviceRetain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM osp_deviceSetParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
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
    make_error(atom_error, "NYI");
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
    make_error(atom_error, "NYI");
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
    make_error(atom_error, "NYI");
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
    make_error(atom_error, "NYI");
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

    // igl_mem = enif_open_resource_type(env, NULL, "igl_mem", NULL, ERL_NIF_RT_CREATE, NULL);
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
   {"deviceRelease", 1, osp_deviceRelease, 0},
   {"deviceRemoveParam", 2, osp_deviceRemoveParam, 0},
   {"deviceRetain", 1, osp_deviceRetain, 0},
   {"deviceSetParam", 4, osp_deviceSetParam, 0},
   {"geometricModel", 1, osp_geometricModel, 0},
   {"getBounds", 1, osp_getBounds, 0},
   {"getCurrentDevice", 0, osp_getCurrentDevice, 0},
   {"getProgress", 1, osp_getProgress, 0},
   {"getTaskDuration", 1, osp_getTaskDuration, 0},
   {"getVariance", 1, osp_getVariance, 0},
   {"isReady", 2, osp_isReady, 0},
   {"loadModule", 1, osp_loadModule, 0},
   {"mapFrameBuffer", 2, osp_mapFrameBuffer, 0},
   {"newCamera", 1, osp_newCamera, 0},
   {"newData", 4, osp_newData, 0},
   {"newDevice", 1, osp_newDevice, 0},
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
