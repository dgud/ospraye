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

extern "C" {
#include "erl_nif.h"

    ERL_NIF_TERM atom_ok;
    ERL_NIF_TERM atom_true;
    ERL_NIF_TERM atom_false;
    ERL_NIF_TERM atom_badarg;
    ERL_NIF_TERM atom_error;
    // ErlNifResourceType* osp_mem = NULL;
    ERL_NIF_TERM cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM copyData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM deviceCommit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM deviceGetLastErrorCode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM deviceGetLastErrorMsg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM deviceGetProperty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM deviceRelease(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM deviceRemoveParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM deviceRetain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM deviceSetParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM geometricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM getBounds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM getCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM getProgress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM getTaskDuration(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM getVariance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM isReady(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM loadModule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM mapFrameBuffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newCamera(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newFrameBuffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newGeometry(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newGroup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newImageOperation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newInstance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newMaterial(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newRenderer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newSharedData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newTexture(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newTransferFunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newVolume(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newVolumetricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM newWorld(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM release(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM removeParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM renderFrame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM resetAccumulation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM retain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM setCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM setParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM shutdown(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM wait(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

}  // extern c

#define make_error(TYPE, Desc)                                          \
    { fprintf(stderr, "OSP:%d error %s\r\n", __LINE__, Desc);           \
        fflush(stderr);                                                 \
        return enif_raise_exception(env,                                \
                                    enif_make_tuple2(env, TYPE, enif_make_string(env, Desc, ERL_NIF_LATIN1))); \
    }


/* Implementations */

ERL_NIF_TERM cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM copyData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM deviceCommit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM deviceGetLastErrorCode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM deviceGetLastErrorMsg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM deviceGetProperty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM deviceRelease(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM deviceRemoveParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM deviceRetain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM deviceSetParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM geometricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM getBounds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM getCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM getProgress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM getTaskDuration(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM getVariance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM isReady(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM loadModule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM mapFrameBuffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newCamera(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newFrameBuffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newGeometry(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newGroup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newImageOperation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newInstance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newMaterial(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newRenderer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newSharedData(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newTexture(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newTransferFunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newVolume(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newVolumetricModel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM newWorld(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM release(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM removeParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM renderFrame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM resetAccumulation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM retain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM setCurrentDevice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM setParam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM shutdown(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    make_error(atom_error, "NYI");
}

ERL_NIF_TERM wait(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
   {"cancel", 1, cancel, 0},
   {"commit", 1, commit, 0},
   {"copyData", 5, copyData, 0},
   {"deviceCommit", 1, deviceCommit, 0},
   {"deviceGetLastErrorCode", 1, deviceGetLastErrorCode, 0},
   {"deviceGetLastErrorMsg", 1, deviceGetLastErrorMsg, 0},
   {"deviceGetProperty", 2, deviceGetProperty, 0},
   {"deviceRelease", 1, deviceRelease, 0},
   {"deviceRemoveParam", 2, deviceRemoveParam, 0},
   {"deviceRetain", 1, deviceRetain, 0},
   {"deviceSetParam", 4, deviceSetParam, 0},
   {"geometricModel", 1, geometricModel, 0},
   {"getBounds", 1, getBounds, 0},
   {"getCurrentDevice", 0, getCurrentDevice, 0},
   {"getProgress", 1, getProgress, 0},
   {"getTaskDuration", 1, getTaskDuration, 0},
   {"getVariance", 1, getVariance, 0},
   {"isReady", 2, isReady, 0},
   {"loadModule", 1, loadModule, 0},
   {"mapFrameBuffer", 2, mapFrameBuffer, 0},
   {"newCamera", 1, newCamera, 0},
   {"newData", 4, newData, 0},
   {"newDevice", 1, newDevice, 0},
   {"newFrameBuffer", 4, newFrameBuffer, 0},
   {"newGeometry", 1, newGeometry, 0},
   {"newGroup", 0, newGroup, 0},
   {"newImageOperation", 1, newImageOperation, 0},
   {"newInstance", 1, newInstance, 0},
   {"newLight", 1, newLight, 0},
   {"newMaterial", 1, newMaterial, 0},
   {"newRenderer", 1, newRenderer, 0},
   {"newSharedData", 8, newSharedData, 0},
   {"newTexture", 1, newTexture, 0},
   {"newTransferFunction", 1, newTransferFunction, 0},
   {"newVolume", 1, newVolume, 0},
   {"newVolumetricModel", 1, newVolumetricModel, 0},
   {"newWorld", 0, newWorld, 0},
   {"release", 1, release, 0},
   {"removeParam", 2, removeParam, 0},
   {"renderFrame", 4, renderFrame, 0},
   {"resetAccumulation", 1, resetAccumulation, 0},
   {"retain", 1, retain, 0},
   {"setCurrentDevice", 1, setCurrentDevice, 0},
   {"setParam", 4, setParam, 0},
   {"shutdown", 0, shutdown, 0},
   {"wait", 2 wait, 0},
  };
ERL_NIF_INIT(osp,nif_funcs,load,NULL,NULL,NULL)

}  // extern c
