%%
%%   API testing for ospraye
%%
%%   Copyright Dan Gudmundsson
%%

-module(osp_SUITE).

-compile(export_all).

all() ->
    [
     convert,
     loadModule,
     device,
     camera,
     geometry
    ].

-define(get_error(Call),
        fun() ->
                try Call of should_not_happen -> exit(fail)
                catch error:Reason -> Reason
                end
        end()).

convert(_Config) ->
    Ref = make_ref(),
    Ref = osp:single_data(device, Ref),
    _ = ?get_error(osp:single_data(vec3u, Ref)),

    <<"a", 0:8>> = osp:single_data(string, "a"),  %% Adds eos
    <<"a", 0:8>> = osp:single_data(string, <<"a">>),  %% Adds eos

    <<1:32/native>> = osp:single_data(bool, true),
    <<0:32/native>> = osp:single_data(bool, false),
    _ = ?get_error(osp:single_data(bool, falseasd)),
    _ = ?get_error(osp:single_data(boolenan, true)),
    _ = ?get_error(osp:single_data(boolenan, false)),

    <<13:8>> = osp:single_data(char, 13),
    <<13:8, 15:8>> = osp:single_data(vec2uc, [13,15]),
    <<13:8, 15:8, 17:8, 19:8>> = osp:single_data(vec4uc, {13,15, 17, 19}),
    _ = ?get_error(osp:single_data(vec3u, [foo, asd])),

    <<13:16/native>> = osp:single_data(short, 13),
    <<13:16/native>> = osp:single_data(ushort, 13),

    <<13:16/float-native, _:16>> = osp:single_data(vec2h, [13, 25]),
    <<13.0:32/native-float, _:32/native-float>> = osp:single_data(box1f, [13, 0.234]),
    <<1.0:64/native-float, 2.0:64/native-float>> = osp:single_data(vec2d, {1.0, 2}),

    <<0.0:16/native-float, _:10/binary>> = osp:multiple_data(half, index_1()),
    <<0.0:32/native-float, _:20/binary>> = osp:multiple_data(vec3f, index_1()),
    <<0.0:64/native-float, _:40/binary>> = osp:multiple_data(vec2d, index_1()),

    Data2 = [{-1, 2}, {1, 42}],
    <<-1:8/signed-native, _:3/binary>> = osp:multiple_data(vec2c, Data2),
    <<65535:16/native, _:6/binary>> = osp:multiple_data(vec2us, Data2),
    <<-1.0:16/native-float, _:6/binary>> = osp:multiple_data(vec2h, Data2),
    <<-1:32/signed-native, _:12/binary>> = osp:multiple_data(vec2i, Data2),
    <<-1.0:32/float-native, _:12/binary>> = osp:multiple_data(vec2f, Data2),
    <<-1.0:64/float-native, _:24/binary>> = osp:multiple_data(vec2d, Data2),

    Data3 = [{-1, 2, 0}, {1, 42, 0}],
    <<255:8/native, _:5/binary>> = osp:multiple_data(vec3uc, Data3),
    <<-1:16/signed-native, _:10/binary>> = osp:multiple_data(vec3s, Data3),
    <<-1.0:16/native-float, _:10/binary>> = osp:multiple_data(vec3h, Data3),
    <<-1:32/signed-native, _:20/binary>> = osp:multiple_data(vec3i, Data3),
    <<-1.0:32/float-native, _:20/binary>> = osp:multiple_data(vec3f, Data3),
    <<-1.0:64/float-native, _:40/binary>> = osp:multiple_data(vec3d, Data3),

    Data4 = [{-1, 2, 0, 4}, {1, 42, 0, 4}],
    <<255:8/native, _:7/binary>> = osp:multiple_data(vec4uc, Data4),
    <<-1:16/signed-native,  _:14/binary>> = osp:multiple_data(vec4s, Data4),
    <<-1.0:16/native-float, _:14/binary>> = osp:multiple_data(vec4h, Data4),
    <<-1:32/signed-native,  _:28/binary>> = osp:multiple_data(vec4i, Data4),
    <<-1.0:32/float-native, _:28/binary>> = osp:multiple_data(vec4f, Data4),
    <<-1.0:64/float-native, _:56/binary>> = osp:multiple_data(vec4d, Data4),

    [Ref, Ref] = osp:multiple_data(object, [Ref, Ref]),
    _ = ?get_error(osp:multiple_data(object, [foo, Ref])),
    ok.

loadModule(_Config) ->
    invalid_operation = osp:loadModule("foobar"),
    no_error = osp:loadModule("ispc"),
    ok.

device(_Config) ->
    no_error = osp:loadModule("ispc"),
    {error, "invalid device"} = ?get_error(osp:newDevice(foobar)),
    {error, "invalid device"} = ?get_error(osp:newDevice("foobar")),
    Dev = osp:newDevice(cpu),

    Props = [{Prop, osp:deviceGetProperty(Dev, Prop)} || Prop <- [version, major, minor, patch, so_version]],
    io:format("Dev props ~p~n",[Props]),
    {badarg, "Device"} = ?get_error(osp:deviceGetProperty("foobar", 1)),
    {badarg, "DeviceProperty"} = ?get_error(osp:deviceGetProperty(Dev, 1)),

    no_error = osp:deviceGetLastErrorCode(Dev),
    "no error" = osp:deviceGetLastErrorMsg(Dev),

    {error, "invalid device"} = ?get_error(osp:getCurrentDevice()),
    ok = osp:deviceCommit(Dev),
    ok = osp:setCurrentDevice(Dev),
    Dev2 = osp:getCurrentDevice(),
    false = Dev == Dev2,  %% Notice this, do we need a compare functionality

    osp:deviceSetParam(Dev, numThreads, int, <<4:32/native>>),
    osp:deviceSetParam(Dev, "setAffinity", bool, false),
    osp:deviceSetParam(Dev, numThreads, int, 16),

    osp:deviceRemoveParam(Dev, "setAffinity"),
    ok = osp:deviceCommit(Dev),
    no_error = osp:deviceGetLastErrorCode(Dev),

    ok.

camera(_Config) ->
    no_error = osp:loadModule("ispc"),
    Dev = osp:newDevice(cpu),
    ok = osp:deviceCommit(Dev),
    ok = osp:setCurrentDevice(Dev),
    no_error = osp:deviceGetLastErrorCode(Dev),

    Cam_pos  = {0.0, 0, 0},
    Cam_up   = {0.0, 1, 0},
    Cam_view = {0.1, 0, 1},

    Camera = osp:newCamera(perspective),
%%     ospSetFloat(camera, "aspect", imgSize_x / (float)imgSize_y);
    osp:setParam(Camera, "position", vec3f, Cam_pos),
    osp:setParam(Camera, "direction", vec3f, Cam_view),
    osp:setParam(Camera, "up", vec3f, Cam_up),
    osp:commit(Camera), %%  commit each object to indicate modifications are done
    no_error = osp:deviceGetLastErrorCode(Dev),
    %% _ = ?get_error(osp:newCamera(foobar)),
    %% _ = osp:deviceGetLastErrorCode(Dev),
    osp:setParam(Camera, "direction_foobar", vec3f, Cam_view),
    %% No Error here :-(
    no_error = osp:deviceGetLastErrorCode(Dev),
    ok.

geometry(_Config) ->
    Mesh = osp:newGeometry("mesh"),
    Vs = vertex_pos_1(),
    NumOfVs = length(Vs) div 3,
    4 = NumOfVs,
    Data0 = osp:newCopiedData(Vs, vec3f, 4),
    %%  alternatively with an OSPRay managed OSPData
    %%  OSPData managed = ospNewData1D(OSP_VEC3F, 4);
    %%  ospCopyData1D(data, managed, 0);

    osp:commit(Data0),
    osp:setObject(Mesh, "vertex.position", Data0),

    Data1 = osp:newCopiedData(colors_1(), vec4f, 4),
    osp:commit(Data1),
    osp:setObject(Mesh, "vertex.color", Data1),
    %% ospRelease(data),  Not needed gc'ed when not used

    Data2 = osp:newCopiedData(index_1(), vec3ui, 2),
    osp:commit(Data2),
    osp:setObject(Mesh, "index", Data2),
    %% ospRelease(data),

    osp:commit(Mesh).



vertex_pos_1() ->
    [-1.0, -1.0, 3.0, -1.0, 1.0, 3.0, 1.0, -1.0, 3.0, 0.1, 0.1, 0.3].
colors_1() ->
    [0.9, 0.5, 0.5, 1.0, 0.8, 0.8, 0.8, 1.0, 0.8, 0.8, 0.8, 1.0, 0.5, 0.9, 0.5, 1.0].
index_1() ->
    [0, 1, 2, 1, 2, 3].
