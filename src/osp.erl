%%
%%   Erlang Wrapper library for OSPray
%%
%%   Copyright Dan Gudmundsson
%%
%%
%%   Todo:
%%      Fix thread for wait (instead of stealing/blocking a dirty thread)
%%      Helper functions: pick?  logging?
%%      Better build (download and build, and fix windows)
%%      Docs
%%

-module(osp).
-export([cancel/1,
         commit/1,
         copyData/2, copyData/3, copyData/4, copyData/5,
         deviceCommit/1,
         deviceGetLastErrorCode/1,
         deviceGetLastErrorMsg/1,
         deviceGetProperty/2,
         %% deviceRelease/1,
         deviceRemoveParam/2,
         %% deviceRetain/1,
         deviceSetParam/4,
         newGeometricModel/0, newGeometricModel/1,
         getBounds/1,
         getCurrentDevice/0,
         getProgress/1,
         %% getTaskDuration/1,
         getVariance/1,
         init/0, init/1,
         isReady/1, isReady/2,
         loadModule/1,
         readFrameBuffer/4, readFrameBuffer/5,
         newCamera/1,
         newData/2, newData/3, newData/4,
         newDevice/0, newDevice/1,
         newFrameBuffer/2, newFrameBuffer/3, newFrameBuffer/4,
         newGeometry/1,
         newGroup/0,
         newImageOperation/1,
         newInstance/1,
         newLight/1,
         newMaterial/1,
         newRenderer/1,
         newCopiedData/3, newCopiedData/4, newCopiedData/5, newCopiedData/6, newCopiedData/7, newCopiedData/8,
         newTexture/1,
         newTransferFunction/1,
         newVolume/1,
         newVolumetricModel/0, newVolumetricModel/1,
         newWorld/0,
         %%  release/1,
         removeParam/2,
         renderFrame/4,
         resetAccumulation/1,
         %% retain/1,
         setCurrentDevice/1,
         setParam/4,
         %% Shortcuts to setParam/4
         setString/3, setObject/3, setBool/3, setInt/3, setFloat/3, setObjectAsData/4,
         subscribe/1, subscribe/2,
         %% shutdown/0,
         wait/1, wait/2
        ]).

-export([single_data/2, multiple_data/2]).

-include("osp_types.hrl").

-define(nif_stub,nif_stub_error(?LINE)).
-on_load(nif_init/0).

-type managedObject() :: term().
-type device() :: term().
-type camera() :: managedObject().
-type data() :: managedObject().
-type frameBuffer() :: managedObject().
-type future() :: managedObject().
-type geometricModel() :: managedObject().
-type geometry() :: managedObject().
-type group() :: managedObject().
-type imageOperation() :: managedObject().
-type instance() :: managedObject().
-type light() :: managedObject().
-type material() :: managedObject().
-type object() :: managedObject().
-type renderer() :: managedObject().
-type texture() :: managedObject().
-type transferFunction() :: managedObject().
-type volume() :: managedObject().
-type volumetricModel() :: managedObject().
-type world() :: managedObject().

-type osp_id() :: atom() | string() | binary().
-type param_data() :: binary() | managedObject() |
                      string() |atom() |
                      number() | [number()] |
                      {number(), number()} | {number(), number(), number()} |
                      {number(), number(), number(), number()}.
-type multiple_data() :: binary() | [managedObject()] | [number()] | [tuple()] | [[number()]].

%% Device Initialization

-spec init() -> device().
init() ->
    init([]).

-type init_param() ::
        {debug, boolean()} |
        {logLevel, logLevel()} |
        {warnAsError, boolean()} |
        {logOutput, cerr | cout} |
        {errorOutput, cerr | cout} |
        {device, string()} |
        {loadModules, [string()]} |
        {numThreads, non_neg_integer()} |
        {setAffinity, boolean()}.

-spec init([init_param()]) -> device().
init(Params) ->
    init_impl(Params).

%% returns the OSPRay Version in use by the device
-spec deviceGetProperty(Dev::device(), deviceProperty()) -> integer().
deviceGetProperty(_Dev, _DeviceProperty) -> ?nif_stub.

%% Shutdown the Ray engine...effectively deletes whatever device is currently
%% set.

%% -spec shutdown() -> ok.
%% shutdown() -> ?nif_stub.

%% @doc
%% Create an Ray engine backend using explicit device string.
%% ospray always provides a <<"cpu">> device.
-spec newDevice() -> device().
newDevice() -> newDevice(cpu).
-spec newDevice(DeviceType::unicode:chardata()|atom()) -> device().
newDevice(Type) -> newDevice_nif(id_to_string(Type)).
newDevice_nif(_DeviceType) -> ?nif_stub.

%% Set current device the API responds to
-spec setCurrentDevice(device()) -> ok.
setCurrentDevice(_Device) -> ?nif_stub.

%% Get the currently set device
-spec getCurrentDevice() -> device().
getCurrentDevice() -> ?nif_stub.

-spec deviceSetParam(Dev::device(), Id::osp_id(), Type::dataType(), Mem::param_data()) -> ok.
deviceSetParam(Device, Id, Type, Mem) ->
    deviceSetParam_nif(Device, id_to_string(Id), Type, single_data(Type, Mem)).
deviceSetParam_nif(_Device, _Id, _DataType, _Mem) -> ?nif_stub.

-spec deviceRemoveParam(Dev::device(), Id::osp_id()) -> ok.
deviceRemoveParam(Device, Id) ->
    deviceRemoveParam_nif(Device, id_to_string(Id)).
deviceRemoveParam_nif(_Device, _Id) -> ?nif_stub.

%% Status message callback function type
%% typedef void (*StatusCallback)(void *userData, const char *messageText)
%% typedef void (*StatusCallback)(void *userData, const char *messageText)

%% Set callback for given Device to call when a status message occurs
%% void DeviceSetStatusCallback(Device, StatusCallback, void *userData);

%% Error message callback function type
%% typedef void (*ErrorCallback)(void *userData, Error, const char *errorDetails);

%% Set callback for given Device to call when an error occurs
%% void DeviceSetErrorCallback( Device, ErrorCallback, void *userData);

%% Get the Error code for the last error that has occurred on the device
-spec deviceGetLastErrorCode(Device::device()) -> error_code().
deviceGetLastErrorCode(_Device) -> ?nif_stub.

%% Get the message for the last error that has occurred on the device
-spec deviceGetLastErrorMsg(Device::device()) -> unicode:chardata().
deviceGetLastErrorMsg(_Device) -> ?nif_stub.

%% Commit parameters on a given device
-spec deviceCommit(device()) -> ok.
deviceCommit(_Device) -> ?nif_stub.

%% %% Device handle lifetimes
%% -spec deviceRelease(device()) -> ok.
%% deviceRelease(_Device) -> ?nif_stub.
%% -spec deviceRetain(device()) -> ok.
%% deviceRetain(_Device) -> ?nif_stub.

%% Load module 'name' from shared lib libray_module_<name>.so
%% returns Error value to report any errors during initialization
-spec loadModule(Name :: unicode:chardata()) -> error_code().
loadModule(Name) ->
    loadModule_nif(unicode:characters_to_binary([Name|[0]])).
loadModule_nif(_Name) ->
    ?nif_stub.

%% OSPRay Data Arrays %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

-spec newCopiedData(CopiedData :: multiple_data(), Type :: dataType(), NumItems1 ::integer()) ->
          data().
newCopiedData(CopiedData, Type, NumItems1) ->
    newCopiedData(CopiedData, Type, NumItems1,0,  1,0, 1,0).

-spec newCopiedData(CopiedData :: multiple_data(), Type :: dataType(),
                    NumItems1 ::integer(), ByteStride1 :: integer()) -> data().
newCopiedData(CopiedData, Type, NumItems1, ByteStride1) ->
    newCopiedData(CopiedData, Type, NumItems1,ByteStride1,  1,0, 1,0).

-spec newCopiedData(CopiedData :: multiple_data(), Type :: dataType(),
                    NumItems1 ::integer(), ByteStride1 :: integer(),
                    NumItems2 ::integer()) -> data().
newCopiedData(CopiedData, Type, NumItems1, ByteStride1, NumItems2) ->
    newCopiedData(CopiedData, Type, NumItems1,ByteStride1,  NumItems2,0, 1,0).

-spec newCopiedData(CopiedData :: multiple_data(), Type :: dataType(),
                    NumItems1 ::integer(), ByteStride1 :: integer(),
                    NumItems2 ::integer(), ByteStride2 :: integer()) -> data().
newCopiedData(CopiedData, Type, NumItems1, ByteStride1, NumItems2, ByteStride2) ->
    newCopiedData(CopiedData, Type, NumItems1,ByteStride1,  NumItems2,ByteStride2, 1,0).

-spec newCopiedData(CopiedData :: multiple_data(), Type :: dataType(),
                    NumItems1 ::integer(), ByteStride1 :: integer(),
                    NumItems2 ::integer(), ByteStride2 :: integer(),
                    NumItems3 ::integer()) -> data().
newCopiedData(CopiedData, Type, NumItems1, ByteStride1, NumItems2, ByteStride2, NumItems3) ->
    newCopiedData(CopiedData, Type, NumItems1,ByteStride1,  NumItems2, ByteStride2, NumItems3,0).

-spec newCopiedData(CopiedData :: multiple_data(), Type :: dataType(),
                    NumItems1 ::integer(), ByteStride1 :: integer(),
                    NumItems2 ::integer(), ByteStride2 :: integer(),
                    NumItems3 ::integer(), ByteStride3 :: integer()
                   ) ->  data().
newCopiedData(CopiedData, Type, NumItems1, ByteStride1, NumItems2, ByteStride2, NumItems3, ByteStride3) ->
    newCopiedData_nif(multiple_data(Type, CopiedData), Type,
                      NumItems1, ByteStride1, NumItems2, ByteStride2, NumItems3, ByteStride3).
newCopiedData_nif(_CopiedData, _Type,
                  _NumItems1, _ByteStride1, _NumItems2, _ByteStride2, _NumItems3, _ByteStride3) -> ?nif_stub.

-spec newData(Type::dataType(), NumItems1::integer()) ->  data().
newData(Type, NumItems1) -> newData(Type, NumItems1, 1, 1).
-spec newData(Type::dataType(), NumItems1::integer(), NumItems2::integer()) ->  data().
newData(Type, NumItems1, NumItems2) -> newData(Type, NumItems1, NumItems2, 1).
-spec newData(Type::dataType(), NumItems1::integer(), NumItems2::integer(), NumItems3::integer()) ->  data().
newData(_Type, _NumItems1, _NumItems2, _NumItems3) -> ?nif_stub.

-spec copyData(Source::data(), Destination::data()) -> ok.
copyData(Source, Destination) ->
    copyData(Source, Destination, 0, 0, 0).
-spec copyData(Source::data(), Destination::data(), DestinationIndex1::integer()) -> ok.
copyData(Source, Destination, DestinationIndex1) ->
    copyData(Source, Destination, DestinationIndex1,0, 0).
-spec copyData(Source::data(), Destination::data(), DestinationIndex1::integer(),DestinationIndex2::integer()) -> ok.
copyData(Source, Destination, DestinationIndex1,DestinationIndex2) ->
    copyData(Source, Destination, DestinationIndex1,DestinationIndex2, 0).
-spec copyData(Source::data(), Destination::data(), DestinationIndex1::integer(),DestinationIndex2::integer(), DestinationIndex3::integer()) -> ok.
copyData(_Source, _Destination, _DestinationIndex1, _DestinationIndex2, _DestinationIndex3) -> ?nif_stub.

%% Renderable Objects %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

-spec newLight(Type::osp_id()) -> light().
newLight(Type) -> newLight_nif(id_to_string(Type)).
newLight_nif(_Type) -> nif_stub.

-spec newCamera(Type::osp_id()) -> camera().
newCamera(Type) -> newCamera_nif(id_to_string(Type)).
newCamera_nif(_Type) -> nif_stub.

-spec newGeometry(Type::osp_id()) -> geometry().
newGeometry(Type) -> newGeometry_nif(id_to_string(Type)).
newGeometry_nif(_Type) -> nif_stub.

-spec newVolume(Type::osp_id()) -> volume().
newVolume(Type) -> newVolume_nif(id_to_string(Type)).
newVolume_nif(_Type) -> nif_stub.

-spec newGeometricModel() -> geometricModel().
newGeometricModel() -> newGeometricModel(null).
-spec newGeometricModel(Geom::geometry() | null) -> geometricModel().
newGeometricModel(_Geom) -> ?nif_stub.

-spec newVolumetricModel() -> volumetricModel().
newVolumetricModel() -> newVolumetricModel(null).
-spec newVolumetricModel(Volume::volume() | null) -> volumetricModel().
newVolumetricModel(_Volume) -> ?nif_stub.

% Model Meta-Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec newMaterial(MaterialType::osp_id()) ->  material().
newMaterial(MaterialType) -> newMaterial_nif(id_to_string(MaterialType)).
newMaterial_nif(_MaterialType) -> ?nif_stub.

-spec newTransferFunction(Type::osp_id()) ->  transferFunction().
newTransferFunction(Type) -> newTransferFunction_nif(id_to_string(Type)).
newTransferFunction_nif(_type) -> ?nif_stub.

-spec newTexture(Type::osp_id()) ->  texture().
newTexture(Type) -> newTexture_nif(id_to_string(Type)).
newTexture_nif(_type) -> ?nif_stub.

%% Instancing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

-spec newGroup() -> group().
newGroup() -> ?nif_stub.

-spec newInstance(Group::group()) ->  instance().
newInstance(_Group) -> ?nif_stub.

%% Top-level Worlds %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

-spec newWorld() ->  world().
newWorld() -> ?nif_stub.

%% Return bounds if the object is able (World, Instance, and Group)
-spec getBounds(Object :: object()) ->  {{float(), float(), float()}, {float(), float(), float()}}.
getBounds(_Object) -> ?nif_stub.

%% Object + Parameter Lifetime Management %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

-spec setString(Object::object(), Id::osp_id(), String::osp_id()) ->  ok.
setString(Obj, Id, String) ->
    setParam_nif(Obj, id_to_string(Id), string, id_to_string(String)).

-spec setObject(Object::object(), Id::osp_id(), Other::object()) ->  ok.
setObject(Obj, Id, Other) ->
    setParam_nif(Obj, id_to_string(Id), object, Other).

-spec setBool(Object::object(), Id::osp_id(), Bool::boolean()) ->  ok.
setBool(Obj, Id, Bool) ->
    setParam_nif(Obj, id_to_string(Id), bool, single_data(bool, Bool)).

-spec setInt(Object::object(), Id::osp_id(), Num::integer()) ->  ok.
setInt(Obj, Id, Num) ->
    setParam_nif(Obj, id_to_string(Id), int, single_data(int, Num)).

-spec setFloat(Object::object(), Id::osp_id(), Num::float()) ->  ok.
setFloat(Obj, Id, Num) ->
    setParam_nif(Obj, id_to_string(Id), float, single_data(float, Num)).

-spec setObjectAsData(Object::object(), Id::osp_id(), Type::dataType(), Other::object()) ->  ok.
setObjectAsData(Obj, Id, Type, Other) ->
    setParam(Obj, Id, Type, osp:newCopiedData([Other], Type, 1)).

-spec setParam(Object::object(), Id::osp_id(), Type :: dataType(), Mem :: param_data()) ->  ok.
setParam(Device, Id, Type, Mem) ->
    setParam_nif(Device, id_to_string(Id), Type, single_data(Type, Mem)).
setParam_nif(_Object, _id, _Type, _mem) -> ?nif_stub.

-spec removeParam(Object::object(), Id::osp_id()) ->  ok.
removeParam(Object, Id) ->
    removeParam_nif(Object, id_to_string(Id)).
removeParam_nif(_Object, _id) -> ?nif_stub.

%% Make parameters which have been set visible to the object
-spec commit(Object::object()) -> ok.
commit(_Object) -> ?nif_stub.

%% %% Reduce the application-side object ref count by 1
%% -spec release(Object::object()) ->  ok.
%% release(_Object) -> ?nif_stub.

%% %% Increace the application-side object ref count by 1
%% -spec retain(Object::object()) ->  ok.
%% retain(_Object) -> ?nif_stub.

%% FrameBuffer Manipulation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

-spec newFrameBuffer(SizeX::integer(), SizeY::integer()) -> frameBuffer().
newFrameBuffer(SizeX, SizeY) ->
    newFrameBuffer(SizeX, SizeY, fb_srbga, [fb_color]).
-spec newFrameBuffer(SizeX::integer(), SizeY::integer(), Format::frameBufferFormat()) -> frameBuffer().
newFrameBuffer(SizeX, SizeY, Format) ->
    newFrameBuffer(SizeX, SizeY, Format, [fb_color]).
-spec newFrameBuffer(SizeX::integer(), SizeY::integer(),
                     Format::frameBufferFormat(), FrameBufferChannels::[frameBufferChannel()]) ->
          frameBuffer().
newFrameBuffer(_size_x, _size_y, _format, _frameBufferChannels) -> ?nif_stub.

-spec newImageOperation(Type::osp_id()) ->  imageOperation().
newImageOperation(Type) -> newImageOperation_nif(id_to_string(Type)).
newImageOperation_nif(_type) -> ?nif_stub.

%% @doc
%%   Returns the specified framebuffer content
%%   SizeX, SizeY and Format must be the same as when created with newFramebuffer/3
%%
%%   Does ospMapFramebuffer(), copies data to binary and ospUnmapframebuffer().
-spec readFrameBuffer(FB::frameBuffer(), SizeX::integer(), SizeY::integer(),
                      Format::frameBufferFormat()) ->  binary().
readFrameBuffer(FB,X,Y, Format) -> readFrameBuffer(FB, X,Y, Format, fb_color).
-spec readFrameBuffer(FB::frameBuffer(), SizeX::integer(), SizeY::integer(),
                      Format::frameBufferFormat(), FBC::frameBufferChannel()) ->  binary().
readFrameBuffer(_FrameBuffer, _X, _Y, _Format, _FrameBufferChannel) -> ?nif_stub.

%% Unmap a previously mapped frame buffer pointer
%% -spec unmapFrameBuffer(const void *mapped, FrameBuffer) ->  void.
%% unmapFrameBuffer(const void *mapped, FrameBuffer) -> ?nif_stub.

%% Get variance from last rendered frame
-spec getVariance(FB::frameBuffer()) ->  float().
getVariance(_FrameBuffer) -> ?nif_stub.

%% Reset frame buffer accumulation for next render frame call
-spec resetAccumulation(FB::frameBuffer()) ->  ok.
resetAccumulation(_FrameBuffer) -> ?nif_stub.

%% Frame Rendering %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec newRenderer(Type::osp_id()) ->  renderer().
newRenderer(Type) -> newRenderer_nif(id_to_string(Type)).
newRenderer_nif(_type) -> ?nif_stub.

%% Render a frame (non-blocking), return a future to the task executed by Ray
-spec renderFrame(frameBuffer(), renderer(), camera(), world()) -> future().
renderFrame(_FrameBuffer, _Renderer, _Camera, _World) -> ?nif_stub.

%% Ask if all events tracked by an Future handle have been completed
-spec isReady(future()) -> boolean().
isReady(Future) -> isReady(Future, task_finished).
-spec isReady(future(), syncEvent()) ->  boolean().
isReady(_Future, _SyncEvent) -> ?nif_stub.

%% Wait on a specific event
-spec wait(future()) -> ok.
wait(Future) -> wait(Future, task_finished).
-spec wait(future(), syncEvent()) -> ok.
wait(Future, Event) ->
    wait_nif(Future, Event).

wait_nif(_Future, _SyncEvent) -> ?nif_stub.

%% Cancel the given task (may block calling thread)
-spec cancel(future()) -> ok.
cancel(_Future) -> ?nif_stub.

%% Get the completion state of the given task [0.f-1.f]
-spec getProgress(future()) ->  float().
getProgress(_Future) -> ?nif_stub.

%% subscribe(Future, Event) -> ok
%% Send msg caller when Event have happend
subscribe(Future) ->
    subscribe(Future, task_finished).

subscribe(Future, Event) ->
    Me = self(),
    Wait = fun() ->
                   Me ! {self(), go},
                   wait_nif(Future, Event),
                   Me ! {?MODULE, Future, Event}
           end,
    Pid = spawn_link(Wait),
    receive
        {'EXIT', Pid, Error} -> error(Error);
        {Pid, go} ->
            receive {'EXIT', Pid, Error1} -> error(Error1)
            after 0 -> ok
            end
    end.

%% %% Get the execution duration (in seconds) state of the given task
%% -spec getTaskDuration(future()) ->  float().
%% getTaskDuration(_Future) ->
%%     ?nif_stub.

%% Helpers
%% Strings need a closing end of string
id_to_string(Id) when is_atom(Id) ->
    unicode:characters_to_binary([atom_to_binary(Id)|[0]]);
id_to_string(Id) when is_list(Id); is_binary(Id) ->
    unicode:characters_to_binary([Id|[0]]).

single_data(string, Data) ->
    id_to_string(Data);
single_data(_, Data) when is_binary(Data) ->
    Data;
single_data(Type, Data) when is_reference(Data) ->
    true =:= is_object(Type) orelse error({badarg, Type, Data}),
    Data;
single_data(bool, Data) ->
    Fun = convert(bool),
    try Fun(Data)
    catch _:_ -> error({badarg, bool, Data})
    end;
single_data(Type, EnumAtom) when is_atom(EnumAtom) ->
    (Type =:= int orelse Type =:= uint) orelse error({badarg, Type, EnumAtom}),
    %% Let driver deal with this
    EnumAtom;
single_data(Type, Data) ->
    Fun = convert(Type),
    try Fun(Data)
    catch _:_ -> error({badarg, Type, Data})
    end.

multiple_data(_Type, Data) when is_binary(Data) ->
    Data;
multiple_data(Type, [Entry|_] = Data) when is_reference(Entry) ->
    true = is_object(Type),
    Data;
multiple_data(Type, [Entry|_] = Data) when is_list(Entry); is_tuple(Entry) ->
    Fun = convert(Type),
    try << <<(Fun(E))/binary>> || E <- Data >>
    catch _:_ -> error({badarg, Type, Data})
    end;
multiple_data(Type, Data) when is_list(Data) ->
    {Sz, FlatType} = flat_type(Type),
    Fun = convert(FlatType),
    try  Bin = << <<(Fun(E))/binary>> || E <- Data >>,
         0 = byte_size(Bin) rem Sz,  %% Assert
         Bin
    catch _:_ -> error({badarg, Type, Data})
    end.

flat_type(T) when T =:= vec2c; T =:= vec2uc ->
    {2, char};
flat_type(T) when T =:= vec3c; T =:= vec3uc ->
    {3, char};
flat_type(T) when T =:= vec4c; T =:= vec4uc ->
    {4, char};
flat_type(T) when T =:= vec2s; T =:= vec2us ->
    {4, short};
flat_type(T) when T =:= vec3s; T =:= vec3us ->
    {6, short};
flat_type(T) when T =:= vec4s; T =:= vec4us ->
    {8, short};
flat_type(T) when T =:= vec2i; T =:= vec2ui; box1i ->
    {8, int};
flat_type(T) when T =:= vec3i; T =:= vec3ui ->
    {12, int};
flat_type(T) when T =:= vec4i; T =:= vec4ui; box2i ->
    {16, int};
flat_type(T) when T =:= box3i ->
    {24, int};
flat_type(T) when T =:= box4i ->
    {32, int};
flat_type(T) when T =:= vec2l; T =:= vec2ul ->
    {16, long};
flat_type(T) when T =:= vec3l; T =:= vec3ul ->
    {24, long};
flat_type(T) when T =:= vec4l; T =:= vec4ul ->
    {32, long};
flat_type(T) when T =:= vec2h ->
    {4, half};
flat_type(T) when T =:= vec3h ->
    {6, half};
flat_type(T) when T =:= vec4h ->
    {8, half};
flat_type(T) when T =:= vec2f; T =:= box1f ->
    {8, float};
flat_type(T) when T =:= vec3f ->
    {12, float};
flat_type(T) when T =:= vec4f; T=:=box2f; T =:= linear2f; T =:= quatf ->
    {16, float};
flat_type(T) when T =:= vec2d ->
    {16, double};
flat_type(T) when T =:= vec3d ->
    {24, double};
flat_type(T) when T =:= vec4d ->
    {32, double};
flat_type(T) when T =:= box3f; T =:= affine2f ->
    {24, float};
flat_type(T) when T =:= box4f ->
    {32, float};
flat_type(T) when T =:= linear3f ->
    {36, float};
flat_type(T) when T =:= affine3f ->
    {48, float};

flat_type(Flat) ->
    {1, Flat}.  %% Ignore sz


convert(bool) ->
    fun(true)  -> <<1:32/native>>;
       (false) -> <<0:32/native>>
    end;

convert(T) when T =:= char; T =:= uchar, T =:= byte, T =:= raw ->
    fun(Num) -> <<Num:8/native>> end;
convert(T) when T =:= vec2c; T =:= vec2uc ->
    fun({N1, N2}) -> <<N1:8/native, N2:8/native>>;
       ([N1, N2]) -> <<N1:8/native, N2:8/native>>
    end;
convert(T) when T =:= vec3c; T =:= vec3uc ->
    fun({N1, N2, N3}) -> <<N1:8/native, N2:8/native, N3:8/native>>;
       ([N1, N2, N3]) -> <<N1:8/native, N2:8/native, N3:8/native>>
    end;
convert(T) when T =:= vec4c; T =:= vec4uc ->
    fun({N1, N2, N3, N4}) -> <<N1:8/native, N2:8/native, N3:8/native, N4:8/native>>;
       ([N1, N2, N3, N4]) -> <<N1:8/native, N2:8/native, N3:8/native, N4:8/native>>
    end;

convert(T) when T =:= short; T =:= ushort ->
    fun(Num) -> <<Num:16/native>> end;
convert(T) when T =:= vec2s; T =:= vec2us ->
    fun({N1, N2}) -> <<N1:16/native, N2:16/native>>;
       ([N1, N2]) -> <<N1:16/native, N2:16/native>>
    end;
convert(T) when T =:= vec3s; T =:= vec3us ->
    fun({N1, N2, N3}) -> <<N1:16/native, N2:16/native, N3:16/native>>;
       ([N1, N2, N3]) -> <<N1:16/native, N2:16/native, N3:16/native>>
    end;
convert(T) when T =:= vec4s; T =:= vec4us ->
    fun({N1, N2, N3, N4}) -> <<N1:16/native, N2:16/native, N3:16/native, N4:16/native>>;
       ([N1, N2, N3, N4]) -> <<N1:16/native, N2:16/native, N3:16/native, N4:16/native>>
    end;

convert(T) when T =:= int; T =:= uint ->
    fun(Num) -> <<Num:32/native>> end;
convert(T) when T =:= vec2i; T =:= vec2ui; T =:= box1i ->
    fun({N1, N2}) -> <<N1:32/native, N2:32/native>>;
       ([N1, N2]) -> <<N1:32/native, N2:32/native>>
    end;
convert(T) when T =:= vec3i; T =:= vec3ui ->
    fun({N1, N2, N3}) -> <<N1:32/native, N2:32/native, N3:32/native>>;
       ([N1, N2, N3]) -> <<N1:32/native, N2:32/native, N3:32/native>>
    end;
convert(T) when T =:= vec4i; T =:= vec4ui; T =:= box2i ->
    fun({N1, N2, N3, N4}) ->
            <<N1:32/native, N2:32/native, N3:32/native, N4:32/native>>;
       ([N1, N2, N3, N4]) ->
            <<N1:32/native, N2:32/native, N3:32/native, N4:32/native>>
    end;
convert(T) when T =:= box3i ->
    fun({N1, N2, N3, N4, N5, N6}) ->
            <<N1:32/native, N2:32/native, N3:32/native,
              N4:32/native, N5:32/native, N6:32/native>>;
       ([N1, N2, N3, N4, N5, N6]) ->
            <<N1:32/native, N2:32/native, N3:32/native,
              N4:32/native, N5:32/native, N6:32/native>>
    end;
convert(T) when T =:= box4i ->
    fun({N1, N2, N3, N4, N5, N6, N7, N8}) ->
            <<N1:32/native, N2:32/native, N3:32/native, N4:32/native,
              N5:32/native, N6:32/native, N7:32/native, N8:32/native>>;
       ([N1, N2, N3, N4, N5, N6, N7, N8]) ->
            <<N1:32/native, N2:32/native, N3:32/native, N4:32/native,
              N5:32/native, N6:32/native, N7:32/native, N8:32/native>>
    end;

convert(T) when T =:= long; T =:= ulong ->
    fun(Num) -> <<Num:64/native>> end;
convert(T) when T =:= vec2l; T =:= vec2ul ->
    fun({N1, N2}) -> <<N1:64/native, N2:64/native>>;
       ([N1, N2]) -> <<N1:64/native, N2:64/native>>
    end;
convert(T) when T =:= vec3l; T =:= vec3ul ->
    fun({N1, N2, N3}) -> <<N1:64/native, N2:64/native, N3:64/native>>;
       ([N1, N2, N3]) -> <<N1:64/native, N2:64/native, N3:64/native>>
    end;
convert(T) when T =:= vec4l; T =:= vec4ul ->
    fun({N1, N2, N3, N4}) -> <<N1:64/native, N2:64/native, N3:64/native, N4:64/native>>;
       ([N1, N2, N3, N4]) -> <<N1:64/native, N2:64/native, N3:64/native, N4:64/native>>
    end;

convert(T) when T =:= half ->
    fun(Num) -> <<Num:16/float-native>> end;
convert(T) when T =:= vec2h ->
    fun({N1, N2}) -> <<N1:16/float-native, N2:16/float-native>>;
       ([N1, N2]) -> <<N1:16/float-native, N2:16/float-native>>
    end;
convert(T) when T =:= vec3h ->
    fun({N1, N2, N3}) -> <<N1:16/float-native, N2:16/float-native, N3:16/float-native>>;
       ([N1, N2, N3]) -> <<N1:16/float-native, N2:16/float-native, N3:16/float-native>>
    end;
convert(T) when T =:= vec4h ->
    fun({N1, N2, N3, N4}) -> <<N1:16/float-native, N2:16/float-native, N3:16/float-native, N4:16/float-native>>;
       ([N1, N2, N3, N4]) -> <<N1:16/float-native, N2:16/float-native, N3:16/float-native, N4:16/float-native>>
    end;

convert(T) when T =:= float ->
    fun(Num) -> <<Num:32/float-native>> end;
convert(T) when T =:= vec2f; T =:= box1f ->
    fun({N1, N2}) -> <<N1:32/float-native, N2:32/float-native>>;
       ([N1, N2]) -> <<N1:32/float-native, N2:32/float-native>>
    end;
convert(T) when T =:= vec3f ->
    fun({N1, N2, N3}) -> <<N1:32/float-native, N2:32/float-native, N3:32/float-native>>;
       ([N1, N2, N3]) -> <<N1:32/float-native, N2:32/float-native, N3:32/float-native>>
    end;
convert(T) when T =:= vec4f; T =:= box2f; T =:= linear2f; T =:= quatf ->
    fun({N1, N2, N3, N4}) -> <<N1:32/float-native, N2:32/float-native, N3:32/float-native, N4:32/float-native>>;
       ([N1, N2, N3, N4]) -> <<N1:32/float-native, N2:32/float-native, N3:32/float-native, N4:32/float-native>>
    end;

convert(T) when T =:= box3f; T =:= affine2f ->
    fun({N1, N2, N3, N4, N5, N6}) ->
            <<N1:32/float-native, N2:32/float-native,
              N3:32/float-native, N4:32/float-native,
              N5:32/float-native, N6:32/float-native
            >>;
       ([N1, N2, N3, N4, N5, N6]) ->
            <<N1:32/float-native, N2:32/float-native,
              N3:32/float-native, N4:32/float-native,
              N5:32/float-native, N6:32/float-native>>
    end;
convert(T) when T =:= box4f ->
    fun({N1, N2, N3, N4, N5, N6, N7, N8}) ->
            <<N1:32/float-native, N2:32/float-native,
              N3:32/float-native, N4:32/float-native,
              N5:32/float-native, N6:32/float-native,
              N7:32/float-native, N8:32/float-native>>;
       ([N1, N2, N3, N4, N5, N6, N7, N8]) ->
            <<N1:32/float-native, N2:32/float-native,
              N3:32/float-native, N4:32/float-native,
              N5:32/float-native, N6:32/float-native,
              N7:32/float-native, N8:32/float-native>>
    end;
convert(T) when T =:= linear3f ->
    fun({N1, N2, N3, N4, N5, N6, N7, N8, N9}) ->
            <<N1:32/float-native, N2:32/float-native,
              N3:32/float-native, N4:32/float-native,
              N5:32/float-native, N6:32/float-native,
              N7:32/float-native, N8:32/float-native,
              N9:32/float-native>>;
       ([N1, N2, N3, N4, N5, N6, N7, N8, N9]) ->
            <<N1:32/float-native, N2:32/float-native,
              N3:32/float-native, N4:32/float-native,
              N5:32/float-native, N6:32/float-native,
              N7:32/float-native, N8:32/float-native,
              N9:32/float-native>>
    end;
convert(T) when T =:= affine3f ->
    fun({N1, N2, N3, N4, N5, N6, N7, N8, N9, NA, NB, NC}) ->
            <<N1:32/float-native, N2:32/float-native,
              N3:32/float-native, N4:32/float-native,
              N5:32/float-native, N6:32/float-native,
              N7:32/float-native, N8:32/float-native,
              N9:32/float-native, NA:32/float-native,
              NB:32/float-native, NC:32/float-native>>;
       ([N1, N2, N3, N4, N5, N6, N7, N8, N9, NA, NB, NC]) ->
            <<N1:32/float-native, N2:32/float-native,
              N3:32/float-native, N4:32/float-native,
              N5:32/float-native, N6:32/float-native,
              N7:32/float-native, N8:32/float-native,
              N9:32/float-native, NA:32/float-native,
              NB:32/float-native, NC:32/float-native>>
    end;
convert(T) when T =:= double ->
    fun(Num) -> <<Num:64/float-native>> end;
convert(T) when T =:= vec2d ->
    fun({N1, N2}) -> <<N1:64/float-native, N2:64/float-native>>;
       ([N1, N2]) -> <<N1:64/float-native, N2:64/float-native>>
    end;
convert(T) when T =:= vec3d ->
    fun({N1, N2, N3}) -> <<N1:64/float-native, N2:64/float-native, N3:64/float-native>>;
       ([N1, N2, N3]) -> <<N1:64/float-native, N2:64/float-native, N3:64/float-native>>
    end;
convert(T) when T =:= vec4d ->
    fun({N1, N2, N3, N4}) -> <<N1:64/float-native, N2:64/float-native, N3:64/float-native, N4:64/float-native>>;
       ([N1, N2, N3, N4]) -> <<N1:64/float-native, N2:64/float-native, N3:64/float-native, N4:64/float-native>>
    end;
convert(Type) ->
    error({unknown_conversion, Type}).

is_object(Type) ->
    case Type of %% Assert the reference have an object data type
        device -> true;
        object -> true;
        data -> true;
        camera -> true;
        framebuffer -> true;
        future -> true;
        geometric_model -> true;
        geometry -> true;
        group -> true;
        image_operation -> true;
        instance -> true;
        light -> true;
        material -> true;
        renderer -> true;
        texture -> true;
        transfer_function -> true;
        volume -> true;
        volumetric_model -> true;
        world -> true;
        _ -> false
    end.

init_impl(Options) ->
    Mods0 = case os:getenv("OSPRAY_LOAD_MODULES") of
                false -> [];
                ModStr -> string:lexemes(ModStr, ",")
            end,
    Mods = Mods0 ++ proplists:get_value(loadModules, Options, []),
    [no_error = loadModule(Mod) || Mod <- Mods],
    Dev = case proplists:get_value(device, Options, "default") of
              "default" ->
                  no_error = loadModule("ispc"),
                  newDevice("cpu");
              DevStr ->
                  newDevice(DevStr)
          end,
    [set_init_param(Dev, Param, proplists:get_value(Param, Options, false)) ||
        Param <- [numThreads, setAffinity, logLevel, logOutput, errorOutput, debug, warnAsError]],
    deviceCommit(Dev),
    setCurrentDevice(Dev),
    case deviceGetLastErrorCode(Dev) of
        no_error -> Dev;
        _ -> error({error, deviceGetLastErrorMsg(Dev)})
    end.

set_init_param(_, _, false) -> ok;
set_init_param(Dev, logLevel=Id, Int) when is_integer(Int) ->
    deviceSetParam(Dev, Id, int, Int);
set_init_param(Dev, logOutput=Id, Atom) when Atom =:= cerr; Atom =:= cout ->
    deviceSetParam(Dev, Id, string, Atom);
set_init_param(Dev, errorOutput=Id, Atom) when Atom =:= cerr; Atom =:= cout ->
    deviceSetParam(Dev, Id, string, Atom);
set_init_param(Dev, debug=Id, Bool) when is_boolean(Bool) ->
    deviceSetParam(Dev, Id, bool, Bool);
set_init_param(Dev, setAffinity=Id, Bool) when is_boolean(Bool) ->
    deviceSetParam(Dev, Id, bool, Bool);
set_init_param(Dev, numThreads=Id, Int) when is_integer(Int) ->
    deviceSetParam(Dev, Id, int, Int);
set_init_param(_, Id, _) ->
    error({unknown_param, Id}).

%% Nif init

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

nif_init() ->
    Name = case os:type() of
               {win32, _} -> "osp";
               _ -> "libosp"
           end,
    Dir = case code:priv_dir(ospraye) of
              {error, _} ->
                  MPath = code:which(?MODULE),
                  Path  = filename:dirname(filename:dirname(MPath)),
                  filename:join([Path, "priv"]);
              Priv -> Priv
          end,
    Nif = filename:join(Dir, Name),
    %% io:format("Loading ~p exists ~p ~n",[Nif, filelib:is_file(Nif ++ ".dll")]),
    erlang:load_nif(Nif, 0).
