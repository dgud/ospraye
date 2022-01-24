%%
%%   Erlang Wrapper library for OSPray
%%
%%   Copyright Dan Gudmundsson
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
         geometricModel/0, geometricModel/1,
         getBounds/1,
         getCurrentDevice/0,
         getProgress/1,
         getTaskDuration/1,
         getVariance/1,
         isReady/1, isReady/2,
         loadModule/1,
         mapFrameBuffer/1, mapFrameBuffer/2,
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
         newSharedData/3, newSharedData/4, newSharedData/5, newSharedData/6, newSharedData/7, newSharedData/8,
         newTexture/1,
         newTransferFunction/1,
         newVolume/1,
         newVolumetricModel/0, newVolumetricModel/1,
         newWorld/0,
         release/1,
         removeParam/2,
         renderFrame/4,
         resetAccumulation/1,
         retain/1,
         setCurrentDevice/1,
         setParam/4,
         shutdown/0,
         wait/1, wait/2
        ]).

-include("osp_types.hrl").

-define(nif_stub,nif_stub_error(?LINE)).
-on_load(init/0).

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

%% Device Initialization

%% returns the OSPRay Version in use by the device
-spec deviceGetProperty(Dev::device(), deviceProperty()) -> integer().
deviceGetProperty(_Dev, _DeviceProperty) -> ?nif_stub.

%% Shutdown the Ray engine...effectively deletes whatever device is currently
%% set.

-spec shutdown() -> ok.
shutdown() -> ?nif_stub.

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

-spec deviceSetParam(Dev::device(), Id::binary(), Type::dataType(), Mem::binary()) -> ok.
deviceSetParam(Device, Id, Type, Mem) ->
    deviceSetParam_nif(Device, id_to_string(Id), Type, data_to_binary(Type, Mem)).
deviceSetParam_nif(_Device, _Id, _DataType, _Mem) -> ?nif_stub.

-spec deviceRemoveParam(Dev::device(), Id::binary) -> ok.
deviceRemoveParam(_Device, _Id) -> ?nif_stub.

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

-spec newSharedData(SharedData :: binary(), Type :: dataType(), NumItems1 ::integer()) ->  data().
newSharedData(SharedData, Type, NumItems1) ->
    newSharedData(SharedData, Type, NumItems1,0,  1,0, 1,0).

-spec newSharedData(SharedData :: binary(), Type :: dataType(),
                    NumItems1 ::integer(), ByteStride1 :: integer()) -> data().
newSharedData(SharedData, Type, NumItems1, ByteStride1) ->
    newSharedData(SharedData, Type, NumItems1,ByteStride1,  1,0, 1,0).

-spec newSharedData(SharedData :: binary(), Type :: dataType(),
                    NumItems1 ::integer(), ByteStride1 :: integer(),
                    NumItems2 ::integer()) -> data().
newSharedData(SharedData, Type, NumItems1, ByteStride1, NumItems2) ->
    newSharedData(SharedData, Type, NumItems1,ByteStride1,  NumItems2,0, 1,0).

-spec newSharedData(SharedData :: binary(), Type :: dataType(),
                    NumItems1 ::integer(), ByteStride1 :: integer(),
                    NumItems2 ::integer(), ByteStride2 :: integer()) -> data().
newSharedData(SharedData, Type, NumItems1, ByteStride1, NumItems2, ByteStride2) ->
    newSharedData(SharedData, Type, NumItems1,ByteStride1,  NumItems2,ByteStride2, 1,0).

-spec newSharedData(SharedData :: binary(), Type :: dataType(),
                    NumItems1 ::integer(), ByteStride1 :: integer(),
                    NumItems2 ::integer(), ByteStride2 :: integer(),
                    NumItems3 ::integer()) -> data().
newSharedData(SharedData, Type, NumItems1, ByteStride1, NumItems2, ByteStride2, NumItems3) ->
    newSharedData(SharedData, Type, NumItems1,ByteStride1,  NumItems2, ByteStride2, NumItems3,0).

-spec newSharedData(SharedData :: binary(), Type :: dataType(),
                    NumItems1 ::integer(), ByteStride1 :: integer(),
                    NumItems2 ::integer(), ByteStride2 :: integer(),
                    NumItems3 ::integer(), ByteStride3 :: integer()
                   ) ->  data().
newSharedData(_SharedData, _Type, _NumItems1, _ByteStride1, _NumItems2, _ByteStride2, _NumItems3, _ByteStride3) -> ?nif_stub.

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

-spec newLight(Type::binary()) -> light().
newLight(_Type) -> nif_stub.

-spec newCamera(Type::binary()) -> camera().
newCamera(_Type) -> nif_stub.

-spec newGeometry(Type::binary()) -> geometry().
newGeometry(_Type) -> nif_stub.

-spec newVolume(Type::binary()) -> volume().
newVolume(_Type) -> nif_stub.

-spec geometricModel() -> geometricModel().
geometricModel() -> geometricModel(null).
-spec geometricModel(Geom::geometry()) -> geometricModel().
geometricModel(_Geom) -> ?nif_stub.

-spec newVolumetricModel() -> volumetricModel().
newVolumetricModel() -> newVolumetricModel(null).
-spec newVolumetricModel(Volume::volume()) -> volumetricModel().
newVolumetricModel(_Volume) -> ?nif_stub.

% Model Meta-Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec newMaterial(MaterialType::binary()) ->  material().
newMaterial(_MaterialType) -> ?nif_stub.

-spec newTransferFunction(Type::binary()) ->  transferFunction().
newTransferFunction(_type) -> ?nif_stub.

-spec newTexture(Type::binary()) ->  texture().
newTexture(_type) -> ?nif_stub.

%% Instancing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

-spec newGroup() -> group().
newGroup() -> ?nif_stub.

-spec newInstance(Group::group()) ->  instance().
newInstance(_Group) -> ?nif_stub.

%% Top-level Worlds %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

-spec newWorld() ->  world().
newWorld() -> ?nif_stub.

%% Return bounds if the object is able (World, Instance, and Group)
-spec getBounds(Object :: object()) ->  {float(), float(), float(), float(), float(), float()}.
getBounds(_Object) -> ?nif_stub.

%% Object + Parameter Lifetime Management %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

%% Set a parameter, where 'mem' points the address of the type specified

-spec setParam(Object::object(), Id::binary(), Type :: dataType(), Mem :: binary()) ->  ok.
setParam(_Object, _id, _Type, _mem) -> ?nif_stub.

-spec removeParam(Object::object(), Id::binary()) ->  ok.
removeParam(_Object, _id) -> ?nif_stub.

%% Make parameters which have been set visible to the object
-spec commit(Object::object()) -> ok.
commit(_Object) -> ?nif_stub.

%% Reduce the application-side object ref count by 1
-spec release(Object::object()) ->  ok.
release(_Object) -> ?nif_stub.

%% Increace the application-side object ref count by 1
-spec retain(Object::object()) ->  ok.
retain(_Object) -> ?nif_stub.

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


-spec newImageOperation(Type::binary()) ->  imageOperation().
newImageOperation(_type) -> ?nif_stub.

%% Pointer access (read-only) to the memory of the given frame buffer channel
-spec mapFrameBuffer(FB::frameBuffer()) ->  binary().
mapFrameBuffer(FB) -> mapFrameBuffer(FB, fb_channel).
-spec mapFrameBuffer(FB::frameBuffer(), FBC::frameBufferChannel()) ->  binary().
mapFrameBuffer(_FrameBuffer, _FrameBufferChannel) -> ?nif_stub.

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

-spec newRenderer(Type::binary()) ->  renderer().
newRenderer(_type) -> ?nif_stub.

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
wait(_Future, _SyncEvent) -> ?nif_stub.

%% Cancel the given task (may block calling thread)
-spec cancel(future()) -> ok.
cancel(_Future) -> ?nif_stub.

%% Get the completion state of the given task [0.f-1.f]
-spec getProgress(future()) ->  float().
getProgress(_Future) -> ?nif_stub.

%% Get the execution duration (in seconds) state of the given task
-spec getTaskDuration(future()) ->  float().
getTaskDuration(_Future) ->
    ?nif_stub.

%% Helpers
%% Strings need a closing end of string
id_to_string(Id) when is_atom(Id) ->
    unicode:characters_to_binary([atom_to_binary(Id)|[0]]);
id_to_string(Id) when is_list(Id); is_binary(Id) ->
    unicode:characters_to_binary([Id|[0]]).

data_to_binary(string, Data) ->
    id_to_string(Data);
data_to_binary(Type, Data) when is_reference(Data) ->
    case Type of  %% Assert the reference have an object data type
        object -> ok;
        data -> ok;
        camera -> ok;
        framebuffer -> ok;
        future -> ok;
        geometric_model -> ok;
        geometry -> ok;
        group -> ok;
        image_operation -> ok;
        instance -> ok;
        light -> ok;
        material -> ok;
        renderer -> ok;
        texture -> ok;
        transfer_function -> ok;
        volume -> ok;
        volumetric_model -> ok;
        world -> ok
    end,
    Data;
data_to_binary(Type, Boolean) when is_boolean(Boolean) ->
    bool = Type,
    case Boolean of
        true  -> <<1:32/native>>;
        false -> <<0:32/native>>
    end;
data_to_binary(_, Data) when is_binary(Data) ->
    Data.

%% Nif init

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    Name = "libosp",
    Dir = case code:priv_dir(ospraye) of
              {error, _} ->
                  MPath = code:which(?MODULE),
                  Path  = filename:dirname(filename:dirname(MPath)),
                  filename:join([Path, "priv"]);
              Priv -> Priv
          end,
    Nif = filename:join(Dir, Name),
    erlang:load_nif(Nif, 0).
