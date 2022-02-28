%%
%%   API testing for ospraye
%%
%%   Copyright Dan Gudmundsson
%%

-module(osp_tutorial_SUITE).

-compile(export_all).

-include_lib("wx/include/wx.hrl").

all() ->
    [
     tutorial
    ].

vertex_pos_1() ->
    [-1.0,-1.0,3.0, -1.0,1.0,3.0,
      1.0,-1.0,3.0,  1.0,1.0,2.0].
uv_1() ->
    [{0.0,0.0},  {0.0,0.95},
     {0.95,0.0}, {0.95,0.95}].
colors_1() ->
    [0.9,0.5,0.5,1.0,   0.5,0.5,0.9,1.0,
     0.0,0.0,0.0,1.0,   0.5,0.9,0.5,1.0].

index_1() ->
    [0,2,1, 1,2,3].

image_1() ->
    R = {255,0,0}, B={0,0,255},
    [R,R,R,R,
     R,R,R,R,
     B,B,B,B,
     B,B,B,B].

-define(check_error(Dev),
        fun() ->
                case osp:deviceGetLastErrorCode(Dev) of
                    no_error -> ok;
                    _ -> io:format("~p:~p: ~p~n",[?MODULE, ?LINE, osp:deviceGetLastErrorMsg(Dev)])
                end
        end()).

tutorial(Config) ->
    Dev = case lists:member(manual_config, Config) of
              true ->
                  no_error = osp:loadModule("ispc"),
                  Dev0 = osp:newDevice(cpu),
                  osp:deviceSetParam(Dev0, "setAffinity", bool, false),
                  osp:deviceSetParam(Dev0, numThreads, int, 16),
                  ok = osp:deviceCommit(Dev0),
                  ok = osp:setCurrentDevice(Dev0),
                  Dev0;
              false ->
                  osp:init()
          end,
    ?check_error(Dev),
    %% osp:deviceSetParam(Dev, debug, bool, true), osp:deviceCommit(Dev),

    Cam_pos  = {0.0, 0, 0},
    Cam_up   = {0.0, 1, 0},
    Cam_view = {0.1, 0, 1},

    {W,H} = Sz = {800,600},
    Camera = osp:newCamera(perspective),
    osp:setFloat(Camera, "aspect", W / H),
    osp:setParam(Camera, "position", vec3f, Cam_pos),
    osp:setParam(Camera, "direction", vec3f, Cam_view),
    osp:setParam(Camera, "up", vec3f, Cam_up),
    osp:commit(Camera), %%  commit each object to indicate modifications are done

    ?check_error(Dev),
    Mesh = osp:newGeometry("mesh"),
    VsData = osp:newCopiedData(vertex_pos_1(), vec3f, 4),
    osp:commit(VsData),
    osp:setObject(Mesh, "vertex.position", VsData),
    case proplists:get_value(use_colors, Config, false) of
        true ->
            io:format("Use vertex colors~n",[]),
            Data1 = osp:newCopiedData(colors_1(), vec4f, 4),
            osp:commit(Data1),
            osp:setObject(Mesh, "vertex.color", Data1);
        false ->
            io:format("Use uv coords~n",[]),
            UvData = osp:newCopiedData(uv_1(), vec2f, 4),
            osp:commit(UvData),
            osp:setObject(Mesh, "vertex.texcoord", UvData)
    end,
    ?check_error(Dev),

    IxData = osp:newCopiedData(index_1(), vec3ui, 2),
    osp:commit(IxData),
    osp:setObject(Mesh, "index", IxData),
    osp:commit(Mesh),
    ?check_error(Dev),

    TxData = osp:newCopiedData(image_1(), vec3uc, 4, 0, 4),
    osp:commit(TxData),
    Texture = osp:newTexture("texture2d"),
    osp:setParam(Texture, "format", int, texture_rgb8),
    osp:setObject(Texture, "data", TxData),
    osp:commit(Texture),
    ?check_error(Dev),

    Mat = osp:newMaterial("principled"),
    %% osp:setParam(Mat, "map_kd", texture, Texture),
    case proplists:get_value(use_colors, Config, false) of
        false ->
            osp:setParam(Mat, "map_baseColor", texture, Texture);
        true ->
            io:format("Using vertex colors, ignoring textures~n",[]),
            osp:removeParam(Mat, "map_baseColor"),
            ignore
    end,
    osp:commit(Mat),
    ?check_error(Dev),

    %% put the mesh into a model
    Model = osp:newGeometricModel(Mesh),
    osp:setObject(Model, "material", Mat),
    osp:commit(Model),
    ?check_error(Dev),

    %% put the model into a group (collection of models)
    Group = osp:newGroup(),
    %% osp:setObjectAsData(group, "geometry", OSP_GEOMETRIC_MODEL, model),
    ModelList = osp:newCopiedData([Model], geometric_model, 1),
    osp:setParam(Group, "geometry", geometric_model, ModelList),

    %%osp:setParam(Group, "geometry", geometric_model, osp:newCopiedData([Model], model, 1)),
    osp:commit(Group),

    %% put the group into an instance (give the group a world transform)
    Instance = osp:newInstance(Group),
    osp:commit(Instance),

    %% put the instance in the world
    World = osp:newWorld(),
    %% osp:SetObjectAsData(world, "instance", OSP_INSTANCE, instance),
    osp:setParam(World, "instance", instance, osp:newCopiedData([Instance], instance, 1)),

    %% create and setup light for Ambient Occlusion
    Light = osp:newLight("ambient"),
    osp:commit(Light),
    osp:setParam(World, "light", light, osp:newCopiedData([Light], light, 1)),
    %% osp:SetObjectAsData(world, "light", OSP_LIGHT, light),
    osp:commit(World),

    io:format("World bounds ~p~n",[osp:getBounds(World)]),
    io:format("setting up renderer..."),

    %% create renderer
    Renderer = osp:newRenderer("pathtracer"), %% choose path tracing renderer
    %% complete setup of renderer
    osp:setParam(Renderer, "backgroundColor", vec4f, {0,0,0,1.0}),
    osp:commit(Renderer),

    %% create and setup framebuffer
    Framebuffer = osp:newFrameBuffer(W,H, fb_srgba, [fb_color, fb_accum]),
    osp:resetAccumulation(Framebuffer),

    io:format("rendering initial frame to firstFrame.ppm..."),
    %% render one frame
    Future1 = osp:renderFrame(Framebuffer, Renderer, Camera, World),
    osp:wait(Future1),

    %% access framebuffer and write its content as PPM file
    FirstImage = osp:readFrameBuffer(Framebuffer, W,H, fb_srgba, fb_color),
    %% const uint32_t *fb = (uint32_t *)osp:MapFrameBuffer(framebuffer, OSP_FB_COLOR),
    %% writePPM("firstFrame.ppm", imgSize_x, imgSize_y, fb),
    %% osp:UnmapFrameBuffer(fb, framebuffer),

    display("Tutorial N=1", Sz, FirstImage),

    io:format("done\n"),
    io:format("rendering 10 accumulated frames..."),

    %% render 10 more frames, which are accumulated to result in a better
    %% converged image

    Render = fun() ->
                     Future = osp:renderFrame(Framebuffer, Renderer, Camera, World),
                     osp:wait(Future)
             end,
    [Render() || _ <- lists:seq(1,10)],
    Image10 = osp:readFrameBuffer(Framebuffer, W,H, fb_srgba, fb_color),

    display("Tutorial N=10", Sz, Image10),
    io:format("done\n\n"),

    %% OSPPickResult p,
    %% osp:Pick(&p, framebuffer, renderer, camera, world, 0.5f, 0.5f),

    %% io:format("ospPick() center of screen --> [inst: %p, model: %p, prim: %u]\n",
    %%   p.instance,
    %%   p.model,
    %%   p.primID),

    io:format("cleaning up objects..."),
    io:format("done\n"),

    timer:sleep(5000),
    %% osp:Shutdown(),
    ok.


display(Title, {IW,IH}=_Size, RGBABin0) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title, [{size, {IW+50, IH+50}}]),
    Panel = wxPanel:new(Frame),
    wxWindow:setBackgroundColour(Panel, {200, 180, 180}),
    Szr = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:addStretchSpacer(Szr),
    %% Sigh wxWidgets splits rgb and alpha (and is upside down)
    RowSz = 4*IW,
    RGBABin = iolist_to_binary(lists:reverse([ Row || <<Row:RowSz/binary>> <= RGBABin0])),

    RGB = << <<RGB:24>> || <<RGB:24,_:8>> <= RGBABin >>,
    Alpha = << <<A:8>> || <<_:24, A:8>> <= RGBABin >>,
    Image = wxImage:new(IW,IH, RGB, Alpha),
    BMImage = wxBitmap:new(Image),
    SBM = wxStaticBitmap:new(Panel, ?wxID_ANY, BMImage),
    wxBitmap:destroy(BMImage),
    wxImage:destroy(Image),
    wxSizer:add(Szr, SBM, [{flag, ?wxALIGN_CENTER}]),
    wxSizer:addStretchSpacer(Szr),
    wxPanel:setSizer(Panel, Szr),
    wxFrame:show(Frame),
    #{frame => Frame, panel => Panel}.
