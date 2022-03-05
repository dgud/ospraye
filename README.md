ospraye
=======

A erlang wrapper library for [OSPRAY](https://www.ospray.org)

Intel OSPRay is an **o**pen source, **s**calable, and **p**ortable
**ray** tracing engine for high-performance, high-fidelity visualization
on Intel Architecture CPUs. OSPRay is part of the [Intel oneAPI
Rendering Toolkit](https://software.intel.com/en-us/rendering-framework)
and is released under the permissive [Apache 2.0
license](https://www.apache.org/licenses/LICENSE-2.0).

The purpose of OSPRay is to provide an open, powerful, and easy-to-use
rendering library that allows one to easily build applications that use
ray tracing based rendering for interactive applications (including both
surface- and volume-based visualizations). OSPRay is completely
CPU-based, and runs on anything from laptops, to workstations, to
compute nodes in HPC systems.

The OSPRAY documentation can be found [here: https://www.ospray.org/documentation.html] (https://www.ospray.org/documentation.html)


Requirements
------------

cmake


Build unix
-----------
    $ export CMAKE_PREFIX_PATH = "$CMAKE_PREFIX_PATH:/home/dgud/src/ospray/build/install/ospray"
    $ mkdir build
    $ cd build
    $ cmake ..
    $ cmake --build . --config RelWithDebInfo --target install

Build windows
-------------
    > $Env:Path += ";C:\Program Files\erl-24.2\bin"
    > $Env:CMAKE_PREFIX_PATH += ";e:/src/ospray/win32/install/"
    > mkdir build
    > cd build
    > cmake ..
    > cmake --build . --config RelWithDebInfo --target install

