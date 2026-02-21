cmake_minimum_required(VERSION 3.14)
project(cui CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Use a local folder for dependencies so they persist across builds
set(FETCHCONTENT_BASE_DIR "${CMAKE_SOURCE_DIR}/deps")
include(FetchContent)
FetchContent_Declare(
    imgui
    GIT_REPOSITORY https://github.com/ocornut/imgui.git
    GIT_TAG        master
    GIT_SHALLOW    TRUE
)
FetchContent_MakeAvailable(imgui)

find_package(SDL2 REQUIRED)

add_executable(${PROJECT_NAME}
    src/main.cpp
    src/ui.cpp

    # Imgui sources
    ${imgui_SOURCE_DIR}/imgui.cpp
    ${imgui_SOURCE_DIR}/imgui_draw.cpp
    ${imgui_SOURCE_DIR}/imgui_widgets.cpp
    ${imgui_SOURCE_DIR}/imgui_tables.cpp

    # Imgui renderer
    ${imgui_SOURCE_DIR}/backends/imgui_impl_sdl2.cpp
    ${imgui_SOURCE_DIR}/backends/imgui_impl_sdlrenderer2.cpp
)

target_include_directories(${PROJECT_NAME} PRIVATE
  ${imgui_SOURCE_DIR}
  ${imgui_SOURCE_DIR}/backends
)

target_link_libraries(${PROJECT_NAME} PRIVATE
  SDL2::SDL2
  GL
  dl
  pthread
)
