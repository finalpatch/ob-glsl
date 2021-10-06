#include "Window.hpp"
#include <SDL2/SDL.h>
#include <stdexcept>

static SDL_Window* win = nullptr;
static SDL_GLContext ctx = nullptr;

void init() {
	SDL_Init(SDL_INIT_VIDEO);
    
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE); 

    win = SDL_CreateWindow("hidden", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
        100, 100, SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIDDEN);
    if (!win) throw std::runtime_error("failed to create window");
    ctx = SDL_GL_CreateContext(win);
    if (!ctx) throw std::runtime_error("failed to create gl context");
}
