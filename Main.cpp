#include "EmacsModule.hpp"
#include "Window.hpp"
#include "GLHelper.hpp"
#include <SDL2/SDL_image.h>

static const std::string vs = R"(
#version 330 core
const vec4 vertice[4] = vec4[4] (
    vec4(-1.0,  1.0, 0.0, 1.0),vec4(-1.0, -1.0, 0.0, 1.0),
    vec4( 1.0, -1.0, 0.0, 1.0),vec4( 1.0,  1.0, 0.0, 1.0)
);
void main() {
    gl_Position = vertice[gl_VertexID];
})";

static const std::string ps = R"(
#version 330 core
out vec4 fragColor;
uniform vec2 iResolution;
const float esc = 20.0;
const int depth = 420;
const float p = 30.0;
vec3 mandel(vec2 z0) {
    float k = 0.0;
    vec2 z = vec2(0.0);
    for(int i = 0; i < depth; ++i) {
        z = vec2(z.x*z.x-z.y*z.y, z.x*z.y*2.0) + z0;
        if (length(z) > esc)
            break;
        k += 1.0;
    }
    float mu = k + 1.0 - log2(log(length(z)));
    return sin(mu*0.1 + vec3(0.0,0.5,1.0));
}
void main() {
    float ar = iResolution.x / iResolution.y;
    vec2 uv = gl_FragCoord.xy / iResolution.yy - vec2(0.66 * ar, 0.5);
    float scale = 0.5;
    vec2 offset = vec2(-0.3, 0.0);
    uv += offset*scale;
    uv /= scale;
    fragColor = vec4(mandel(uv), 1.0);
})";

emacs_value obGlslRun (emacs_env* env,
                       const std::string& shaderCode,
                       int width,
                       int height,
                       const std::string& outputPath) {
    FrameBuffer fb;
    RenderBuffer rb;
    rb.bind();
    glRenderbufferStorage(GL_RENDERBUFFER, GL_RGBA8, width, height);
    fb.bind();
    fb.renderBuffer(rb);
    if (!fb.check()) 
        throw std::runtime_error("framebuffer is not complete!");
    
    // prepare shader
    std::vector<Shader> shaders;
    shaders.emplace_back(GL_VERTEX_SHADER, vs);
    shaders.emplace_back(GL_FRAGMENT_SHADER, shaderCode.empty() ? ps : shaderCode);
    RenderProgram prog(shaders);

    // draw
    VertexArray vao;
    glViewport(0, 0, width, height);
    glClearColor(1.0, 1.0, 0.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);
    vao.bind();
    prog.use();
    auto res = prog.getUniformLocation("iResolution");
    glUniform2f(res, width, height);
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

    // read back
    std::vector<uint8_t> pixels(width * 4 * height);
    glReadBuffer(GL_COLOR_ATTACHMENT0);
    glReadPixels(0,0,width,height,GL_RGBA,GL_UNSIGNED_BYTE,pixels.data());

    // Flip the pixel data vertically
    std::vector<uint8_t> flippedPixels(width * 4 * height);
    for (int y = 0; y < height; ++y) {
        memcpy(flippedPixels.data() + y * width * 4,
			   pixels.data() + (height - 1 - y) * width * 4,
			   width * 4);
    }

    auto surface = SDL_CreateRGBSurfaceFrom(flippedPixels.data(), width, height,
                                            32/*depth*/, width * 4/*pitch*/,
                                            0x000000ff, 0x0000ff00, 0x00ff0000, 0xff000000);
    IMG_SavePNG(surface, outputPath.data());
    SDL_FreeSurface(surface);
    
    auto estr = env->make_string(env, outputPath.data(), outputPath.size());
    return estr;
};

int emacs_module_init (emacs_runtime *ert) noexcept {
    emacs_env* env = ert->get_environment (ert);
    try {
        init();
        glbinding::Binding::initialize(false);
    } catch (std::exception& e) {
        reportError(env, e);
    }

    bindFunction(env, obGlslRun, "ob-glsl-run");
    provide(env, "ob-glsl-module");
    return 0;
}
