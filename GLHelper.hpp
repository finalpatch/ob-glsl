#pragma once

#include <glbinding/Binding.h>
#include <glbinding/gl33core/gl.h>
#include <vector>

using namespace gl33core;

class Shader {
    GLuint _handle = 0;
public:
    Shader(GLenum type, const std::string& src) {
        _handle = glCreateShader(type);
        const char* srcList[] = {src.c_str()};
        glShaderSource(_handle, 1, srcList, nullptr);
        glCompileShader(_handle);
        GLint compilationOk;
        glGetShaderiv(_handle, GL_COMPILE_STATUS, &compilationOk);
        if (!compilationOk)
        {
            GLint errLength;
            glGetShaderiv(_handle, GL_INFO_LOG_LENGTH, &errLength);
            std::vector<char> errLog(errLength+1, '\0');
            glGetShaderInfoLog(_handle, errLength, &errLength, errLog.data());
            throw std::runtime_error("shader compilation failed" + errLog);
        }
    }
    Shader(Shader&& other) noexcept {
        _handle = other._handle;
        other._handle = 0;
    }
    virtual ~Shader() {
        if (_handle) {
            glDeleteShader(_handle);
        }
    }
    GLuint handle() const {return _handle;}
};

class RenderProgram
{
    GLuint _handle = 0;
public:
    RenderProgram(const std::vector<Shader>& shaders)
    {
        _handle = glCreateProgram();
        for(const auto& shader: shaders) {
            glAttachShader(_handle, shader.handle());
        }
        glLinkProgram(_handle);
        GLint isLinked = 0;
        glGetProgramiv(_handle, GL_LINK_STATUS, &isLinked);
        if(!isLinked) {
            auto errCode = glGetError();
            GLint maxLength;
            glGetProgramiv(_handle, GL_INFO_LOG_LENGTH, &maxLength);
            std::vector<GLchar> infoLog(maxLength);
            glGetProgramInfoLog(_handle, maxLength, &maxLength, &infoLog[0]);
            throw std::runtime_error(std::string("failed to link program: ") +
                                     std::to_string(static_cast<int>(errCode)) +
                                     reinterpret_cast<const char*>(infoLog.data()));
        }
    }
    RenderProgram(RenderProgram&& other) noexcept {
        _handle = other._handle;
        other._handle = 0;
    }
    virtual ~RenderProgram()
    {
        if (_handle)
            glDeleteProgram(_handle);
    }
    void use() const
    {
        glUseProgram(_handle);
    }
    GLuint getUniformLocation(const char* name) const
    {
        return glGetUniformLocation(_handle, name);
    }
};

class VertexArray
{
    GLuint _handle = 0;
public:
    VertexArray() {
        glGenVertexArrays(1, &_handle);
    }
    VertexArray(VertexArray&& other) noexcept {
        _handle = other._handle;
        other._handle = 0;
    }
    virtual ~VertexArray() {
        if (_handle) {
            glDeleteVertexArrays(1, &_handle);
        }
    }
    void bind() {
        glBindVertexArray(_handle);
    }
};
